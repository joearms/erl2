-module(erl2_codegen).
-compile(export_all).

test() ->
    epp:parse_file("./erl2_codegen.erl","","").

start(L) ->
    elib1_misc:dump("code.all", L),
    Mods = get_mods(L),
    [compile_mod(I, L) || I <- Mods].

get_mods(L) ->
    lists:sort(
      elib1_misc:remove_duplicates(
	[M || {{fundef,{M,_,_}},_} <- L])).

get_locals(L) ->
    [F || {attributes,A} <- L,{attribute,_,local,L1} <- A, F <- L1].

compile_mod(Mod, L) ->
    Exclude = get_locals(L),
    Funcs = get_funcs(Mod, L),
    Funcs2 = [{F,A,B} || {F,A,B} <- Funcs, not lists:member({F,A}, Exclude)],
    put(current_mod, Mod),
    SMod = atom_to_list(Mod),
    File = "gen/" ++ SMod ++ ".erl",
    {ok,Stream} = file:open(File, [write]),
    Attrs = get(attributes),
    Attrs1 = remove_locals(Attrs),
    HeaderStr = [erl_pp:attribute(I) || I <- Attrs1],
    io:format(Stream, "~s~n",[HeaderStr]),
    [compile_func(I, Stream) || I<- Funcs2],
    file:close(Stream),
    io:format("Created:~s~n",[File]).

remove_locals([{attribute,_,local,_}|T]) -> remove_locals(T);
remove_locals([H|T])                     -> [H|remove_locals(T)];
remove_locals([])                        -> [].

get_funcs(Mod, L) ->
    lists:sort(elib1_misc:remove_duplicates(
		 [{F,A,C} || {{fundef,{M,F,A}},C} <- L, M==Mod])).


%%----------------------------------------------------------------------

compile_func({Name,Arity,{Clauses, Bs}}, Stream) ->
    %% io:format("transform ~p/~p~n",[Name,Arity]),
    %% Step 1 - make a binding list
    Bs1 = [Var || {Var,_} <- Bs],
    Clauses1 = [transform_clauses(I, Bs1, Bs) || I <- Clauses],
    F = {function,1,fix_funcname(Name),Arity,Clauses1},
    %% io:format("Form=~p~n",[F]),
    Str = erl_pp:form(F),
    io:format(Stream, "~s~n",[Str]).

transform_clauses({clause, Ln, H, G, B}, Vars, Bs) ->
    Stack = [varsin(H),Vars],
    Add = import_bindings0(B, Stack),
    %% io:format("Add=~p~n",[Add]),
    B1 = xform_body(B),
    case Add of
	[] -> 
	    {clause, Ln, H, G, B1};
	_  ->
	    M = [make_match(Ln, I, Bs) || I <-Add],
	    B2 = M ++ B1,
	    {clause, Ln, H, G, B2}
    end.

xform_body(X) ->
    deep_replace(X, fun fix_99/1).

fix_99({call99,{Mod,Func},Args}) ->
    %% io:format("Mod=~p Func=~p~n",[Mod,Func]),
    Func1 = fix_funcname(Func),
    case get(current_mod) of
	Mod ->
	    {yes, {call,-1,{atom,-1,Func1}, Args}};
	_ ->
	    {yes, {call,-1,{remote, {atom,-1,Mod},{atom,-1,Func1}}, Args}}
    end;
fix_99(_) ->
    no.

fix_funcname({N,A}) ->
    list_to_atom("gen_" ++ i2s(N) ++ "_" ++ atom_to_list(A)); 
fix_funcname(X) ->
    X.

i2s(N) ->
    integer_to_list(N).

deep_replace(X, F) ->
    case F(X) of
	no ->
	    deep_replace1(X, F);
	{yes, New} ->
	    deep_replace(New, F)
    end.

deep_replace1(T, F) when is_tuple(T) ->
    L = tuple_to_list(T),
    L1 = deep_replace(L, F),
    list_to_tuple(L1);
deep_replace1([H|T], F) ->
    [deep_replace(H, F) | deep_replace(T, F)];
deep_replace1(X, _) ->
    X.

make_match(Ln, Var, Bs) ->
    {match,Ln,{var,Ln,Var},value(Ln, Var, Bs)}.

value(Ln, Var, Bs) ->
    {value,{_,Value}} = lists:keysearch(Var,1,Bs),
    erl_parse:abstract(Value, Ln).

import_bindings0(X, B) ->
    Vars = import_bindings(X, B),
    elib1_misc:remove_duplicates(Vars).

%% import_bindings makes a list of the variables
%% that have to be imported

import_bindings({'fun',_Ln,{clauses,C}}, B) ->
    import_clauses(C, B);
import_bindings({var,_,V}, B) ->
    import_var(V, B);
import_bindings(T, B) when is_tuple(T) ->
    import_bindings(tuple_to_list(T), B);
import_bindings([H|T], B) ->
    import_bindings(H, B) ++ import_bindings(T, B);
import_bindings(_, _) ->
    [].

import_clauses([H|T], Stack) ->
    import_clause(H, Stack) ++ import_clauses(T, Stack);
import_clauses([], _) ->
    [].

import_clause({clause,_,Head,_G,Body}, Stack) ->
    import_bindings(Body, [varsin(Head)|Stack]).

import_var(Var, [Last]) -> 
    case lists:member(Var, Last) of
	true -> [Var];
	false -> []
    end;
import_var(Var, [H|T]) ->
    case lists:member(Var, H) of
	true -> [];
	false -> import_var(Var, T)
    end.
	    
value(Ln, Val) ->
    erl_parse:abstract(Val, Ln).

xx(N) ->
    list_to_atom("V" ++ integer_to_list(N)).

varsin(X) -> varsin(X, []).

varsin({var,_,V}, L) ->
    case lists:member(V, L) of
	true  -> L;
	false -> [V|L]
    end;
varsin(T, L) when is_tuple(T) ->
    varsin(tuple_to_list(T), L);
varsin([H|T], L) ->
    varsin(T, varsin(H, L));
varsin(_, L) ->
    L.



	    

