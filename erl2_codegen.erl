-module(erl2_codegen).
-compile(export_all).

test() ->
    epp:parse_file("./erl2_codegen.erl","","").

start() ->
    L = get(),
    Mods = get_mods(L),
    [compile_mod(I, L) || I <- Mods].

get_mods(L) ->
    lists:sort(
      elib1_misc:remove_duplicates(
	[M || {{fundef,{M,_,_}},_} <- L])).

compile_mod(Mod, L) ->
    Funcs = get_funcs(Mod, L),
    [compile_func(I) || I<- Funcs].

get_funcs(Mod, L) ->
    lists:sort(elib1_misc:remove_duplicates(
		 [{F,A,C} || {{fundef,{M,F,A}},C} <- L, M==Mod])).


%%----------------------------------------------------------------------

compile_func({Name,Arity,{Clauses, Bs}}) ->
    io:format("transform ~p/~p~n",[Name,Arity]),
    %% Step 1 - make a binding list
    Bs1 = [Var || {Var,_} <- Bs],
    Clauses1 = [transform_clauses(I, Bs1, Bs) || I <- Clauses],
    F = {function,1,Name,Arity,Clauses1},
    S = erl_pp:form(F),
    io:format("~s~n",[S]).

transform_clauses({clause, Ln, H, G, B}=C, Vars, Bs) ->
    Stack = [varsin(H),Vars],
    Add = import_bindings0(B, Stack),
    %% io:format("Add=~p~n",[Add]),
    case Add of
	[] -> C;
	_  ->
	    M = [make_match(Ln, I, Bs) || I <-Add],
	    B1 = M ++ B,
	    {clause, Ln, H, G, B1}
    end.

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



	    

