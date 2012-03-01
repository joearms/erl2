-module(erl2_expt1).
-compile(export_all).

%% Not finished ....
%% Not done funs and clauses and LCs in bodies

test() ->
    epp:parse_file("./erl2_codegen.erl","","").

start() ->
    L = get(),
    Mods = get_mods(L),
    [compile_mod(I, L) || I <- Mods].

get_mods(L) ->
    L1 = lists:sort(elib1_misc:remove_duplicates([M || {{fundef,{M,_,_}},_} <- L])).

compile_mod(Mod, L) ->
    Funcs = get_funcs(Mod, L),
    %% io:format("Mod:~p~n Funcs=~p~n",[Mod, Funcs]),
    [compile_func(I) || I<- Funcs].

get_funcs(Mod, L) ->
    lists:sort(elib1_misc:remove_duplicates(
		 [{F,A,C} || {{fundef,{M,F,A}},C} <- L, M==Mod])).


%%----------------------------------------------------------------------

compile_func({Name,Arity,{Clauses, Bs}}) ->
    %% Step 1 - make a binding list
    Bs1 = [{Var,{value,Val}} || {Var, Val} <- Bs],
    Clauses1 = [rename_top_clauses(I, [Bs1]) || I <- Clauses],
    F = {function,1,Name,Arity,Clauses1},
    S = erl_pp:form(F),
    io:format("~s~n",[S]).

rename_top_clauses({clause, Ln, H, G, B}=C, Bs0) ->
    %% io:format("in clause C=~p~n",[C]),
    %% io:format("env B0=~p~n",[Bs0]),
    put(max_vars, 0),
    {H1, Bs1} = rename_fun_head(H, Bs0),
    %% We can't create new vars in a guard
    G1 = rename_vars(G, Bs1),
    %% io:format("H1 = ~p~n G1=~p~n Stack1=~p~n",[H1,G1,Bs1]),
    %% io:format("rename_body:~p~n",[Bs1]),
    {B1, Bs2} = rename_body(B, Bs1),
    Ret = {clause, Ln, H1, G1, B1},
    %% io:format("Result=~p~n",[Ret]),
    Ret.

rename_fun_head(H, B) ->
    F = fun funvar/4,
    rename_pattern(H, F, [[]] ++ B).

funvar(Var, Ln, {yes,1,[N]}, B0) ->
    {{var,Ln,xx(N)},B0};
funvar(Var, Ln, {yes,K,_}, B0) when K > 1 ->
    io:format("*** shadowed variable Var:~p line:~p~n",[Var,Ln]),
    New = new_var(),
    B1 = add_var(Var,New,B0),
    {{var,Ln,xx(New)},B1};
funvar(Var, Ln, no, B0) ->
    New = new_var(),
    B1 = add_var(Var,New,B0),
    {{var,Ln,xx(New)},B1}.

%% matchvar occurs in Patterns
%%   X = V

matchvar(Var, Ln, {yes,_,[N]}, B0) ->
    {{var,Ln,xx(N)},B0};
matchvar(Var, Ln, {yes,K,{value,V}}, B0) when K > 1 ->
    io:format("*** warning variable Var:~p line:~p replaced with:~p~n",[Var,Ln,V]),
    {value(Ln, V),B0};
matchvar(Var, Ln, no, B0) ->
    New = new_var(),
    B1 = add_var(Var,New,B0),
    {{var,Ln,xx(New)},B1}.

f1() ->
    X = 1,
    F = fun() ->
		X = 1
	end,
    F().

f2() ->
    X = 1,
    F = fun() ->
		X = 2
	end,
    F().
    
rename_pattern({var,Ln,V}, F, B0) ->
    F(V,Ln,find_var(V, B0),B0);
rename_pattern(T, F, B0) when is_tuple(T) ->
    {L1, B1} = rename_pattern(tuple_to_list(T), F, B0),
    {list_to_tuple(L1), B1};
rename_pattern([H|T], F, B0) ->
    {H1, B1} = rename_pattern(H, F, B0),
    {T1, B2} = rename_pattern(T, F, B1),
    {[H1|T1], B2};
rename_pattern(X, F, B) ->
    {X, B}.

value(Ln, Val) ->
    erl_parse:abstract(Val, Ln).

xx(N) ->
    list_to_atom("V" ++ integer_to_list(N)).

rename_body({var,Ln,V}, B0) ->
    case find_var(V, B0) of
	{yes, _, [New]} ->
	    {{var,Ln,xx(New)}, B0};
	{yes, _, {value,Val}} ->
	    {value(Ln, Val), B0};
	no ->
	    exit({undef,var,line,Ln,V})
    end;
rename_body({match,Ln,Pattern,Rhs},B0) ->
    {Pattern1, B1} = rename_pattern(Pattern, fun matchvar/4, B0),
    {Rhs1, B2} = rename_body(Rhs, B1),
    {{match,Ln,Pattern1,Rhs1},B2};
rename_body(T, B) when is_tuple(T) ->
    {L1, B1} = rename_body(tuple_to_list(T), B),
    {list_to_tuple(L1), B1};
rename_body([H|T], B0) ->
    {H1, B1} = rename_body(H, B0),
    {T1, B2} = rename_body(T, B1),
    {[H1|T1], B2};
rename_body(X, B) ->
    {X, B}.


new_var() ->
    N = get(max_vars),
    put(max_vars, N+1),
    N+1.

add_var(Var,N,[H|T]) ->
    [[{Var,[N]}|H]|T].
    
rename_vars({var,Ln,V}, Stack) ->
    case find_var(V, Stack) of
	{yes, [New]} ->
	    {var,Ln,xx(New)};
	no ->
	    exit({badVar,Ln,V})
    end;
rename_vars(T, Map)  when is_tuple(T) ->
    L = [rename_vars(I, Map) || I <- tuple_to_list(T)],
    list_to_tuple(L);
rename_vars([H|T], Map) ->
    [rename_vars(H, Map)|rename_vars(T, Map)];
rename_vars(X, _) ->
    X.

find_var(Var, Stack) ->
    find_var(Var, 1, Stack).

find_var(Var, Level, [HF|TF]) ->
    case lists:keysearch(Var, 1, HF) of
	{value,{_,Val}} -> {yes, Level, Val};
	false           -> find_var(Var, Level+1, TF)
    end;
find_var(_, _, _) ->
    no.

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



	    

