-module(erl2).
-export([batch/1, dump/0, clone/2, local99/3, local2/4, make_mods/0, run/1]).
-import(lists, [reverse/1, reverse/2]).

%% Copyright Joe Armstrong. 2011 All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% In erl2
 
%%  - no different between escript, the shell and modules
%%  - you run the code before compiling it
%%  - you test the code before compiling it
%%  - advanced metaprogramming
%%  - multiple modules in one file
 
%%  > erl2 foo.erl2
%%  runs an erl2 script - if the program passes all tests
%%  a number of erlang modules will be created.

batch([A]) ->
    F = atom_to_list(A),
    run0(F),
    init:stop().

%% ho ho

run0(F) ->
    put(current_module, shell),
    put(defining_modules, []),
    put(current_function, none),
    run(F).


run(F) ->
    case consult(F) of
	{ok, Exprs} ->
	    dump("exprs.tmp", Exprs),
	    B0 = erl_eval:new_bindings(),
	    case (catch erl_eval:exprs(Exprs, B0, {eval, fun local/3})) of
		{'EXIT', Why} ->
		    io:format("Error:~p~n",[Why]),
		    io:format("Compile failed~n");
		_Other ->
		    dump("all.gen", lists:sort(get())),
		    io:format("Success~n")
	    end;
	error ->
	    io:format("Parse failed~n")
    end.

dump(File, L) ->
    {ok, S} = file:open(File, [write]),
    io:format(S, "~p.~n",[L]),
    file:close(S),
    io:format("Created:~p~n", [File]).

merge_bindings([{K,V}|T], B) ->
    merge_bindings(T, erl_eval:add_binding(K, V, B));
merge_bindings([], B) ->
    B.

local99({M, F}, Args, B) ->
    Arity = length(Args),
    doit(get({fundef, {M, F, Arity}}), F, Arity, Args, B).

local2(Mod, Func, Args, B) ->
    %% io:format("local2:~p~n",[{Mod,Func,Args}]),
    Arity = length(Args),
    doit(get({fundef,{Mod,Func,Arity}}), Func, Arity, Args, B).

local(F, Args, B0) ->
    %% io:format("local:~p~n",[{F,Args}]),
    Arity = length(Args),
    {Mod, Func} = erl_eval:resolve_unqualified_call(F, Arity), 
    %% io:format("calling ~p~n",[{Mod,Func,Arity}]),
    doit(get({fundef,{Mod,Func,Arity}}), F, Arity, Args, B0).

doit(undefined, F, Arity, _Args, _B0) ->
    io:format("** undefined function:~p~n",[{F,Arity}]),
    %% io:format("~p~n",[lists:sort(get())]),
    erlang:error({undefined_function, {F,Arity}});
doit({Clauses,Bd}, F, _Arity, Args, B0) ->
    LF = {eval, fun local/3},
    {Vals, _B1} =  eval1(Args, B0, LF),
    case erl_eval:match_clause(Clauses, Vals, [], LF) of
	{Body, Bs1} ->
	    B2 = merge_bindings(Bs1, Bd),
	    %% io:format("Erl_eval_exprs:: Body=~p ~p~n",[Body, B2]),
	    {value, ZZ, _} = erl_eval:exprs(Body, B2, LF),
	    %% io:format("ZZ=~p~n",[ZZ]),
	    {value, ZZ, B0};
	nomatch ->
	    erlang:error({function_clause,[{local,F,Args}]})
    end.

eval1([H|T], B0, LF) ->
    {value, H1, B1} = erl_eval:expr(H, B0, LF),
    {T1, B2} = eval1(T, B1, LF),
    {[H1|T1], B2};
eval1([], B, _) ->
    {[], B}.

consult(F) ->
    {ok, Bin} = file:read_file(F),
    s(binary_to_list(Bin)).

s(S) ->
    parse_file(S, 1, false, []).

parse_file([], _, true, _) ->
    error;
parse_file([], _, false, L) ->
    {ok, reverse(L)};
parse_file(Str, Ln, Errors, L) ->
    case string2exprs(Str, Ln) of
	{ok, Ln1, Exprs, Rest} ->
	    parse_file(Rest, Ln1, Errors, reverse(Exprs, L));
	{error, Ln1, Rest} ->
	    parse_file(Rest, Ln1, true, L);
	error ->
	    parse_file([], Ln, true, L)
    end.

string2exprs(Str, Ln) ->
    case erl_scan:tokens([], Str, Ln) of
	{done, {ok, Toks, Ln1}, Rest} ->
	    Toks1 = munge_toks(Toks),
	    %% io:format("Toks1=~p~n",[Toks1]),
	    case erl_parse:parse_exprs(Toks1) of
		{ok, Exprs} ->
		    %% io:format("Exprs=~p~n",[Exprs]),
		    {ok, Ln1, Exprs, Rest};
		{error,{Line,Mod,Arg}} ->
		    EStr = io_lib:format("~s",[apply(Mod,format_error,[Arg])]),
		    Msg = lists:flatten(EStr),
		    io:format("~n***PARSE ERROR in line:~w ~s~n", [Line,Msg]),
		    {error, Ln1, Rest}
	    end;
	{more, Cont} ->
	    %% give it an eof
	    case erl_scan:tokens(Cont, eof, Ln) of
		{done, {eof,_}, eof} ->
		    {ok, Ln, [], []};
		{done, _, eof} ->
		    io:format("***SCAN Error DOT WS missing at eof~n"),
		    error
	    end;
	Other ->
	    io:format("~n***SCAN ERROR:~p~n", [Other]),
	    error
    end.
 
%% WHY must I munge spec?? - is the tokeniser wrong?

munge_toks([{atom,N,def}|T])        -> [{def,N}|munge_toks(T)];
munge_toks([{atom,N,defExports}|T]) -> [{defExports,N}|munge_toks(T)];
munge_toks([{atom,N,defMods}|T])    -> [{defMods,N}|munge_toks(T)];
munge_toks([{atom,N,spec}|T])       -> [{spec,N}|munge_toks(T)];
munge_toks([{atom,N,addMod}|T])     -> [{addMod,N}|munge_toks(T)];
munge_toks([{atom,N,beginFunc}|T])  -> [{beginFunc,N}|munge_toks(T)];
munge_toks([{atom,N,deleteFunc}|T]) -> [{deleteFunc,N}|munge_toks(T)];
munge_toks([{atom,N,endFunc}|T])    -> [{endFunc,N}|munge_toks(T)];
munge_toks([H|T])                   -> [H|munge_toks(T)];
munge_toks([])                      -> [].

make_mods() ->
    erl2_codegen:start().

clone(Old, New) ->
    %% Update the list of defining modules
    Mods = get(defining_modules),
    Mods = lists:delete(New, Mods),   %% so it doesn't get their twice
    Mods1 = [New|Mods],
    put(defining_modules, Mods1),
    
    %% io:format("get=~p~n",[get()]),
    Defs = [{{fundef,{I,J,K}}, X} || {{fundef, {I,J,K}}, X} <- get(),
				  I == Old],
    %% io:format("Old=~p~n",[Defs]),
    Defs1 = deep_replace(Defs, Old, New),
    %% io:format("New=~p~n",[Defs1]),  
    [put(K,V) || {K,V} <- Defs1],
    %% io:format("get=~p~n",[get()]),
    true.

deep_replace({call99,{Old,X},Bs}, Old, New) ->
    Bs1 = deep_replace(Bs, Old, New),
    {call99,{New,X}, Bs1};
deep_replace({{fundef,{Old,F,A}},D}, Old, New) ->
    D1 = deep_replace(D, Old, New),
    {{fundef,{New,F,A}},D1};
deep_replace(T, O, N) when is_tuple(T) ->
    list_to_tuple(deep_replace(tuple_to_list(T), O, N));
deep_replace([H|T], O, N) ->
    [deep_replace(H, O, N) | deep_replace(T, O, N)];
deep_replace(X, _, _) ->
    X.

dump() ->
    dump("all.gen", get()).


    


