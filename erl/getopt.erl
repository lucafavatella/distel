%%%----------------------------------------------------------------------
%%% File    : getopt.erl
%%% Author  :  <svg@surnet.ru>
%%% Purpose : Check list of options according to specified format
%%% Created : 17 Sep 2001 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------

-module(getopt).
-vsn('$Id$.').
-author('svg@surnet.ru').

-export([options/2, options/3, value/2, value/3, set/3, delete/2]).
-export([format_error/1]).

-record(opt, {name, value=undefined, check=atom, multi=false}).

value(Name, Opts) ->
    value(Name, Opts, undefined).

value(Name, Opts, Default) ->
    case lists:keysearch(Name, 1, Opts) of
	{value, {_, Val}} when Val =/= undefined ->
	    Val;
	_ ->
	    Default
    end.

set(Key, Value, Opts) ->
    case value(Key, Opts) of
	undefined ->
	    [{Key, Value}|Opts];
	_ ->
	    lists:keyreplace(Key, 1, Opts, {Key, Value})
    end.

delete(Key, Opts) when atom(Key) ->
    delete([Key], Opts);
delete(Keys, Opts) ->
    FilterFun = fun(E) when tuple(E) ->
			lists:member(element(1,E), Keys);
		   (E) ->
			lists:member(E, Keys)
		end,
    [O || O <- Opts, not FilterFun(O)].

options(Opts, OptDefs) ->
    options(Opts, OptDefs, defined).
%%------------------------------------------------------------
%% options(Opts, OptDefs, WhichSelect) -> CheckedOpts
%%        Opts, CheckedOpts = [Opt]
%%        OptDefs = {Key, Default, Guard}
%%        WhichSelect = defined - CheckedOpts include Key only if
%%                                it exists in OptDefs
%%                      defined_strict - throw error if Keys exists
%%                                only in OptDefs
%%                      all - include unspec. in OptDefs to CheckedOpts as is
%%        Opt = {Key, Value}
%%        Default = required | undefined | term()
%%        Guard = list | atom | tuple | integer | float | number | port |
%%                pid | reference | binary | term
%%                GuardFun
%%        GuardFun = function(Value) -> {ok, NewValue} | {error, Reson}
%%        Key = atom
%%        Value = term() | undefined
%%------------------------------------------------------------
options(Opts, OptDefs, WhichSelect) ->
    NormOpt = fun (O={N, M}) when atom(N) -> O;
		  (O) when atom(O) -> {O, O}
	      end,
    NormDef = fun ({Name, Default, Check}) ->
		      #opt{name=Name, value=Default, check=Check};
		  (Def) when record(Def, opt) ->
		      Def
	      end,

    NormOpts = [NormOpt(O) || O <- Opts],
    NormDefs = [NormDef(Def) || Def <- OptDefs],

    IsOptDefined = fun({N, _}) ->
			   lists:any(fun (O=#opt{name=Name}) ->
					     Name == N
				     end, NormDefs)
		   end,
    DefOpts = [check_opt(Def, NormOpts) || Def <- NormDefs],
    OtherOpts = [Opt || Opt <- NormOpts, IsOptDefined(Opt) == false],
    options_select(DefOpts, OtherOpts, WhichSelect).

options_select(DefOpts, OtherOpts=[_|_], defined_strict) ->
    throw(error(invalid_option, {invalid_option, OtherOpts}));
options_select(DefOpts, _, Which) when Which == defined_strict;
                                       Which == defined ->
    DefOpts;
options_select(DefOpts, OtherOpts, Which) when Which == all ->
    DefOpts ++ OtherOpts.

check_opt(Def, Opts) when record(Def, opt) ->
    #opt{name=Name, value=Default, check=Check, multi=Multi} = Def,
    UserVal =
	case [V || {N, V} <- Opts, N == Name] of
	    [V|Vs] ->
		if Multi == true -> [V|Vs];
		   true -> V
		end;
	    [] ->
		undefined
	end,
    {Name, check_value(Def, UserVal)}.

check_value(#opt{name=Name, value=required}, undefined) ->
    throw(error(required, {required, Name}));
check_value(#opt{name=Name, value=Default}, undefined) ->
    Default;
check_value(#opt{name=Name, multi=true, check=Check}, UserVal) ->
    [check_value(Name, UV, Check) || UV <- UserVal];
check_value(#opt{name=Name, check=Check}, UserVal) ->
    check_value(Name, UserVal, Check).

check_value(_, Val, list) when list(Val) ->
    Val;
check_value(_, Val, atom) when atom(Val) ->
    Val;
check_value(_, Val, tuple) when tuple(Val) ->
    Val;
check_value(_, Val, integer) when integer(Val) ->
    Val;
check_value(_, Val, float) when float(Val) ->
    Val;
check_value(_, Val, number) when number(Val) ->
    Val;
check_value(_, Val, port) when port(Val) ->
    Val;
check_value(_, Val, pid) when pid(Val) ->
    Val;
check_value(_, Val, constant) when constant(Val) ->
    Val;
check_value(_, Val, binary) when binary(Val) ->
    Val;
check_value(_, Val, reference) when reference(Val) ->
    Val;
check_value(_, Val, function) when function(Val) ->
    Val;
check_value(_, Val, term) ->
    Val;
check_value(Name, Val, Fun) when function(Fun) ->
    case Fun(Val) of
	{ok, NVal} ->
	    NVal;
	{error, Error} ->
	    throw(error(badarg, {badarg, Name, Error}))
    end;
check_value(_, Val, any) ->
    Val;
check_value(Name, Val, Check) ->
    throw(error(badarg, {badarg, Name, "must be " ++ atom_to_list(Check)})).

error(Info, Desc) ->
    {error, {Info, ?MODULE, Desc}}.

format_error(Info) ->
    [F, A] = error_format(Info),
    lists:flatten(io_lib:format("~p:" ++ F, [?MODULE|A])).

error_format({badarg, Name, Reson}) ->
    ["invalid option ~p value, ~p", [Name, Reson]];
error_format({required, Name}) ->
    ["option ~p is mandatory", [Name]];
error_format(Info) ->
    ["~p", [Info]].
