%%%-------------------------------------------------------------------
%%% File    : distel.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Helper functions to be called from Emacs.
%%%
%%% Created : 18 Mar 2002 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(distel).

-author('luke@bluetail.com').

-include_lib("kernel/include/file.hrl").

-import(lists, [flatten/1, member/2, sort/1, map/2]).

-export([rpc_entry/3, eval_expression/1, find_source/1,
         process_list/0, process_summary/1,
         process_summary_and_trace/2, fprof/3, fprof_analyse/1,
         debug_toggle/2, debug_subscribe/1, debug_add/1,
	 break_toggle/2, break_delete/2, break_add/2, break_restore/1,
	 modules/1, functions/2,
	 free_vars/1, free_vars/2,
	 apropos/1, apropos/2, describe/3, describe/4]).

-export([gl_proxy/1, tracer_init/2, null_gl/0]).

-compile(export_all).

-define(L2B(X), list_to_binary(X)).

%% ----------------------------------------------------------------------
%% RPC entry point, adapting the group_leader protocol.

rpc_entry(M, F, A) ->
    GL = group_leader(),
    Name = gl_name(GL),
    case whereis(Name) of
        undefined ->
            Pid = spawn(?MODULE, gl_proxy, [GL]),
            register(Name, Pid),
            group_leader(Pid, self());
        Pid ->
            group_leader(Pid, self())
    end,
    apply(M,F,A).

gl_name(Pid) ->
    list_to_atom(flatten(io_lib:format("distel_gl_for_~p", [Pid]))).

gl_proxy(GL) ->
    receive
        {io_request, From, ReplyAs, {put_chars, C}} ->
            GL ! {put_chars, C},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {put_chars, M, F, A}} ->
            GL ! {put_chars, flatten(apply(M, F, A))},
            From ! {io_reply, ReplyAs, ok};
        {io_request, From, ReplyAs, {get_until, _, _, _}} ->
            %% Input not supported, yet
            From ! {io_reply, ReplyAs, eof}
    end,
    gl_proxy(GL).

%% ----------------------------------------------------------------------

eval_expression(S) ->
    case parse_expr(S) of
        {ok, Parse} ->
            try_evaluation(Parse);
        {error, {_, erl_parse, Err}} ->
            {error, Err}
    end.

try_evaluation(Parse) ->
    case catch erl_eval:exprs(Parse, []) of
	{value, V, _} ->
	    {ok, flatten(io_lib:format("~p", [V]))};
	{'EXIT', Reason} ->
	    {error, Reason}
    end.

parse_expr(S) ->
    {ok, Scan, _} = erl_scan:string(S),
    erl_parse:parse_exprs(Scan).

find_source(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            case code:is_loaded(Mod) of
                {file, preloaded} ->
                    {error, ?L2B("\"preloaded\"")};
                {file, RelName} ->
		    Name = abs_beamfile_name(RelName),
                    case guess_source_file(Name) of
                        {ok, Fname} ->
                            {ok, Fname};
                        false ->
			    case guess_source_file_from_modinfo(Mod) of
				{ok, Fname} ->
				    {ok, Fname};
				false ->
				    {error, fmt("Can't guess matching "
						"source file from ~p",
						[Name])}
			    end
		    end
	    end;
	{error, nofile} ->
	    {error, fmt("Can't find module '~p' on ~p", [Mod, node()])};
        {error, Why} ->
            {error, fmt("~p", [Why])}
    end.

abs_beamfile_name(RelName) ->
    case file:get_cwd() of
	{ok, Cwd} ->
	    filename:join(Cwd, RelName);
	_ ->
	    RelName
    end.

guess_source_file_from_modinfo(Mod) ->
    case member(Mod, int:interpreted()) of
      true -> {ok, int:file(Mod)};
      false ->
          case get_cwd(Mod) of
              false -> false;
              {ok, CWD} ->
                  Src = filename:join([CWD, to_list(Mod)++".erl"]),
                  case file:read_file_info(Src) of
                      {ok, #file_info{type=regular}} -> {ok, Src};
                      _ -> false
                  end
          end
    end.

get_cwd(Mod) ->
    case [O || {options, O} <- Mod:module_info(compile)] of
      [Opts] -> 
          case [C || {cwd, C} <- Opts] of
              [C] -> {ok, to_list(C)};
              _ -> false
          end;
      _ -> 
          false
    end.

to_list(A) when atom(A) -> atom_to_list(A);
to_list(L) when list(L) -> L.

guess_source_file(Beam) ->
    case regexp:sub(Beam, "\\.beam\$", ".erl") of
        {ok, Src1, _} ->
            case file:read_file_info(Src1) of
                {ok, #file_info{type=regular}} ->
                    {ok, Src1};
                _ ->
                    case regexp:sub(Src1, "/ebin/", "/src/") of
                        {ok, Src2, _} ->
                            case file:read_file_info(Src2) of
                                {ok, #file_info{type=regular}} ->
                                    {ok, Src2};
                                _ ->
                                    false
                            end;
                        _ ->
			    %% This lets us find Distel's own sourcecode
			    %% in the source tree layout
			    case regexp:sub(Src1, "/ebin/", "/erl/") of
				{ok, Src2, _} ->
				    case file:read_file_info(Src2) of
					{ok, #file_info{type=regular}} ->
					    {ok, Src2};
					_ ->
					    false
				    end;
				_ ->
				    false
			    end
		    end
            end;
        _ ->
            false
    end.

%% ----------------------------------------------------------------------
%% Summarise all processes in the system.
%%
%% Returns: {Heading, [{pid(), Row}]}
%% Heading = Row = binary()
process_list() ->
    Heading = iformat("PID/Name", "Initial Call", "Reds", "Msgs"),
    {Heading, [{Pid, iformat_pid(Pid)} || Pid <- processes()]}.

iformat_pid(Pid) ->
    iformat(name(Pid), initial_call(Pid), reductions(Pid), messages(Pid)).

name(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Regname} ->
            atom_to_list(Regname);
        _ ->
            io_lib:format("~p", [Pid])
    end.

initial_call(Pid) ->
    {initial_call, {M, F, A}} = process_info(Pid, initial_call),
    io_lib:format("~s:~s/~p", [M, F, A]).

reductions(Pid) ->
    {reductions, NrReds} = process_info(Pid, reductions),
    integer_to_list(NrReds).

messages(Pid) ->
    {messages, MsgList} = process_info(Pid, messages),
    integer_to_list(length(MsgList)).

iformat(A1, A2, A3, A4) ->
    list_to_binary(io_lib:format("~-21s ~-33s ~12s ~8s~n",
                                 [A1,A2,A3,A4])).

%% ----------------------------------------------------------------------
%% Individual process summary and tracing.

%% Returns: {ok, String} | {error, Rsn}
process_info_item(Pid, Item) ->
    case process_info(Pid, Item) of
	{backtrace, Bin} ->
	    {ok, Bin};
	{Item, Term} ->
	    {ok, fmt("~p~n", [Term])};
	undefined ->
	    case is_process_alive(Pid) of
		true ->
		    {ok, <<"undefined">>};
		false ->
		    {ok, fmt("dead process: ~p", [Pid])}
	    end
    end.

%% Returns: Summary : binary()
process_summary(Pid) ->
    Text = [io_lib:format("~-20w: ~w~n", [Key, Value])
            || {Key, Value} <- [{pid, Pid} | process_info(Pid)]],
    list_to_binary(Text).

%% Returns: Summary : binary()
%%
%% Tracer is sent messages of:
%%    {trace_msg, binary()}
process_summary_and_trace(Tracer, Pid) ->
    case is_process_alive(Pid) of
	true ->
	    spawn_tracer(Tracer, Pid),
	    process_summary(Pid);
	false ->
	    {error, fmt("dead process: ~p", [Pid])}
    end.

spawn_tracer(Tracer, Tracee) ->
    spawn(?MODULE, tracer_init, [Tracer, Tracee]).

tracer_init(Tracer, Tracee) ->
    link(Tracer),
    erlang:trace(Tracee, true, trace_flags()),
    tracer_loop(Tracer, Tracee).

trace_flags() ->
    [send, 'receive', running, procs, garbage_collection, call, return_to].

tracer_loop(Tracer, Tracee) ->
    receive
        Trace when tuple(Trace),
                   element(1, Trace) == trace,
                   element(2, Trace) == Tracee ->
            Msg = tracer_format(Trace),
            Tracer ! {trace_msg, list_to_binary(Msg)}
    end,
    tracer_loop(Tracer, Tracee).

tracer_format(Msg) ->
    %% Let's steal some code reuse..
    pman_buf_utils:textformat(Msg).

%% ----------------------------------------------------------------------
%% Profiling
%% fprof_expr(E) -> {ok, Preamble, Header, Entry}
%% Preamble = binary()
%% Entry = {Tag, MFA, Text, Callers, Callees, Beamfile}
%% Callers = Callees = [Tag]
%% MFA = [Module, Function, Arity] | undefined
%%
%% Entry example,
%%   {'foo:bar/2', "foo:bar/2  10 100 200", ['baz:beer/2'], [], "/foo.beam"}

fprof(Expr) ->
    case parse_expr(Expr) of
        {ok, Parse} ->
            fprof_fun(fun() -> erl_eval:exprs(Parse, []) end);
        {error, Rsn} ->
            {error, Rsn}
    end.

fprof(M,F,A) ->
    fprof_fun(fun() -> apply(M,F,A) end).

fprof_fun(F) ->
    GL = spawn_link(fun null_gl/0),
    group_leader(GL, self()),
    fprof:apply(F, []),
    fprof:profile(),
    fprof:analyse({dest, "/tmp/fprof.analysis"}),
    GL ! die,
    fprof_analyse("/tmp/fprof.analysis").

fprof_analyse(Filename) ->
    {ok, Asys} = file:consult(Filename),
    [_Opts, [Totals], _Proc | Fns] = Asys,
    {ok,
     fprof_preamble(Totals),
     fprof_header(),
     [fprof_entry(Entry) || Entry <- Fns]}.
    
fprof_preamble({totals, Cnt, Acc, Own}) ->
    fmt("Totals: ~p calls, ~.3f real, ~.3f CPU\n\n", [Cnt, Acc, Own]).

fprof_header() ->
    fmt("~sCalls\tACC\tOwn\n", [pad(50, "Function")]).

fprof_entry([{ProcName, Cnt, Acc, Own} | Info]) ->
    %% {process, Name, [Infos]}
    {process, fmt("Process ~s: ~p%", [ProcName, Own]),
     map(fun fprof_process_info/1, Info)};
fprof_entry(F) ->
    {Up, This, Down} = F,
    {Name, _, _, _} = This,
    {tracepoint,
     fprof_tag(Name),
     fprof_mfa(Name),
     fprof_text(This),
     fprof_tags(Up),
     fprof_tags(Down),
     fprof_beamfile(Name)}.

fprof_process_info({spawned_by, Who}) ->
    fmt("  ~s: ~s", [pad(16, "spawned_by"), Who]);
fprof_process_info({spawned_as, What}) ->
    fmt("  ~s: ~s", [pad(16, "spawned as"),
		   fprof_tag_name(What)]);
fprof_process_info({initial_calls, Calls}) ->
    fmt("  ~s: ~p~n", [pad(16, "initial calls"), Calls]);
fprof_process_info(Info) ->
    fmt("  ???: ~p~n", [Info]).

fprof_tag({M,F,A}) when integer(A) ->
    list_to_atom(flatten(io_lib:format("~p:~p/~p", [M,F,A])));
fprof_tag({M,F,A}) when list(A) ->
    fprof_tag({M,F,length(A)});
fprof_tag(Name) when  atom(Name) ->
    Name.

fprof_mfa({M,F,A}) -> [M,F,A];
fprof_mfa(_)       -> undefined.

fprof_tag_name(X) -> flatten(io_lib:format("~s", [fprof_tag(X)])).

fprof_text({Name, Cnt, Acc, Own}) ->
    fmt("~s~p\t~.3f\t~.3f\n",
        [pad(50, fprof_tag_name(Name)), Cnt, Acc, Own]).

fprof_tags(C) -> [fprof_tag(Name) || {Name,_,_,_} <- C].

fprof_beamfile({M,_,_}) ->
    case code:which(M) of
        Fname when list(Fname) ->
            l2b(Fname);
        X ->
            undefined
    end;
fprof_beamfile(_)                  -> undefined.

fmt(X, A) -> l2b(io_lib:format(X, A)).

l2b(X) -> list_to_binary(X).

pad(X, A) when atom(A) ->
    pad(X, atom_to_list(A));
pad(X, S) when length(S) < X ->
    S ++ lists:duplicate(X - length(S), $ );
pad(X, S) ->
    S.

null_gl() ->
    receive
	{io_request, From, ReplyAs, _} ->
	    From ! { io_reply, ReplyAs, ok},
	    null_gl();
	die ->
	    ok
    end.

%% ----------------------------------------------------------------------
%% Debugging
%% ----------------------------------------------------------------------

debug_toggle(Mod, Filename) ->
    case member(Mod, int:interpreted()) of
        true ->
            int:n(Mod),
            uninterpreted;
        false ->
            case int:i(Filename) of
                {module, Mod} ->
                    interpreted;
                error ->
                    error
            end
    end.

debug_add(Modules) ->
    lists:foreach(fun([Mod, FileName]) ->
			  %% FIXME: want to reliably detect whether
			  %% the module is interpreted, but
			  %% 'int:interpreted()' can give the wrong
			  %% answer if code is reloaded behind its
			  %% back.. -luke
			  int:i(FileName)
		  end, Modules),
    ok.

break_toggle(Mod, Line) ->
    case lists:any(fun({Point,_}) -> Point == {Mod,Line} end,
                   int:all_breaks()) of
        true ->
            ok = int:delete_break(Mod, Line),
            disabled;
        false ->
            ok = int:break(Mod, Line),
            enabled
    end.

break_delete(Mod, Line) ->
    case lists:any(fun({Point,_}) -> Point == {Mod,Line} end,
                   int:all_breaks()) of
        true ->
            ok = int:delete_break(Mod, Line);
        false ->
	    ok
    end.

break_add(Mod, Line) ->
    case lists:any(fun({Point,_}) -> Point == {Mod,Line} end,
                   int:all_breaks()) of
        true ->
            ok;
        false ->
            ok = int:break(Mod, Line)
    end.

%% L = [[Module, [Line]]]
break_restore(L) ->
    lists:foreach(
      fun([Mod, Lines]) ->
	      lists:foreach(fun(Line) -> int:break(Mod, Line) end, Lines)
      end, L),
    ok.


fname(Mod) ->
    filename:rootname(code:which(Mod), "beam") ++ "erl".

%% Returns: {InterpretedMods, Breakpoints, [{Pid, Text}]}
%%          InterpretedMods = [[Mod, File]]
%%          Breakpoints     = [{Mod, Line}]
debug_subscribe(Pid) ->
    %% NB: doing this before subscription to ensure that the debugger
    %% server is started (int:subscribe doesn't do this, probably a
    %% bug).
    Interpreted = lists:map(fun(Mod) -> [Mod, fname(Mod)] end,
			    int:interpreted()),
    spawn_link(?MODULE, debug_subscriber_init, [self(), Pid]),
    receive ready -> ok end,
    {Interpreted,
     [Break || {Break, _Info} <- int:all_breaks()],
     [{Proc,
       fmt("~p:~p/~p", [M,F,length(A)]),
       fmt("~w", [Status]),
       fmt("~w", [Info])}
      || {Proc, {M,F,A}, Status, Info} <- int:snapshot()]}.

debug_subscriber_init(Parent, Pid) ->
    link(Pid),
    int:subscribe(),
    Parent ! ready,
    debug_subscriber(Pid).

debug_subscriber(Pid) ->
    receive
        {int, {new_status, P, Status, Info}} ->
            Pid ! {int, {new_status, P, Status, fmt("~w",[Info])}};
        {int, {new_process, {P, {M,F,A}, Status, Info}}} ->
            Pid ! {int, {new_process,
                         [P,
                          fmt("~p:~p/~p", [M,F,length(A)]),
                          Status,
                          fmt("~w", [Info])]}};
	{int, {interpret, Mod}} ->
	    Pid ! {int, {interpret, Mod, fname(Mod)}};
	Msg ->
	    Pid ! Msg
    end,
    debug_subscriber(Pid).

debug_format(Pid, {M,F,A}, Status, Info) ->
    debug_format_row(io_lib:format("~w", [Pid]),
                     io_lib:format("~p:~p/~p", [M,F,length(A)]),
                     io_lib:format("~w", [Status]),
                     io_lib:format("~w", [Info])).

debug_format_row(Pid, MFA, Status, Info) ->
    fmt("~-12s ~-21s ~-9s ~-21s~n", [Pid, MFA, Status, Info]).

%% Attach the client process Emacs to the interpreted process Pid.
%%
%% spawn_link's a new process to proxy messages between Emacs and
%% Pid's meta-process.
debug_attach(Emacs, Pid) ->
    spawn_link(?MODULE, attach_init, [Emacs, Pid]).

%% State for attached process, based on `dbg_ui_trace' in the debugger.
-record(attach, {emacs,			% pid()
		 meta,			% pid()
		 status,		% break | running | idle | ...
		 where,			% {Mod, Line}
		 stack			% {CurPos, MaxPos}
		}).

attach_init(Emacs, Pid) ->
    link(Emacs),
    case int:attached(Pid) of
        {ok, Meta} ->
            attach_loop(#attach{emacs=Emacs,
					meta=Meta,
					status=idle,
					stack={undefined,undefined}});
        error ->
            exit({error, {unable_to_attach, Pid}})
    end.

attach_loop(Att = #attach{emacs=Emacs, meta=Meta}) ->
    receive
	{Meta, {break_at, Mod, Line, Pos}} ->
	    Att1 = Att#attach{status=break,
			      where={Mod, Line},
			      stack={Pos, Pos}},
	    attach_goto(Emacs, Meta, Mod, Line, Pos, Pos),
	    ?MODULE:attach_loop(Att1);
	{Meta, Status} when atom(Status) ->
	    Emacs ! {status, Status},
	    ?MODULE:attach_loop(Att#attach{status=Status,
					   where=undefined});
	{Meta, Other} ->
	    %% FIXME: there are more messages to handle, like
	    %% re_entry, exit_at
	    ?MODULE:attach_loop(Att);
	{emacs, meta, Cmd} when Att#attach.status == break ->
	    attach_loop(attach_meta_cmd(Att, Cmd));
	{emacs, meta, Cmd} ->
	    Emacs ! {message, <<"Not in break">>},
	    ?MODULE:attach_loop(Att)
    end.

attach_meta_cmd(Att, Cmd) when Att#attach.status /= break ->
    Att#attach.emacs ! {message, <<"Not in break">>},    
    Att;
attach_meta_cmd(Att = #attach{emacs=Emacs, meta=Meta, stack={Pos,Max}}, Cmd) ->
    case Cmd of
	_ when Cmd == up; Cmd == down ->
	    case int:meta(Meta, stack_frame, {Cmd, Pos}) of
		{NewPos, Mod, Line} ->
		    attach_goto(Emacs, Meta, Mod, Line, NewPos, Max),
		    Att#attach{stack={NewPos, Max}};
		X when X == top; X == bottom, Pos == Max ->
		    Emacs ! {message, <<"Can't go further">>},
		    Att;
		bottom ->
		    %% Special case: `int' tells us we're at the
		    %% bottom, but really we're trying to go down from
		    %% the second-last frame. Here we take ourselves
		    %% directly to the bottom when this happens.
		    {Mod, Line} = Att#attach.where,
		    attach_goto(Emacs, Meta, Mod, Line, Max, Max),
		    Att#attach{stack={Max, Max}}
	    end;
	{get_binding, Var} ->
	    Bs = int:meta(Meta, bindings, Pos),
	    case lists:keysearch(Var, 1, Bs) of
		{value, Val} ->
		    Emacs ! {show_variable, fmt("~p~n", [Val])};
		false ->
		    Emacs ! {message, fmt("No such variable: ~p",
					  [Var])}
	    end,
	    Att;
	_ ->
	    int:meta(Meta, Cmd),
	    Att
    end.

attach_goto(Emacs, Meta, Mod, Line, Pos, Max) ->
    Bs = sort(int:meta(Meta, bindings, Pos)),
    Vars = [{Name, fmt("~s = ~P~n", [pad(10, Name), Val, 9])} ||
	       {Name,Val} <- Bs],
    Emacs ! {variables, Vars},
    Emacs ! {location, Mod, Line, Pos, Max}.

%% ----------------------------------------------------------------------
%% Completion support
%% ----------------------------------------------------------------------

%% Returns: [ModName] of all modules starting with Prefix.
%% ModName = Prefix = string()
modules(Prefix) ->
    Modnames = [atom_to_list(Mod) || {Mod, _Filename} <- code:all_loaded()],
    {ok, lists:filter(fun(M) -> lists:prefix(Prefix, M) end, Modnames)}.

%% Returns: [FunName] of all exported functions of Mod starting with Prefix.
%% Mod = atom()
%% Prefix = string()
functions(Mod, Prefix) ->
    case catch Mod:module_info(exports) of
	{'EXIT', _} ->
	    {error, fmt("Can't call module_info/1 on ~p", [Mod])};
	List when list(List) ->
	    Fns = [atom_to_list(Fun) || {Fun, _Arity} <- List],
	    {ok, ordsets:to_list(ordsets:from_list(Fns))}
    end.

%% ----------------------------------------------------------------------
%% Refactoring
%% ----------------------------------------------------------------------

%% @spec free_vars(Text::string()) ->
%%           {ok, FreeVars::[atom()]} | {error, Reason::string()}
%% @equiv free_vars(Text, 1)

free_vars(Text) ->
    free_vars(Text, 1).

%% @spec free_vars(Text::string(), Line::integer()) ->
%%           {ok, FreeVars::[atom()]} | {error, Reason::string()}

free_vars(Text, StartLine) ->
    %% StartLine/EndLine may be useful in error messages.
    {ok, Ts, EndLine} = erl_scan:string("begin " ++ Text ++ " end", StartLine),
    Ts1 = lists:reverse([{dot, EndLine} | strip(lists:reverse(Ts))]),
    case erl_parse:parse_exprs(Ts1) of
	{ok, Es} ->
	    E = erl_syntax:block_expr(Es),
	    E1 = erl_syntax_lib:annotate_bindings(E, ordsets:new()),
	    {value, {free, Vs}} = lists:keysearch(free, 1,
						  erl_syntax:get_ann(E1)),
	    {ok, Vs};
	{error, {_Line, erl_parse, Reason}} ->
	    {error, fmt("~s", [Reason])}
    end.

strip([{',', _}   | Ts]) -> strip(Ts);
strip([{';', _}   | Ts]) -> strip(Ts);
strip([{'.', _}   | Ts]) -> strip(Ts);
strip([{'|', _}   | Ts]) -> strip(Ts);
strip([{'=', _}   | Ts]) -> strip(Ts);
strip([{'dot', _} | Ts]) -> strip(Ts);
strip([{'->', _}  | Ts]) -> strip(Ts);
strip([{'||', _}  | Ts]) -> strip(Ts);
strip([{'of', _}  | Ts]) -> strip(Ts);
strip(Ts)                -> Ts.

%% ----------------------------------------------------------------------
%% Online documentation
%% ----------------------------------------------------------------------

apropos(RE, false) ->
    apropos(RE);
apropos(RE, true) ->
    fdoc:stop(),
    apropos(RE).

apropos(RE) ->
    fdoc_binaryify(fdoc:get_apropos(RE)).

describe(M, F, A, false) ->
    describe(M, F, A);
describe(M, F, A, true) ->
    fdoc:stop(),
    describe(M, F, A).

describe(M, F, A) ->
    fdoc_binaryify(fdoc:description(M, F, A)).

%% Converts strings to binaries, for Emacs
fdoc_binaryify({ok, Matches}) ->
    {ok, [{M, F, A, list_to_binary(Doc)} || {M, F, A, Doc} <- Matches]};
fdoc_binaryify(Other) -> Other.

