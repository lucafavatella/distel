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

-import(lists, [flatten/1, member/2, sort/1]).

-export([rpc_entry/3, eval_expression/1, find_source/1,
         process_list/0, process_summary/1,
         process_summary_and_trace/2, fprof/3,
         debug_toggle/1, break_toggle/2, debug_subscribe/1]).

-export([gl_proxy/1, tracer_init/2, null_gl/0]).

-compile(export_all).

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
            case catch erl_eval:exprs(Parse, []) of
                {value, V, _} ->
                    {ok, flatten(io_lib:format("~p", [V]))};
                {'EXIT', Reason} ->
                    {error, Reason}
            end;
        {error, {_, erl_parse, Err}} ->
            {error, Err}
    end.

parse_expr(S) ->
    {ok, Scan, _} = erl_scan:string(S),
    erl_parse:parse_exprs(Scan).

find_source(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            case code:is_loaded(Mod) of
                {file, preloaded} ->
                    {error, preloaded};
                {file, Name} ->
                    case guess_source_file(Name) of
                        {ok, Fname} ->
                            {ok, Fname};
                        false ->
                            {error, cannot_guess_sourcefile}
                    end
            end;
        {error, What} ->
            {error, What}
    end.

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
                            false
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
    spawn_tracer(Tracer, Pid),
    process_summary(Pid).

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
    {ok, Asys} = file:consult("/tmp/fprof.analysis"),
    [_Opts, [Totals], _Proc | Fns] = Asys,
    {ok,
     fprof_preamble(Totals),
     fprof_header(),
     [fprof_entry(F) || F <- Fns]}.

fprof_preamble({totals, Cnt, Acc, Own}) ->
    fmt("Totals: ~p calls, ~.3f real, ~.3f CPU\n\n", [Cnt, Acc, Own]).

fprof_header() ->
    fmt("~sCalls\tACC\tOwn\n", [pad(50, "Function")]).

fprof_entry(F) ->
    {Up, This, Down} = F,
    {Name, _, _, _} = This,
    {fprof_tag(Name),
     fprof_mfa(Name),
     fprof_text(This),
     fprof_tags(Up),
     fprof_tags(Down),
     fprof_beamfile(Name)}.

fprof_tag({M,F,A}) ->
    list_to_atom(flatten(io_lib:format("~p:~p/~p", [M,F,A])));
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

debug_toggle(Mod) ->
    case member(Mod, int:interpreted()) of
        true ->
            int:n(Mod),
            uninterpreted;
        false ->
            case int:i(Mod) of
                {module, Mod} ->
                    interpreted;
                error ->
                    error
            end
    end.

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

%% Returns: {Header, [{Pid, Text}]}
debug_subscribe(Pid) ->
    spawn_link(?MODULE, debug_subscriber_init, [self(), Pid]),
    receive ready -> ok end,
    [{Pid,
      fmt("~p:~p/~p", [M,F,length(A)]),
      fmt("~w", [Status]),
      fmt("~w", [Info])}
     || {Pid, {M,F,A}, Status, Info} <- int:snapshot()].

debug_subscriber_init(Parent, Pid) ->
    link(Pid),
    int:subscribe(),
    Parent ! ready,
    debug_subscriber(Pid).

debug_subscriber(Pid) ->
    receive
        {int, {new_status, P, Status, Info}} ->
            Pid ! [int, [new_status, P, fmt("~w",[Status]), fmt("~w",[Info])]];
        {int, {new_process, {P, {M,F,A}, Status, Info}}} ->
            Pid ! [int, [new_process,
                         [P,
                          fmt("~p:~p/~p", [M,F,length(A)]),
                          fmt("~w", [Status]),
                          fmt("~w", [Info])]]];
        _ ->
	    ok
    end,
    ?MODULE:debug_subscriber(Pid).

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
	{meta, Other} ->
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
    if
	Cmd == up; Cmd == down ->
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
	true ->
	    int:meta(Meta, Cmd),
	    Att
    end.

attach_goto(Emacs, Meta, Mod, Line, Pos, Max) ->
    Bs = sort(int:meta(Meta, bindings, Pos)),
    Vars = [fmt("~s = ~P", [pad(10, Name), Val, 9]) || {Name,Val} <- Bs],
    Emacs ! {variables, Vars},
    Emacs ! {location, Mod, Line, Pos, Max}.
