%%%-------------------------------------------------------------------
%%% File    : distel.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Helper functions to be called from Emacs.
%%%
%%% Created : 18 Mar 2002 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(distel).

-author('luke@bluetail.com').

-import(lists, [flatten/1]).

-export([process_list/0, process_summary/1, process_summary_and_trace/2, fprof/3]).

-export([tracer_init/2, null_gl/0]).

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
%% fprof(M, F, A) -> {ok, Preamble, Header, Entry}
%% Preamble = binary()
%% Entry = {Tag, MFA, Text, Callers, Callees, Beamfile}
%% Callers = Callees = [Tag]
%% MFA = [Module, Function, Arity] | undefined
%%
%% Entry example,
%%   {'foo:bar/2', "foo:bar/2  10 100 200", ['baz:beer/2'], [], "/foo.beam"}

fprof(M, F, A) ->
    GL = spawn_link(fun null_gl/0),
    group_leader(GL, self()),
    fprof:apply(M, F, A),
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
    list_to_atom(lists:flatten(io_lib:format("~p:~p/~p", [M,F,A])));
fprof_tag(Name) when  atom(Name) ->
    Name.

fprof_mfa({M,F,A}) -> [M,F,A];
fprof_mfa(_)       -> undefined.

fprof_tag_name(X) -> lists:flatten(io_lib:format("~s", [fprof_tag(X)])).

fprof_text({Name, Cnt, Acc, Own}) ->
    fmt("~s~p\t~.3f\t~.3f\n",
	[pad(50, fprof_tag_name(Name)), Cnt, Acc, Own]).

fprof_tags(C) -> [fprof_tag(Name) || {Name,_,_,_} <- C].

fprof_beamfile({M,_,_}) -> l2b(code:which(M));
fprof_beamfile(_)                  -> undefined.

fmt(X, A) -> l2b(io_lib:format(X, A)).

l2b(X) -> list_to_binary(X).

pad(X, S) when length(S) < X ->
    S ++ lists:duplicate(X - length(S), $ ).

null_gl() ->
    receive
	{io_request, From, ReplyAs, _} ->
	    From ! { io_reply, ReplyAs, ok},
	    null_gl();
	die ->
	    ok
    end.


