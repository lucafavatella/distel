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

-export([process_list/0, process_summary/1, process_summary_and_trace/2]).

-export([tracer_init/2]).

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

