%%%-------------------------------------------------------------------
%%% File    : error_log_h.erl
%%% Author  :  <svg@disney.surnet.local>
%%% Description : 
%%%
%%% Created : 27 Apr 2002 by  <svg@disney.surnet.local>
%%%-------------------------------------------------------------------
%%
%% $Id$
%%
%% $Log$
%% Revision 1.1  2002/04/28 18:25:33  lukeg
%% *** empty log message ***
%%
%%
%%**
%% 
%% .* error_log_h
%% 
%%*

-module(error_log_h).

-behaviour(gen_event).

%% External exports

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

-record(state, {log}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, S}          |
%%          Other
%%----------------------------------------------------------------------
init(Opts) ->
    case disk_log:open(Opts) of
	{ok, Log} ->
	    {ok, #state{log = Log}};
	{repaired, Log, _, _} ->
	    {ok, #state{log = Log}};
	Error ->
	    Error
    end.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, S}                                |
%%          {swap_handler, Args1, S1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event(Event, S) ->
    case disk_log:alog(S#state.log, Event) of
	ok ->
	    {ok, S};
	Error ->
	    Error
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, S}                                |
%%          {swap_handler, Reply, Args1, S1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(info, S) ->
    {ok, S, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, S}                                |
%%          {swap_handler, Args1, S1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info(Info, S) ->
    {ok, S}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(Arg, S) ->
    disk_log:close(S#state.log).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
