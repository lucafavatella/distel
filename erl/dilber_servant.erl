%%%-------------------------------------------------------------------
%%% File    : dilber_servant.erl
%%% Author  :  <svg@surnet.ru>
%%% Description : 
%%%
%%% Created :  7 Apr 2002 by  <svg@surnet.ru>
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
%% .* dilber_servant
%% 
%%*
-module(dilber_servant).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user,
                master,
                log,
                cont = start,
                psize= infinity,
                pnum = 0,
                rest = [],
                tail = false,
                tail_ref = none,
                grep = fun dummy_grep/1,
                format = fun dummy_format/1}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Log, Opts, Master]) ->
    process_flag(priority, low),
    process_flag(trap_exit, true),
    ChkOpts = check_show_opts(Opts, #state{}),
    [User, Timeout, PgSize, Grep, Format] = [getopt:value(K, ChkOpts)
					     || K <- [user, tail, psize, grep, format]],
    case User of
	undefined -> ok;
	_ when pid(User);atom(User) -> link(User);
	_ -> ok
    end,
    {ok, #state{user=User, master=Master, log=Log,
		tail=Timeout, psize=PgSize, grep=Grep, format=Format}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({show, Opts}, From, State) ->
    {Reply, NewState} = (catch do_show(State, Opts)),
    {reply, Reply, NewState};
handle_call(rescan, From, State) ->
    {Reply, NewState} = (catch do_show(State, [])),
    {reply, Reply, NewState};

handle_call(next, From, S=#state{tail=Tail, tail_ref=TRef}) when Tail /= false ->
    cancel_timer(TRef),
    {reply, [], S#state{tail_ref=erlang:start_timer(0, self(), next)}};
handle_call(next, From, State) ->
    {Reply, NewState} = do_next(State),
    {reply, Reply, NewState};

handle_call(info, From, State) ->
    {reply, State, State};
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(close, S) ->
    {stop, normal, S};
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reson}, S=#state{user=User, master=Master})
  when Pid == User;
       Pid == Master ->
    {stop, Reson, S};
handle_info({timeout, Ref, next}, S=#state{user=User, tail=Timeout, tail_ref=Ref}) when number(Timeout) ->
    cancel_timer(Ref),
    {Res, NewState} = do_next(S),
    User ! {next, self(), Res},
    {noreply, NewState#state{tail_ref=erlang:start_timer(Timeout, self(), next)}};
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_show(S=#state{log=Log, tail_ref=TailRef, user=User}, Opts) ->
    cancel_timer(TailRef),
    ChkOpts = check_show_opts(Opts, S),
    [Timeout, PgSize, Grep, Format] = [getopt:value(K, ChkOpts)
				       || K <- [tail, psize, grep, format]],
    S1 = S#state{tail=Timeout, tail_ref=none,
		 pnum=0, cont=start, psize=PgSize, rest=[],
		 grep=Grep, format=Format},

    {Result, NewState} =
	case Timeout of
	    _ when number(Timeout) ->
		S2 = skip_to_last(S1),
		TRef = erlang:start_timer(0, self(), next),
		{[], S2#state{tail_ref=TRef}};
	    false ->
		do_next(S1)
	end.

skip_to_last(S=#state{rest=Rest}) ->
    skip_to_last(Rest, do_next(S)).

skip_to_last([], {[], State}) -> %already at the end
    State;
skip_to_last(_, {Res, S=#state{pnum=PgNum, psize=infinity}}) ->
    S#state{pnum=PgNum - 1, rest=Res};
skip_to_last(PrevRes, {Res, S=#state{pnum=PgNum, psize=PgSize}})
  when length(Res) < PgSize; Res == [] ->
    PrevTailLen = length(PrevRes) - min(PgSize - length(Res), length(PrevRes)),
    PrevTail = lists:nthtail(PrevTailLen, PrevRes),
    S#state{pnum=PgNum - 1, rest=PrevTail++Res};
skip_to_last(_, {Res, State}) ->
    skip_to_last(Res, do_next(State)).

do_next(S=#state{log=Log,
                 cont=Cont,
                 pnum=PgNum,
                 psize=PgSize,
                 rest=Rest,
                 grep=Grep,
                 format=Format
                }) ->
    {Result, NewState} =
        case read_log(Log, Cont, Grep, Format, PgSize, Rest) of
	    {ok, NewCont, Res=[], NewRest=[]} -> %already at eof
		{Res, S#state{cont=NewCont, rest=NewRest}};
	    {ok, NewCont, Res, NewRest} ->
		NewPgNum = if PgSize == infinity -> 1; true -> PgNum+1 end,
		{Res, S#state{pnum=NewPgNum, cont=NewCont, rest=NewRest}};
	    Error ->
		throw({Error, S})
        end,
    {Result, NewState}.

read_log(_, Cont, _, _, BlockSize, Acc)
  when BlockSize =< length(Acc) ->
    {ok, Cont, lists:sublist(Acc, BlockSize), lists:nthtail(BlockSize, Acc)};
read_log(Log, Cont, Grep, Format, BlockSize, Acc) ->
    case disk_log:chunk(Log, Cont) of
	{NextCont, Terms} ->
	    NextAcc = Acc ++
		[Str || Str <- lists:map(Format, Terms), Grep(Str) =/= false],
	    read_log(Log, NextCont, Grep, Format, BlockSize, NextAcc);
	eof ->
	    {ok, Cont, Acc, []};
	Error = {error, _} ->
	    Error
    end.

check_show_opts(Args, S=#state{user=DefUser,
                               tail=Timeout, psize=DefPgSize,
                               grep=DefGrep, format=DefFormat}) ->
    ChkPgSize = fun (infinity) -> {ok, infinity};
		    (N) when integer(N), N > 0 -> {ok, N};
		    (V) -> {error, {bad_number, V}}
		end,
    ChkGrep = fun (none) -> {ok, fun dummy_grep/1};
		  (F) when function(F) -> {ok, F};
		  (V) -> {error, {badarg, V}}
	      end,
    ChkFormat = fun (none) -> {ok, fun dummy_format/1};
		    (F) when function(F) -> {ok, F};
		    (V) -> {error, {badarg, V}}
		end,
    ChkTail = fun (false) ->
		      {ok, false};
		  (T) when number(T);T > 0 ->
		      {ok, T};
		  (V) ->
		      {error, {badarg, V}}
	      end,

    Opts = getopt:options(Args,
			  [{user,   DefUser,   term},
			   {tail,  Timeout,   ChkTail},
			   {psize,  DefPgSize, ChkPgSize},
			   {grep,   DefGrep,   ChkGrep},
			   {format, DefFormat, ChkFormat}
			  ],
			  defined_strict),
    Opts.

cancel_timer(none) ->
    ok;
cancel_timer(Ref) ->
    erlang:cancel_timer(Ref),
    receive
	{timeout, Ref, _} ->
	    ok
    after 0 ->
	    ok
    end.

min(A, B) when A =< B -> A;
min(_, B) -> B.

dummy_grep(_) ->
    true.

dummy_format(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).
