%%%-------------------------------------------------------------------
%%% File    : dilber.erl
%%% Author  :  <svg@surnet.ru>
%%% Description : Interface for online disk log browsing
%%%
%%% Created :  6 Apr 2002 by  <svg@surnet.ru>
%%%-------------------------------------------------------------------
%%
%% $Id$
%%
%% $Log$
%%
%%**
%% 
%% .* dilber
%% 
%%*
-module(dilber).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start/0, start_link/0, stop/0, info/1, info/2]).
-export([list/0, list/1, open/2, open/3, close/1, grep/3, show/2, rescan/1, next/1]).

-export([test_log_open/0, test_log_close/0, test_gen/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {servants=[]}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
  start([]).

start_link() ->
  start_link([]).

stop() ->
  stop(node()).

stop(Node) ->
  gen_server:cast({?SERVER, Node}, stop).

start(Opts) ->
  gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

start_link(Opts) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

info(Server) when pid(Server) ->
  gen_server:call(Server, info, infinity);
info(Query) ->
  info(node(), Query).

info(Node, Query) when atom(Node) ->
  gen_server:call({?SERVER, Node}, {info, Query}, infinity).

list() ->
  list(node()).

list(Node) ->
  gen_server:call({?SERVER, Node}, list, infinity).

open(Log, Opts) ->
  open(node(), Log, Opts).

open(Node, Log, Opts) ->
  gen_server:call({?SERVER, Node}, {open, Log, Opts}, infinity).

close(Servant) ->
  gen_server:cast(Servant, close).

grep(Servant, RegExp, Opts) ->
  case regexp:parse(RegExp) of
    {ok, RE} ->
      GrepF = fun(Str) ->
                  case regexp:first_match(Str, RE) of
                    {match, _,  _} -> true;
                    _ -> false
                  end
              end,
      show(Servant, [{grep, GrepF}|Opts]);
    Error ->
      Error
  end.

show(Servant, Opts) ->
  gen_server:call(Servant, {show, Opts}, infinity).

rescan(Servant) ->
  gen_server:call(Servant, rescan, infinity).

next(Servant) ->
  gen_server:call(Servant, next, infinity).

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
init(Opts) ->
  process_flag(priority, low),
  process_flag(trap_exit, true),
  {ok, #state{}}.

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
handle_call(list, From, State) ->
  {Local, Dist} = disk_log:accessible_logs(),
  Reply = [{Name, list_to_binary(io_lib:format("~w", [Name]))}
           || Name <- Local ++ Dist],
  {reply,  Reply, State};
handle_call({open, Log, Opts}, From, State) ->
  case catch open_log(Log, getopt:options(Opts, [{user, From, term}], all), State) of
    {ok, Reply, NewState} ->
      {reply, Reply, NewState};
    Error ->
      {reply, better_error(Error), State}
  end;
handle_call({info, state}, From, State) ->
  {reply, State, State};
handle_call({info, {log, LogName}}, From, State) ->
  Format = fun(V ={Key, _}) when Key == file ->
               V;
              ({Key, Value}) ->
               {Key, list_to_binary(io_lib:format("~w", [Value]))}
           end,
  Reply = [Format(V) || V <- disk_log:info(LogName)],
  {reply, Reply, State};
handle_call({info, _}, From, State) ->
  {reply, bad_query, State};
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
handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reson}, S=#state{servants=Servants}) ->
  {noreply, S#state{servants=Servants -- [Pid]}};
handle_info(Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, S=#state{servants=Servants}) ->
  lists:foreach(fun(Pid) -> (catch exit(Pid, normal)) end, Servants),
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

open_log(Log, Opts, S=#state{servants=Servants}) ->
  {ok, LogName} = open_log(Log),
  {ok, Pid} = dilber_servant:start_link([LogName, Opts, self()]),
  {ok, Pid, S#state{servants=[Pid|Servants]}}.

open_log(Log) ->
  case disk_log:info(Log) of
    Error = {error, _} ->
      Error;
    LogInfo ->
      Opts = check_log_opts(LogInfo),
      Name = getopt:value(name, Opts),
      {ok, Name}
  end.

check_log_opts(LogInfo) ->
  ChkFormat = fun (V=internal) -> {ok, V};
                  (BadFormat) -> {error, {bad_format, BadFormat}}
              end,
  ChkName = fun (LogName) ->
                {Local, Dist} = disk_log:accessible_logs(),
                case lists:member(LogName, Local ++ Dist) of
                  true -> {ok, LogName};
                  false -> {error,no_such_log}
                end
            end,

  getopt:options(LogInfo,
                 [{name, required, ChkName},
                  {file, required, list},
                  {type, required, atom},
                  {format, required, ChkFormat},
                  {size, required, term}
                 ]).

better_error({'EXIT', {{badmatch, Error}, _}}) ->
  better_error(Error);
better_error({aborted, Reson}) ->
  {error, mnesia:error_description(Reson)};
better_error({'EXIT', Error}) ->
  better_error(Error);
better_error(Error) ->
  Error.

  
test_log_open() ->
  Opts = [{name,dilber_test},
          {file,"./dilber_test"},
          {type,wrap},
          {format,internal},
          {size,{1000, 3}},
          {mode,read_write}],
  error_logger:add_report_handler(error_log_h, Opts).

test_log_close() ->
  error_logger:delete_report_handler(error_log_h).

test_gen(N) when N < 0 ->
  ok;
test_gen(N) when integer(N) ->
  error_logger:info_report([{'DILBER_TEST', {N, erlang:now()}}]),
  test_gen(N - 1).

%%test() ->
%%  spawn(fun () -> test1() end).
%%
%%test1 () ->
%%  SPid = dilber:open(account, [{user, self()}, {psize, 25},{tail, 30000}]),
%%  dilber:show(SPid, []),
%%  receive
%%    {next, SPid, Result} ->
%%      io:format("Message from ~w : ~w ~p~n", [SPid, length(Result), Result]),
%%      io:format("State of ~w: ~p~n", [dilber:info(SPid), SPid]);
%%    Msg ->
%%      io:format("Unknown msg: ~p ~n", [Msg])
%%  after 60000 ->
%%      ok
%%  end.
