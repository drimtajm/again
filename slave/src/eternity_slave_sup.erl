%%%-------------------------------------------------------------------
%%% @author  drimtajm
%%% @copyright (C) 2018
%%% @doc     Supervisor for eternity slave application
%%%
%%% @end
%%% Created : 9 Aug 2018 by drimtajm
%%%-------------------------------------------------------------------
-module(eternity_slave_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKER(Name, Restart, Shutdown, Type), 
	{Name, {eternity_worker, start_link, Name},
	 Restart, Shutdown, Type, [eternity_worker]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    NumWorkers = erlang:system_info(logical_processors_available),
    Workers = lists:map(fun (Id) ->
				Name = get_worker_name(Id),
				?WORKER(Name, permanent, 20000, worker)
			end, lists:seq(1, NumWorkers)),
    put(workers, Workers),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Workers]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Workers]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Workers}}.

stop() ->
    Workers = get(workers),
    lists:foreach(fun ({Name, _, _, _, _, _} = _Worker) ->
			  eternity_worker:stop(Name)
		  end, Workers),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_worker_name(Id) ->
    list_to_atom("eternity_worker_"++[$0+Id]).
