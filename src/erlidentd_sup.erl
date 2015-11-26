%%%-------------------------------------------------------------------
%% @doc identd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlidentd_sup).
-author('kim@dxtr.im').

-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 0, 15},
	  [{serv,
	    {erlidentd_server, start_link, []},
	    5000,
	    worker,
	    [erlidentd_fsm]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
