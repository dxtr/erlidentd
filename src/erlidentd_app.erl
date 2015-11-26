-module(erlidentd_app).
-author('kim@dxtr.im').
-behaviour(application).
-export([start/0, start/2, stop/1, start_client/0, init/1]).
-define(MAX_RESTART,  5).
-define(MAX_TIME,    60).
-define(DEF_PORT,   1130).

start(_Type, _Args) ->
    LPort = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LPort, erlidentd_fsm]).

start() ->
    start([], []).

stop(_State) -> ok.

start_client() ->
    supervisor:start_child(erlidentd_fsm_sup, []).

init([Port, Module]) ->
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       { erlidentd_listener4_sup,                          % Id       = internal id
	 {erlidentd_listener,start_link,[inet:parse_address("127.0.0.1"), Port, listenerv4, Module]}, % StartFun = {M, F, A}
	 permanent,                               % Restart  = permanent | transient | temporary
	 2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
	 worker,                                  % Type     = worker | supervisor
	 [erlidentd_listener]                           % Modules  = [Module] | dynamic
       },
       { erlidentd_listener6_sup,
	 {erlidentd_listener, start_link, [inet:parse_address("::1"), Port, listenerv6, Module]},
	 permanent,
	 2000,
	 worker,
	 [erlidentd_listener]
       },
						% Client instance supervisor
       { erlidentd_fsm_sup,
	 {supervisor,start_link,[{local, erlidentd_fsm_sup}, ?MODULE, [Module]]},
	 permanent,                               % Restart  = permanent | transient | temporary
	 infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
	 supervisor,                              % Type     = worker | supervisor
	 []                                       % Modules  = [Module] | dynamic
       }
      ]
     }
    };
init([Module]) ->
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
						% TCP Client
       {   undefined,                               % Id       = internal id
	   {Module,start_link,[]},                  % StartFun = {M, F, A}
	   temporary,                               % Restart  = permanent | transient | temporary
	   2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
	   worker,                                  % Type     = worker | supervisor
	   []                                       % Modules  = [Module] | dynamic
       }
      ]
     }
    }.

get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
	{ok, Val} -> Val;
	_ ->
	    case init:get_argument(Opt) of
		[[Val | _]] -> Val;
		error       -> Default
	    end
    end.
