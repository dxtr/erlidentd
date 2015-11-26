-module(erlidentd_listener).
-author('kim@dxtr.im').

-behaviour(gen_server).

-export([start_link/4, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

-record(state, {
	  listener,       % Listening socket
	  acceptor,       % Asynchronous acceptor's internal reference
	  module          % FSM handling module
	 }).

% start_link/3
start_link({ok, IP}, Port, Name, Module) when is_integer(Port),
					is_atom(Module) ->
    start_link(IP, Port, Name, Module);
start_link(IP, Port, Name, Module) when is_integer(Port),
			      is_atom(Module) ->
    gen_server:start_link({local, Name}, ?MODULE, [IP, Port, Module], []);
start_link(_IP, _Port, _Name, _Module) ->
    % What to do here? Port isn't an integer and Module is not an atom
    io:format("Invalid data passed!~n").


stop([]) -> gen_server:call([?MODULE], stop).

init([IP, Port, Module]) ->
    io:format("Initializing listener..~n"),
    process_flag(trap_exit, true),
    Opts = [binary,
	    {reuseaddr, true},
	    {keepalive, true},
	    {backlog, 30},
	    {active, false},
	    {ip, IP}
	   ],
    case gen_tcp:listen(Port, Opts) of
	{ok, LSock} ->
	    %%Create first accepting process
	    {ok, Ref} = prim_inet:async_accept(LSock, -1),
	    {ok, #state{listener = LSock,
			acceptor = Ref,
			module   = Module}};
	{error, Reason} ->
	    {stop, Reason}
    end.

    %Opts = [binary,],
    %% sets a seed for random number generation for the life of the process
    %% uses the current time to do it. Unique value guaranteed by now()
    %random:seed(now()),
    %StrId = atom_to_list(Pid),
    %io:format("init. id: ~s~n", [StrId]),
    %{ok, []}.

handle_call(Request, _From, State) ->
    io:format("listener handle_call~n"),
    {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    io:format("listener handle_cast~n"),
    {noreply, State}.

handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, acceptor=Ref, module=Module} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
	    ok              -> ok;
	    {error, Reason} -> exit({set_sockopt, Reason})
        end,
	
        %% New client connected - spawn a new process using the simple_one_for_one
        %% supervisor.
        {ok, Pid} = erlidentd_app:start_client(),
        gen_tcp:controlling_process(CliSocket, Pid),
        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, CliSocket),
	
        %% Signal the network driver that we are ready to accept another connection
        case prim_inet:async_accept(ListSock, -1) of
	    {ok,    NewRef} -> ok;
	    {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,
	
        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
	    error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
	    {stop, Why, State}
    end;

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
	    case prim_inet:setopts(CliSocket, Opts) of
		ok -> ok;
		Error -> gen_tcp:close(CliSocket), Error
	    end;
	Error ->
	    gen_tcp:close(CliSocket), Error
    end.
