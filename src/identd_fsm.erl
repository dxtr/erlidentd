-module(identd_fsm).
-author('kim@dxtr.im').
 
-behaviour(gen_fsm).
 
-export([start_link/0, set_socket/2]).
 
%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
 
%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2
]).
 
-record(state, {
	  socket,    % client socket
	  addr,      % client address
	  port       % client port
	 }).
 
-define(TIMEOUT, 120000).

% To get hexadecimals from binary data: lists:flatten(io_lib:format("~40.16.0b", [data])).
 
start_link() ->
    io:format("Starting fsm link~n"),
    gen_fsm:start_link(?MODULE, [], []).
 
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    io:format("Setting socket~n"),
    gen_fsm:send_event(Pid, {socket_ready, Socket}).
 
init(_) ->
    io:format("fsm init~n"),
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.
 
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    io:format("~p Socket is ready~n", [self()]),
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    inet:setopts(Socket, [{active, once}]),
    {ok, {IP, Port}} = inet:peername(Socket),
    io:format("~p Connection from ~p:~p~n", [self(), IP, Port]),
    io:format("State: ~p~n", [State#state{socket=Socket, addr=IP}]),
    {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    io:format("wait_for_socket: ~p~n", [State]),
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p~n", [Other]),
    {next_state, 'WAIT_FOR_SOCKET', [State]}.
 
'WAIT_FOR_DATA'({data, Data}, #state{socket=S} = State) ->
    io:format("Got some data! ~p~n", [Data]),
    ok = gen_tcp:send(S, Data),
    io:format("Sent some data!~n"),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.~n", [self()]),
    {stop, normal, State};
'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p~n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.
 
handle_event(Event, StateName, StateData) ->
    io:format("~p Event ~p happened~n", [self(), Event]),
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_sync_event(Event, From, StateName, StateData) ->
    io:format("~p Sync event ~p from ~p happened~n", [self(), Event, From]),
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    io:format("~p Something happened.~n", [self()]),
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);
handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=Addr} = StateData) ->
    io:format("~p Client ~p disconnected.~n", [self(), Addr]),
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    io:format("~p Something generic happened~n", [self()]),
    {noreply, StateName, StateData}.

terminate(_Reason, _StateName, #state{socket=Socket}) ->
    io:format("Terminate~n"),
    (catch gen_tcp:close(Socket)),
    ok.
 
code_change(_OldVsn, StateName, StateData, _Extra) ->
    io:format("code_change~n"),
    {ok, StateName, StateData}.
