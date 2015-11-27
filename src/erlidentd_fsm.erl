-module(erlidentd_fsm).
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
 
-define(TIMEOUT, 10000).
-define(IDENT_PATTERN, "^\s*([0-9]{1,5})\s*,\s*([0-9]{1,5})\s*$").
-define(IDENT_COMPILED_PATTERN, re:compile(?IDENT_PATTERN, [{newline, anycrlf}])).

% To get hexadecimals from binary data: lists:flatten(io_lib:format("~40.16.0b", [data])).
 
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).
 
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).
 
init(_) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.
 
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, binary]),
    {ok, {IP, Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP, port=Port}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p~n", [Other]),
    {next_state, 'WAIT_FOR_SOCKET', [State]}.
 
'WAIT_FOR_DATA'({data, Data}, #state{socket=S} = State) ->
    Response = handle_data(binary_to_list(Data)),
    ok = gen_tcp:send(S, [Response, 13, 10, 0]),
    gen_tcp:close(S),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
'WAIT_FOR_DATA'(timeout, State) ->
    {stop, normal, State};
'WAIT_FOR_DATA'(_Data, State) ->
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

generate_randomness() ->
    lists:flatten([io_lib:format("~2.16.0b", [Byte]) || Byte <- binary_to_list(crypto:strong_rand_bytes(4))]).

generate_response(Serverport, Clientport) when is_integer(Serverport),
					       Serverport > 0,
					       is_integer(Clientport),
					       Clientport < 65536 ->
    io_lib:format("~.10b , ~.10b : USERID : UNIX : ~s", [Serverport, Clientport, generate_randomness()]);
generate_response(Serverport, Clientport) when is_integer(Serverport),
					       is_integer(Clientport) ->
    io_lib:format("~.10b , ~.10b : ERROR : INVALID-PORT", [Serverport, Clientport]);
generate_response(_Serverport, _Clientport) ->
    generate_response(0,0).

handle_data(Data) ->
    {_ReStatus, RePattern} = ?IDENT_COMPILED_PATTERN,
    case re:run(Data, RePattern, []) of
	{match, [_Fullmatch, {ServerportStart, ServerportLength}, {ClientportStart, ClientportLength}]} ->
	    Serverport = str_to_port(string:substr(Data, ServerportStart + 1, ServerportLength)),
	    Clientport = str_to_port(string:substr(Data, ClientportStart + 1, ClientportLength)),
	    generate_response(Serverport, Clientport);
	nomatch ->
	    generate_response(0,0)
    end.

str_to_port(Port) ->
    case string:to_integer(Port) of
	{error, _} -> 0;
	{Res, _} -> Res
    end.

handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
 
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);
handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=_Addr} = StateData) ->
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.
 
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
