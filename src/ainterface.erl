-module(ainterface).
-behaviour(gen_server).
-include("omesc.hrl").
-include("ipaccess.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {socket=null}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Socket) ->
    ?debug("start"),
    Res = gen_server:start_link(?MODULE, [Socket], []),
    case Res of
        {ok, Pid} ->
            ?debug("Giving control to gen_server"),
            gen_tcp:controlling_process(Socket, Pid);
        _Otherwise ->
            ?debug("Error creating process")
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Socket]) ->
    ?debug("init"),
    State = #state{socket=Socket},
    ID_ACK = ipaccess:encode(#ipa_id_ack{}),
    send(State, ID_ACK),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({tcp, _Port, Data}, State)->
    handle_data(State, Data),
    {noreply, State};
handle_info({tcp_closed, Port}, State) ->
    ?debug("closing."),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send(#state{socket=S}, Data) ->
    gen_tcp:send(S, Data).

handle_data(State, Data) ->
    {Resp, Rest} = ipaccess:decode(Data),

    case Resp of
        #ipa_ping{}->
            ?debug(">BSC> Ping"),
            Pong = ipaccess:encode(#ipa_pong{}),
            send(State, Pong),
            ?debug("<BSC< Pong."),
            {noreply, State};
        #ipa_id_ack{} ->
            ?debug(">BSC> ID-ACK, do nothing"),
            {noreply, State};
        _Otherwise ->
            ?debug(">BSC> Unknown message, do nothing"),
            {noreply, State}
    end,

    % If there is more data to be parsed.
    (Rest /= <<>>) andalso handle_data(State, Rest).
