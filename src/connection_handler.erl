-module(connection_handler).
-behavior(gen_server).

-include("omesc.hrl").

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start_link/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]).

-record(server_state, {
        port,
        loop,
        ip=any,
        lsocket=null}).

start_link(Name, Port, Loop) ->
    ?debug("Start ~w", [Name]),
    State = #server_state{port = Port, loop = Loop},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            ?debug("Listening socket at port ~w created!", [Port]),
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    ?debug("Wait for more connections"),
    {noreply, accept(State)}.

accept_loop({Server, LSocket, {M, F}}) ->
    ?debug("Waiting for connection"),
    {ok, Socket} = gen_tcp:accept(LSocket),
    ?debug("Got connection!"),
    gen_server:cast(Server, {accepted, self()}),
    ?debug("Calling: ~w:~w", [M, F]),
    M:F(Socket).

accept(State = #server_state{lsocket=LSocket, loop = Loop}) ->
    Pid = spawn_link(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    erlang:monitor(process, Pid),
    State.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
