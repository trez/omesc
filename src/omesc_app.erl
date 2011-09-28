-module(omesc_app).

-behaviour(application).

-include("omesc.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, [{port, Port} | _StartArgs]) ->
    ?debug("Start"),
    omesc_sup:start_link([[ainterface, Port, {ainterface, start_link}]]).

stop(_State) ->
    ok.
