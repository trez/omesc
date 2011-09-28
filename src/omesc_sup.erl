
-module(omesc_sup).

-behaviour(supervisor).

-include("omesc.hrl").
%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type),       {I, {I, start_link, []},   permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link([AInterface]) ->
    ?debug("Start supervisor ~w", [1]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AInterface]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([AInterface]) ->
    AInterface_spec = ?CHILD(connection_handler, worker, AInterface),
    ?debug("Init supervisor"),
    {ok, {{one_for_one, 5, 10},
          [AInterface_spec]
         }
    }.

