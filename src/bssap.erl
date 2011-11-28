%% ---------------------------------------------------------------------------
%% File    : bssap.erl
%% @author OMeSC
%% @doc Parsing and Decoding of bssap protocol.
%% @version '0.000001'
%% @end
%% ---------------------------------------------------------------------------
-module(bssap).

-export([decode/1, encode/1]).

-include("omesc.hrl").

%% ---------------------------------------------------------------------------
%% @doc Decodes a binary as a bssap packet.
%% @spec decode(Bin :: binary()) -> {bssap_packet(), binary()} | error
%% @end
%% ---------------------------------------------------------------------------
decode(Bin) ->
  bssap_decode:start(Bin),
  error.

%% ---------------------------------------------------------------------------
%% @doc Encode a bssap packet into a binary.
%% @spec encode(Packet :: bssap_packet()) -> binary() | error
%% @end
%% ---------------------------------------------------------------------------
encode(Packet) ->
  bssap_encode:start(Packet),
  error.
