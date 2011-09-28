%% ---------------------------------------------------------------------------
%% File    : ipaccess.erl
%% @author OpenMSC
%% @doc Parsing and Decoding of ipaccess layer.
%% @version '0.000001'
%% @headerfile "ipaccess.hrl"
%% @end
%% ---------------------------------------------------------------------------
-module(ipaccess).

-export([decode/1, encode/1]).

-include("ipaccess.hrl").

%% ---------------------------------------------------------------------------
%% @doc Decodes a binary as a ipaccess packet.
%% @spec decode(Bin :: binary()) -> #ipaccess{}
%% @end
%% ---------------------------------------------------------------------------
decode(<<0, Length:8, Protocol:8, Data:Length/binary, Rest/binary>>) ->
    IPA = case Protocol of
        ?IPA_PROT_IPA ->
            decode_ipa(Data);
        ?IPA_PROT_SCCP ->
            decode_sccp(Data)
    end,
    {IPA, Rest}.

%% @private
decode_ipa(Data) ->
    case Data of
        <<?IPA_PING>>   -> #ipa_ping{};
        <<?IPA_PONG>>   -> #ipa_pong{};
        <<?IPA_ID_ACK>> -> #ipa_id_ack{};
        <<?IPA_LCS_MSG, Rest/binary>> ->
            decode_pkt_id(Rest)
    end.

decode_pkt_id(<<SLR:24, Packet_ID:8, Rest/binary>>)->
    #ipa_lcs_msg{slr = SLR, pkt_id = Packet_ID, payload = Rest}.
    
%% @private
decode_sccp(Data) ->
    #ipa_sccp{payload = Data}.

%% ---------------------------------------------------------------------------
%% @doc Encodes a ipaccess packet into a binary.
%% @spec encode(Packet :: #ipaccess{}) -> binary()
%% @end
%% ---------------------------------------------------------------------------
encode(#ipa_ping{})->
    <<0,1,?IPA_PROT_IPA, ?IPA_PING>>;
encode(#ipa_pong{})->
    <<0,1,?IPA_PROT_IPA, ?IPA_PONG>>;
encode(#ipa_id_ack{})->
    <<0,1,?IPA_PROT_IPA, ?IPA_ID_ACK>>;
encode(#ipa_lcs_msg{payload = Data})->
    Length = size(Data),
    <<0, Length, ?IPA_PROT_IPA, Data/binary>>;
encode(#ipa_sccp{payload = Data})->       
    Length = size(Data),
    <<0, Length, ?IPA_PROT_SCCP, Data/binary>>.
