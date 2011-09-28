-record(ipa_ping, {}).
-record(ipa_pong, {}).
-record(ipa_id_ack, {}).
-record(ipa_lcs_msg,
        {slr,
         pkt_id ,
         payload :: binary()}).
-record(ipa_sccp, {payload :: binary()}).

-define(IPA_PROT_IPA,  16#FE).
-define(IPA_PROT_SCCP, 16#FD).

-define(IPA_PING, 0).
-define(IPA_PONG, 1).
-define(IPA_ID_ACK, 6).
-define(IPA_LCS_MSG, 9).

-define(TA_REQ_IND, 7).
-define(LOC_RESP, 8).
-define(LOC_REQ, 9).
