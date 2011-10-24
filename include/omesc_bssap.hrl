% ----------------------------------------------------------------------------
% File: omesc_bssap.hrl
% Info: Contains definitions of internal representation of bssap protocol.
% ----------------------------------------------------------------------------

% Reset ----------------------------------------------------------------------
% gsm.3gpp.08.08/3.2.1.23
-record(bssmap_reset,     {cause :: integer()}).
% gsm.3gpp.08.08/3.2.1.24
-record(bssmap_reset_ack, {}).
