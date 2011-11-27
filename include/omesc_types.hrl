% -------------------------------------------------------------

% -------------------------------------------------------------

-record(omesc_imei, {tac :: #omesc_tac{},
                     snr :: #omesc_snr{},
                     cdsd :: binary()   }).

-record(omesc_imeisv, {tac :: #omesc_tac{},
                       snr :: #omesc_snr{},
                       svn :: binary()    }).

-record(omesc_tac, {number :: binary()}).

-record(omesc_snr, {number :: binary()}).

% International Mobile Subscriber Identifier.
-record(omesc_imsi, {mnc  :: #omesc_mnc{},
                     mcc  :: #omesc_mcc{},
                     msin :: binary()   }).

% Mobile Network Code.
-record(omesc_mnc, {number :: binary()}).

% Mobile Country Code.
-record(omesc_mcc, {number :: binary()}).

% Temporary Mobile Subscriber Identifier.
-record(omesc_tmsi, {number :: binary()}).

% -------------------------------------------------------------

% -------------------------------------------------------------
