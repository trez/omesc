
% Debug prints ---------------------------------------------------------------
-ifdef(debug).
-define(debug(Msg),
  io:format("OMESC/Debug/~s:~w> ~s~n", [?MODULE_STRING, ?LINE, Msg])).
-define(debug(Msg, Args),
  io:format("OMESC/Debug/~s:~w> " Msg "~n", [?MODULE_STRING, ?LINE] ++ Args)).
-else.
-define(debug(Msg), true).
-define(debug(Msg, Args), true).
-endif.

% Error prints ---------------------------------------------------------------
-ifdef(error).
-define(error(Msg),
  io:format("OMESC/ERROR/~s:~w> ~s~n", [?MODULE_STRING, ?LINE, Msg])).
-define(error(Msg, Args),
  io:format("OMESC/ERROR/~s:~w> " Msg "~n", [?MODULE_STRING, ?LINE] ++ Args)).
-else.
-define(error(Msg), true).
-define(error(Msg, Args), true).
-endif.
