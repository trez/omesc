-define(debug(Msg),
  io:format("OMESC/Debug/~s:~w> ~s~n", [?MODULE_STRING, ?LINE, Msg])).

-define(debug(Msg, Args),
  io:format("OMESC/Debug/~s:~w> " Msg "~n", [?MODULE_STRING, ?LINE] ++ Args)).
