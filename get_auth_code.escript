#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ebin

main([ClientId]) ->
    AuthUrl = skydrive_util:auth_url(ClientId, ["wl.skydrive", "wl.offline_access"], "https://login.live.com/oauth20_desktop.srf"),
    RedirectedUrl = io:get_line(io_lib:format("copy this URL into the browser's address line: ~s~ncopy URL from address line after login and redirect here:", [AuthUrl])),
    {ok, AuthCode} = skydrive_util:parse_auth_code(RedirectedUrl),
    io:format("auth code: ~s~n", [AuthCode]).
