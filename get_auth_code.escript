#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ebin deps/mochijson2/ebin

-include("include/skydrive.hrl").

main([ClientId, ClientSecret]) ->
    ssl:start(),
    inets:start(),
    Scopes = ["wl.skydrive", "wl.offline_access", "wl.signin", "wl.skydrive_update"],
    AuthUrl = skydrive_util:auth_url(ClientId, Scopes, skydrive_util:desktop_url()),
    RedirectedUrl = io:get_line(io_lib:format("copy this URL into the browser's address line: ~s~ncopy URL from address line after login and redirect here:", [AuthUrl])),
    {ok, AuthCode} = skydrive_util:auth_parse_code(RedirectedUrl),
    io:format("auth code: ~s~n", [AuthCode]),
    {ok, TokenData} = skydrive:get_token(ClientId, ClientSecret, skydrive_util:desktop_url(), AuthCode),
    io:format("auth token: ~p~n", [TokenData]),
    #skydrive_token{access_token = AccessToken} = TokenData,
    FilesResp = skydrive:request_json(AccessToken, "me/skydrive/files"),
    io:format("~p~n", [FilesResp]).
