#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ebin deps/mochijson2/ebin

-include("include/skydrive.hrl").

enumerate_directory(Token, FolderId) ->
    Files = skydrive:get_directory_files(Token, FolderId),
    [process_file(Token, File) || File <- Files].

process_file(Token, #skydrive_file{id = Id, name = Name, type = Type}) ->
    case Type of
	<<"file">> ->
	    print_file(Id, Name);
	<<"folder">> ->
	    process_directory(Token, Id, Name);
	<<"album">> ->
	    process_directory(Token, Id, Name);
	<<"photo">> ->
	    print_file(Id, Name)
    end.

print_file(Id, Name) ->
    io:format("file name ~p, id ~p~n", [Name, Id]).

process_directory(Token, Id, Name) ->
    io:format("folder name ~p, id ~p~n", [Name, Id]),
    enumerate_directory(Token, Id).

main([ClientId, ClientSecret]) ->
    ssl:start(),
    inets:start(),
    Scopes = ["wl.skydrive", "wl.offline_access", "wl.signin", "wl.skydrive_update"],
    AuthUrl = skydrive_util:auth_url(ClientId, Scopes, skydrive_util:desktop_url()),
    RedirectedUrl = io:get_line(io_lib:format("copy this URL into the browser's address line: ~s~ncopy URL from address line after login and redirect here: ", [AuthUrl])),
    {ok, AuthCode} = skydrive_util:auth_parse_code(RedirectedUrl),
    io:format("auth code: ~s~n", [AuthCode]),
    {ok, TokenData} = skydrive:get_token(ClientId, ClientSecret, skydrive_util:desktop_url(), AuthCode),
    io:format("auth token: ~p~n", [TokenData]),
    #skydrive_token{access_token = AccessToken} = TokenData,
    enumerate_directory(AccessToken, "me/skydrive").
