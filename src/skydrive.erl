-module(skydrive).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([get_token/4,request/3,request_json/3,get_directory_files/2,get_root_directory_files/1,get_root_directory_files/2,request_json/2,request/2]).

-include("skydrive.hrl").

get_token(ClientId, ClientSecret, RedirectUrl, AuthCode) ->
    TokenReqBody = skydrive_util:token_req_body(ClientId, ClientSecret, RedirectUrl, AuthCode),
    {ok, {{_, 200, _}, _, TokenRespBody}} = httpc:request(post, {skydrive_util:token_req_url(), [], "application/x-www-form-urlencoded", TokenReqBody}, [], []),
    {struct, TokenJsonData} = mochijson2:decode(TokenRespBody),
    AccessToken = proplists:get_value(<<"access_token">>, TokenJsonData),
    ExpiresIn = proplists:get_value(<<"expires_in">>, TokenJsonData),
    Scope = proplists:get_value(<<"scope">>, TokenJsonData),
    TokenType = proplists:get_value(<<"token_type">>, TokenJsonData),
    RefreshToken = proplists:get_value(<<"refresh_token">>, TokenJsonData),
    {ok, #skydrive_token{access_token = AccessToken, expires_in = ExpiresIn, scope = Scope, token_type = TokenType, refresh_token = RefreshToken}}.

get_root_directory_files(Token) ->
    get_root_directory_files(Token, me).

get_root_directory_files(Token, UserId) ->
    get_directory_files(Token, [UserId, skydrive]).

get_directory_files(Token, FolderId) ->
    {struct, [{<<"data">>, Files}]} = request_json(Token, [FolderId, files]),
    [parse_skydrive_file(File) || File <- Files].

parse_skydrive_file(File) ->
    {struct, Data} = File,
    Id = proplists:get_value(<<"id">>, Data),
    Type = proplists:get_value(<<"type">>, Data),
    Name = proplists:get_value(<<"name">>, Data),
    #skydrive_file{id = Id, type = Type, name = Name}.

request(Token, Path) ->
    request(Token, Path, []).

request(Token, Path, Params) ->
    RequestUrl = skydrive_util:request_url(Token, Path, Params),
    httpc:request(RequestUrl).

request_json(Token, Path) ->
    request_json(Token, Path, []).

request_json(Token, Path, Params) ->
    {ok, {{_, 200, _}, _, Body}} = request(Token, Path, Params),
    mochijson2:decode(Body).
