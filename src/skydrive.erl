-module(skydrive).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([get_token/4,request/3,request_json/3,get_directory_files/2,get_root_directory_files/1,get_root_directory_files/2,request_json/2,request/2]).

-include("skydrive.hrl").

get_token(ClientId, ClientSecret, RedirectUrl, AuthCode) ->
    TokenReqBody = skydrive_util:token_req_body(ClientId, ClientSecret, RedirectUrl, AuthCode),
    {ok, {{_, 200, _}, _, TokenRespBody}} = httpc:request(post, {skydrive_util:token_req_url(), [], "application/x-www-form-urlencoded", TokenReqBody}, [], [{body_format, binary}]),
    #{ <<"access_token">> := AccessToken
     , <<"expires_in">> := ExpiresIn
     , <<"scope">> := Scope
     , <<"token_type">> := TokenType
     , <<"refresh_token">> := RefreshToken } = jsxn:decode(TokenRespBody),
    {ok, #skydrive_token{access_token = AccessToken, expires_in = ExpiresIn, scope = Scope, token_type = TokenType, refresh_token = RefreshToken}}.

get_root_directory_files(Token) ->
    get_root_directory_files(Token, me).

get_root_directory_files(Token, UserId) ->
    get_directory_files(Token, [UserId, skydrive]).

get_directory_files(Token, FolderId) ->
    #{ <<"data">> := Files } = request_json(Token, [FolderId, files]),
    [parse_skydrive_file(File) || File <- Files].

parse_skydrive_file(File) ->
    #{ <<"id">> := Id
     , <<"type">> := Type
     , <<"name">> := Name } = File,
    #skydrive_file{id = Id, type = Type, name = Name}.

request(Token, Path) ->
    request(Token, Path, []).

request(Token, Path, Params) ->
    RequestUrl = skydrive_util:request_url(Token, Path, Params),
    httpc:request(get, {RequestUrl, []}, [], [{body_format, binary}]).

request_json(Token, Path) ->
    request_json(Token, Path, []).

request_json(Token, Path, Params) ->
    {ok, {{_, 200, _}, _, Body}} = request(Token, Path, Params),
    jsxn:decode(Body).
