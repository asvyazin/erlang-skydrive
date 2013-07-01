-module(skydrive).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([get_token/4, request/2]).

-include("skydrive.hrl").

-define(API_BASE, "https://apis.live.net/v5.0").

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

request(Token, Query) ->
    RequestUrl = lists:flatten(io_lib:format("~s/~s?access_token=~s", [?API_BASE, Query, edoc_lib:escape_uri(Token)])),
    httpc:request(RequestUrl).
