-module(skydrive).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([get_token/4, request/2]).

-define(API_BASE, "https://apis.live.net/v5.0").

get_token(ClientId, ClientSecret, RedirectUrl, AuthCode) ->
    TokenReqBody = skydrive_util:token_req_body(ClientId, ClientSecret, RedirectUrl, AuthCode),
    {ok, {{_, 200, _}, _, TokenRespBody}} = httpc:request(post, {skydrive_util:token_req_url(), [], "application/x-www-form-urlencoded", TokenReqBody}, [], []),
    TokenRespJson = mochijson2:decode(TokenRespBody),
    io:format("~p~n", [TokenRespJson]).

request(Token, Query) ->
    RequestUrl = lists:flatten(io_lib:format("~s/~s?access_token=~s", [?API_BASE, Query, edoc_lib:escape_uri(Token)])),
    httpc:request(RequestUrl).
