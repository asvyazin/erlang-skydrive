-module(skydrive_util).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([auth_url/3, parse_auth_code/1, get_token/4, get_token_url/0, get_token_body/4, request/2]).

-define(AUTH_HOST, "https://login.live.com").
-define(AUTH_PATH_BASE, "oauth20_authorize.srf").
-define(AUTH_TOKEN_PATH, "oauth20_token.srf").
-define(API_BASE, "https://apis.live.net/v5.0").

auth_url(ClientId, Scopes, RedirectUrl) ->
    ScopeArg = string:join(Scopes, " "),
    Url = io_lib:format("~s/~s?client_id=~s&scope=~s&response_type=code&redirect_uri=~s",
			[?AUTH_HOST, ?AUTH_PATH_BASE, edoc_lib:escape_uri(ClientId), edoc_lib:escape_uri(ScopeArg), edoc_lib:escape_uri(RedirectUrl)]),
    lists:flatten(Url).

parse_auth_code(RedirectedUrl) ->
    {ok, {_, _, _, _, _, Query}} = http_uri:parse(RedirectedUrl),
    Params = url_query_string:parse(Query),
    {ok, proplists:get_value(<<"code">>, Params)}.

get_token_url() ->
    lists:flatten(io_lib:format("~s/~s", [?AUTH_HOST, ?AUTH_TOKEN_PATH])).

get_token_body(ClientId, ClientSecret, RedirectUrl, AuthCode) ->
    TokenReqBody = io_lib:format("client_id=~s&redirect_uri=~s&client_secret=~s&code=~s&grant_type=authorization_code",
				 [edoc_lib:escape_uri(ClientId), edoc_lib:escape_uri(RedirectUrl), edoc_lib:escape_uri(ClientSecret), edoc_lib:escape_uri(AuthCode)]),
    iolist_to_binary(TokenReqBody).

get_token(ClientId, ClientSecret, RedirectUrl, AuthCode) ->
    TokenReqBody = get_token_body(ClientId, ClientSecret, RedirectUrl, AuthCode),
    {ok, {{_, 200, _}, _, TokenRespBody}} = httpc:request(post, {get_token_url(), [], "application/x-www-form-urlencoded", TokenReqBody}, [], []),
    TokenRespJson = mochijson2:decode(TokenRespBody),
    io:format("~p~n", [TokenRespJson]).

request(Token, Query) ->
    RequestUrl = lists:flatten(io_lib:format("~s/~s?access_token=~s", [?API_BASE, Query, edoc_lib:escape_uri(Token)])),
    httpc:request(RequestUrl).
