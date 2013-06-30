-module(skydrive_util).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([auth_url/3, auth_parse_code/1, token_req_url/0, token_req_body/4]).

-define(AUTH_HOST, "https://login.live.com").
-define(AUTH_PATH_BASE, "oauth20_authorize.srf").
-define(AUTH_TOKEN_PATH, "oauth20_token.srf").

auth_url(ClientId, Scopes, RedirectUrl) ->
    ScopeArg = string:join(Scopes, " "),
    Url = io_lib:format("~s/~s?client_id=~s&scope=~s&response_type=code&redirect_uri=~s",
			[?AUTH_HOST, ?AUTH_PATH_BASE, edoc_lib:escape_uri(ClientId), edoc_lib:escape_uri(ScopeArg), edoc_lib:escape_uri(RedirectUrl)]),
    lists:flatten(Url).

auth_parse_code(RedirectedUrl) ->
    {ok, {_, _, _, _, _, Query}} = http_uri:parse(RedirectedUrl),
    Params = url_query_string:parse(Query),
    {ok, proplists:get_value(<<"code">>, Params)}.

token_req_url() ->
    lists:flatten(io_lib:format("~s/~s", [?AUTH_HOST, ?AUTH_TOKEN_PATH])).

token_req_body(ClientId, ClientSecret, RedirectUrl, AuthCode) ->
    TokenReqBody = io_lib:format("client_id=~s&redirect_uri=~s&client_secret=~s&code=~s&grant_type=authorization_code",
				 [edoc_lib:escape_uri(ClientId), edoc_lib:escape_uri(RedirectUrl), edoc_lib:escape_uri(ClientSecret), edoc_lib:escape_uri(AuthCode)]),
    iolist_to_binary(TokenReqBody).
