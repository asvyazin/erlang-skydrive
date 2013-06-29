-module(skydrive_util).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([auth_url/3, parse_auth_code/1]).

-define(AUTH_HOST, "https://login.live.com").
-define(AUTH_PATH_BASE, "oauth20_authorize.srf").

auth_url(ClientId, Scopes, RedirectUrl) ->
    ScopeArg = string:join(Scopes, " "),
    io_lib:format("~s/~s?client_id=~s&scope=~s&response_type=code&redirect_uri=~s",
		  [?AUTH_HOST, ?AUTH_PATH_BASE, edoc_lib:escape_uri(ClientId), edoc_lib:escape_uri(ScopeArg), edoc_lib:escape_uri(RedirectUrl)]).

parse_auth_code(RedirectedUrl) ->
    {ok, {_, _, _, _, _, Query}} = http_uri:parse(RedirectedUrl),
    Params = url_query_string:parse(Query),
    {ok, proplists:get_value(<<"code">>, Params)}.
