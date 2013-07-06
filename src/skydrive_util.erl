-module(skydrive_util).
-author('Alexander Svyazin <guybrush@live.ru>').

-export([auth_url/3,auth_parse_code/1,token_req_url/0,token_req_body/4,token_refresh_url/0,token_refresh_body/4,desktop_url/0,request_url/3,request_url/2]).

-define(AUTH_HOST, "https://login.live.com").
-define(AUTH_PATH_BASE, "oauth20_authorize.srf").
-define(AUTH_TOKEN_PATH, "oauth20_token.srf").
-define(AUTH_DESKTOP_PATH, "oauth20_desktop.srf").
-define(API_BASE, "https://apis.live.net/v5.0").

format_qs(Params) ->
    FParams = [format_param(P) || P <- Params],
    string:join(FParams, "&").

format_param({Name, Value}) ->
    io_lib:format("~s=~s", [Name, escape_uri(Value)]).

escape_uri(Value) when is_atom(Value) -> Value;
escape_uri(Value) when is_binary(Value) -> escape_uri(binary_to_list(Value));
escape_uri(Value) -> edoc_lib:escape_uri(Value).

auth_url(ClientId, Scopes, RedirectUrl) ->
    Query = format_qs([{client_id, ClientId},
		       {scope, string:join(Scopes, " ")},
		       {redirect_uri, RedirectUrl},
		       {response_type, code}]),
    Url = io_lib:format("~s/~s?~s", [?AUTH_HOST, ?AUTH_PATH_BASE, Query]),
    lists:flatten(Url).

auth_parse_code(RedirectedUrl) ->
    {ok, {_, _, _, _, _, Query}} = http_uri:parse(RedirectedUrl),
    Params = url_query_string:parse(Query),
    {ok, proplists:get_value(<<"code">>, Params)}.

token_req_url() ->
    lists:flatten(io_lib:format("~s/~s", [?AUTH_HOST, ?AUTH_TOKEN_PATH])).

token_req_body(ClientId, ClientSecret, RedirectUrl, AuthCode) ->
    TokenReqBody = format_qs([{client_id, ClientId},
			      {redirect_uri, RedirectUrl},
			      {client_secret, ClientSecret},
			      {grant_type, authorization_code},
			      {code, AuthCode}]),
    iolist_to_binary(TokenReqBody).

token_refresh_url() ->
    token_req_url().

token_refresh_body(ClientId, ClientSecret, RedirectUrl, RefreshToken) ->
    TokenReqBody = format_qs([{client_id, ClientId},
			      {redirect_uri, RedirectUrl},
			      {client_secret, ClientSecret},
			      {grant_type, refresh_token},
			      {refresh_token, RefreshToken}]),
    iolist_to_binary(TokenReqBody).

desktop_url() ->
    lists:flatten(io_lib:format("~s/~s", [?AUTH_HOST, ?AUTH_DESKTOP_PATH])).

request_url(Token, Path, Params) ->
    request_url(Path, [{access_token, Token} | Params]).

request_url(Path, Params) ->
    lists:flatten(io_lib:format("~s/~s?~s", [?API_BASE, format_path(Path), format_qs(Params)])).

format_path(Path) when is_binary(Path) or is_atom(Path) or is_integer(Path) ->
    io_lib:format("~s", [Path]);
format_path(Path) when is_list(Path) ->
    string:join([format_path(Part) || Part <- Path], "/").
