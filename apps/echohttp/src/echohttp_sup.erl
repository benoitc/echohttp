
-module(echohttp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Port} = application:get_env(echohttp, port),
    {ok, BindAddress} = application:get_env(echohttp, ip),

    ParsedIp = case BindAddress of
        any ->
            any;
        Ip when is_tuple(Ip) ->
            Ip;
        Ip when is_list(Ip) ->
            {ok, IpTuple} = inet_parse:address(Ip),
            IpTuple
    end,


    HTTPd_Dispatch = [
     {'_', [{[<<"echo">>,  '...'], echohttp_echo_handler, []},
            {[], cowboy_http_static, [
				{directory, {priv_dir, echohttp, []}},
                {file, <<"index.html">>},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			]}]}
    ],
    HTTPd = cowboy:child_spec(echohttpd,
                              10, cowboy_tcp_transport, [{port, Port},
                                                         {ip, ParsedIp}],
                              cowboy_http_protocol, [{dispatch, HTTPd_Dispatch}]),


    Children = [HTTPd],
    RestartStrategy = {one_for_one, 1, 60},
    {ok, { RestartStrategy, Children} }.

