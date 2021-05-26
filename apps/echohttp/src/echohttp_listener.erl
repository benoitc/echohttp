-module(echohttp_listener).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,terminate/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

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
    Dispatch = cowboy_router:compile([
        {'_', [{"/echo/[...]", echohttp_echo_handler, []}
              ,{"/ip", echohttp_ip_handler, []}
              ,{"/test/:action", echohttp_test_handler, []}
              ,{"/test/:action/:param", echohttp_test_handler, []}
              ,{'_', cowboy_static, {priv_file, echohttp, "index.html"}}]
            }]),
    {ok, _} = cowboy:start_clear(echo_http_listener
                                ,[{port, Port}, {ip, ParsedIp}]
                                , #{env => #{dispatch => Dispatch}}),
    {ok, {}}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(echo_http_listener).
