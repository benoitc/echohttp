-module(echohttp_echo_handler).

-export([init/3, handle/2, terminate/2]).

-include_lib("cowboy/include/http.hrl").

-define(MAX_BODY_SIZE, 4294967296).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {ok, JsonObj, Req} = build_echo(Req0),

    JsonBody0 = jiffy:encode(JsonObj, [uescape, pretty]),
    JsonBody = binary:replace(JsonBody0, <<"\\">>, <<"">>, [global]),

    RespHeaders = [{<<"Content-Type">>, <<"application/json">>}],

    {ok, Reply} = cowboy_http_req:reply(200, RespHeaders, JsonBody, Req),
    {ok, Reply, State}.



terminate(_Req, _State) ->
    ok.

build_echo(Req) ->
    #http_req{transport=Transport,
              method=Method,
              version=Version,
              raw_path=Path,
              path_info=PathInfo,
              raw_qs=QS,
              qs_vals=QSVals,
              headers=Headers,
              raw_host=Host,
              port=Port}= Req,

    %%{PeerAddr, _} = cowboy_http_req:peer_addr(Req),

    %% version str
    {Min, Maj} = Version,
    VersionStr = iolist_to_binary(["HTTP/", integer_to_list(Min), ".",
                                integer_to_list(Maj)]),

    %% fix host header
    DefaultPort = default_port(Transport:name()),
    Host1 = case Port of
        DefaultPort ->
            binary_to_list(Host);
        _ ->
            %% fix raw host
            binary_to_list(Host) ++ ":" ++ integer_to_list(Port)
    end,

    Headers1 = lists:reverse(lists:foldl(fun
                ({'Host'=K, _V}, Acc) ->
                    [{K, list_to_binary(Host1)} | Acc];
                (KV, Acc) ->
                    [KV | Acc]
                end, [], Headers)),


    %% fix raw path
    Path1 = case Path of
        <<>> ->
            <<"/">>;
        _ ->
            Path
    end,
    RawPath = case QS of
        <<>> ->
            Path1;
        _ ->
            << Path1/binary, "?", QS/binary >>
    end,

    {CType, Req1} = cowboy_http_req:parse_header('Content-Type', Req),
    {ok, ReqBody, Req2} = cowboy_http_req:body(?MAX_BODY_SIZE, Req1),
    {JsonReq, Form} = case CType of
        {<<"application">>, <<"json">>, _} ->
            {jiffy:decode(ReqBody), {[]}};
        {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
            URLDecode = fun cowboy_http:urldecode/1,
            DecodedForm = cowboy_http:x_www_form_urlencoded(ReqBody,
                                                            URLDecode),
            {{[]}, {DecodedForm}};
        _ ->
            {{[]},{[]}}
    end,

    QSObj = case QSVals of
        undefined -> {[]};
        _ -> {QSVals}
    end,


    JsonObj = {[
                {<<"version">>, VersionStr},
                {<<"method">>, Method},
                {<<"path">>, RawPath},
                {<<"decode_path">>, {[
                            {<<"path">>, Path},
                            {<<"query_string">>, QS},
                            {<<"qs">>, QSObj},
                            {<<"path_info">>, PathInfo}]}
                },
                {<<"headers">>, {Headers1}},
                {<<"body">>, ReqBody},
                {<<"json">>, JsonReq},
                {<<"form">>, Form}]},

    io:format("json obj ~p~n", [JsonObj]),
    {ok, JsonObj, Req2}.

-spec default_port(atom()) -> 80 | 443.
default_port(ssl) -> 443;
default_port(_) -> 80.
