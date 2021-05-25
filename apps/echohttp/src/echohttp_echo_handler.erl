-module(echohttp_echo_handler).

-export([init/2, terminate/2]).

-define(MAX_BODY_SIZE, 4294967296).

init(Req0, State) ->
    {ok, JsonObj, Req} = build_echo(Req0),

    JsonBody0 = jsx:encode(JsonObj, [space, {indent, 2}]),
    JsonBody = binary:replace(JsonBody0, <<"\\">>, <<"">>, [global]),

    RespHeaders = #{<<"Content-Type">> => <<"application/json">>},

    Req = cowboy_req:reply(200, RespHeaders, JsonBody, Req),
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

build_echo(Req) ->
    #{method := Method
     ,version := Version
     ,scheme := Scheme
     ,host := Host
     ,port := Port
     ,path := Path
     ,qs := QS
     ,headers := Headers} = Req,

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

    CType = cowboy_req:parse_header(<<"content-type">>, Req),
    {ok, ReqBody, NewReq} =  cowboy_req:read_body(Req, #{length => ?MAX_BODY_SIZE}),

    {JsonReq, Form} = 
        case CType of
            {<<"application">>, <<"json">>, _} ->
                {jsx:decode(ReqBody), #{}};
            {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
                DecodedForm = cow_qs:parse_qs(ReqBody),
                {#{}, maps:from_list(DecodedForm)};
            _ ->
               {#{}, #{}}
        end,

    QSObj = 
        case cowboy_req:parse_qs(Req) of
            undefined -> #{};
            QSVals -> maps:from_list(QSVals)
        end,

    JsonObj = #{<<"version">> => Version,
                <<"method">> => Method,
                <<"scheme">> => Scheme,
                <<"host">> => Host,
                <<"port">> => Port,
                <<"raw_path">> => RawPath,
                <<"decode_path">> => #{<<"path">> => Path,
                                       <<"query_string">> => QS,
                                       <<"qs">> => QSObj,
                                       <<"path_info">> => cowboy_req:path_info(Req)},
                <<"headers">> => Headers,
                <<"body">> => ReqBody,
                <<"json">> => JsonReq,
                <<"form">> => Form},

    io:format("json obj ~p~n", [JsonObj]),
    {ok, JsonObj, NewReq}.
