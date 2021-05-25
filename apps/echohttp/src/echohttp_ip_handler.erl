-module(echohttp_ip_handler).

-export([init/2, terminate/2]).

init(Req, State) ->
    #{ peer := {PeerIP, _} } = Req,
    %% we try to get the IP from the X-Forwarded-For heade
    %% https://en.wikipedia.org/wiki/X-Forwarded-For
    IP = 
        case cowboy_req:parse_header(<<"x-forwarded-for">>, Req) of
            [IP0 | _] -> IP0;
            _ ->
                list_to_binary(inet:ntoa(PeerIP))
        end,
    ParsedQs = maps:from_list(cowboy_req:parse_qs(Req)),
    {RespHeaders, RespBody} = 
        case maps:find(<<"format">>, ParsedQs) of
            {ok, <<"json">>} ->
                {#{<<"Content-Type">> => <<"application/json">>},
                jsx:encode(#{ <<"ip">> => IP })};
            {ok, <<"jsonp">>} ->
                Cb = 
                    case maps:find(<<"callback">>, ParsedQs) of
                        {ok, Name} -> Name;
                        error -> <<"callback">>
                    end,
                JsonBody = #{ <<"ip">> => IP },
                Body = << Cb/binary, "(", (jsx:encode(JsonBody))/binary, ");" >>,
                {#{<<"Content-Type">> => <<"application/javascript">>},
                Body};
            _ ->
                {#{<<"Content-Type">> => <<"text/plain">>}, IP}
        end,
    NewReq = cowboy_req:reply(200, RespHeaders, RespBody, Req),
    {ok, NewReq, State}.

terminate(_Req, _State) ->
    ok.
