-module(echohttp_test_handler).

-export([init/2, terminate/2]).

init(Req, State) ->
    Action = cowboy_req:binding(action, Req),
    try
        handle_action(Action, Req, State)
    catch
        _C:_E ->
            io:format("error ~p~n", [_E]),
            reply_bad_request(Req, State)
    end.

handle_action(<<"status">>, Req, State) ->
    handle_status(Req, State);
handle_action(<<"method">>, Req, State) ->
    ReqMethod = string:uppercase(cowboy_req:binding(param, Req)),
    handle_method(ReqMethod, Req, State);
handle_action(<<"cookies">>, Req, State) ->
    handle_cookies(cowboy_req:binding(param, Req), Req, State);
handle_action(<<"redirect">>, Req, State) ->
    handle_redirect(cowboy_req:binding(param, Req), Req, State);
handle_action(_Action, Req, State) ->
    NewReq =  cowboy_req:reply(404, #{}, Req),
    {ok, NewReq, State}.

terminate(_Req, _State) ->
    ok.

handle_status(Req, State) ->
    Status = binary_to_integer(cowboy_req:binding(param, Req)),
    NewReq =  cowboy_req:reply(Status, #{}, Req),
    {ok, NewReq, State}.

handle_method(Method, Req0 = #{ method := Method }, State) ->
    {ok, JsonObj, Req} = echohttp_echo_handler:build_echo_object(Req0),
    JsonBody0 = jsx:encode(JsonObj, [space, {indent, 2}]),
    JsonBody = binary:replace(JsonBody0, <<"\\">>, <<"">>, [global]),
    RespHeaders = #{<<"Content-Type">> => <<"application/json">>},
    NewReq = cowboy_req:reply(200, RespHeaders, JsonBody, Req),
    {ok, NewReq, State};
handle_method(_Method, Req, State) ->
    NewReq =  cowboy_req:reply(405, #{}, Req),
    {ok, NewReq, State}.

handle_cookies(<<"set">>, Req, State) ->
    ParsedQs = cowboy_req:parse_qs(Req),
    NewReq = cowboy_req:reply(200, #{}, set_resp_cookies(ParsedQs, Req)),
    {ok, NewReq, State};
handle_cookies(<<"delete">>, Req0, State) ->
    ParsedQs = maps:from_list(cowboy_req:parse_qs(Req0)),
    Keys = binary:split(maps:get(<<"keys">>, ParsedQs, <<"">>), <<",">>),
    CookieOpts= #{ max_age => 0 },
    NewReq = lists:foldl(
        fun(Key, Req1) ->
            cowboy_req:set_resp_cookie(Key, <<>>, Req1, CookieOpts)
        end,
        Req0, Keys),
    {ok, cowboy_req:reply(200, #{}, NewReq), State};
handle_cookies(undefined, Req, State) ->
    Cookies = cowboy_req:parse_cookies(Req),
    JsonObj = #{ <<"cookies">> => maps:from_list(Cookies)},
    JsonBody = jsx:encode(JsonObj, [space, {indent, 2}]),
    RespHeaders = #{<<"Content-Type">> => <<"application/json">>},
    NewReq = cowboy_req:reply(200, RespHeaders, JsonBody, Req),
    {ok, NewReq, State};
handle_cookies(_, Req, State) ->
    NewReq =  cowboy_req:reply(404, #{}, Req),
    {ok, NewReq, State}.

set_resp_cookies([], Req) ->
    Req;
set_resp_cookies([{Key, Value} | Rest], Req) ->
    set_resp_cookies(Rest, cowboy_req:set_resp_cookie(Key, Value, Req)).


handle_redirect(<<"permanent">>, Req, State) ->
    ParsedQs = maps:from_list(cowboy_req:parse_qs(Req)),
    StatusStr = maps:get(<<"code">>, ParsedQs, <<"301">>),
    Location = location_url(Req, ParsedQs),
    NewReq = 
        case lists:member(StatusStr, [<<"301">>, <<"308">>]) of
            true ->
                cowboy_req:reply(binary_to_integer(StatusStr), #{ <<"Location">> => Location}, Req);
            false ->
                cowboy_req:reply(400, #{}, <<"Bad status code">>, Req)
        end,

    {ok, NewReq, State};
handle_redirect(<<"temporary">>, Req, State) ->
    ParsedQs = maps:from_list(cowboy_req:parse_qs(Req)),
    StatusStr = maps:get(<<"code">>, ParsedQs, <<"302">>),
    Location = location_url(Req, ParsedQs),
    NewReq = 
        case lists:member(StatusStr, [<<"302">>, <<"307">>]) of
            true ->
                cowboy_req:reply(binary_to_integer(StatusStr), #{ <<"Location">> => Location}, Req);
            false ->
                cowboy_req:reply(400, #{}, <<"Bad status code">>, Req)
        end,

    {ok, NewReq, State};
handle_redirect(<<"see-other">>, Req, State) ->
    ParsedQs = maps:from_list(cowboy_req:parse_qs(Req)),
    Location = location_url(Req, ParsedQs),
    NewReq = cowboy_req:reply(303, #{ <<"Location">> => Location}, Location, Req),
    {ok, NewReq, State};
handle_redirect(_, Req, State) ->
    NewReq =  cowboy_req:reply(404, #{}, Req),
    {ok, NewReq, State}.

location_url(Req, ParsedQs) ->
    #{ scheme := Scheme, host := Host, port := Port} = Req,
    DefaultUrl = echohttp_echo_handler:location(Scheme, Host, Port, <<"/test/method/get">>),
    ParsedQs = maps:from_list(cowboy_req:parse_qs(Req)),
    maps:get(<<"url">>, ParsedQs, DefaultUrl).

reply_bad_request(Req, State) ->
    NewReq =  cowboy_req:reply(400, #{}, Req),
    {ok, NewReq, State}.