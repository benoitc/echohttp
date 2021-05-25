
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
    Specs = [#{id=>listener,
               start => {echohttp_listener, start_link, []}}],
    SupFlags = #{strategy => one_for_one,
                intensity => 1,
                period => 60},
    {ok, { SupFlags, Specs} }.
