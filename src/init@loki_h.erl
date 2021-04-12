-module(init@loki_h).

-behaviour(gen_server).

-export([adding_handler/1, removing_handler/1, log/2]).

%%% -- public control --
-export([start_link/2, stop/0]).

%%% -- gen_server callbacks --
-export([init/1, 
         handle_call/3, 
         handle_cast/2,
         terminate/2]).

%%% -- public control ----------------------------------------------------------
start_link(Args, SysOps) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, SysOps).

stop() ->
    gen_server:stop(?MODULE).

%%% -- logger callbacks --------------------------------------------------------
adding_handler(Config) ->
    {ok, Config}.

removing_handler(#{}) ->
    ok.

log(LogEvent, _Cfg) ->
    loki_h:log(LogEvent).

%%% -- gen_server callbacks ----------------------------------------------------
init(#{}=Cfg) ->
    ok = logger:add_handler(?MODULE, ?MODULE, Cfg),
    {ok, Cfg}.

handle_call(_, _, #{}) ->
    throw(never_match).

handle_cast(never_match, #{}) ->
    throw(never_match).

terminate(_Reason, #{}) ->
    ok = logger:remove_handler(?MODULE),
    ok.