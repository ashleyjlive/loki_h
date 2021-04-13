%%%-------------------------------------------------------------------
%% @doc loki_h top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(sup@loki_h).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

init(undefined) ->
    LokiH = #{sys_ops := SysOps} = app@loki_h:start_args(),
    SupSpec = 
        #{strategy => one_for_one,
          intensity => 10,
          period => 60},
    InitChild =
        #{id => init,
           start => {init@loki_h, start_link, [LokiH, SysOps]},
           restart => permanent, % permanent | transient | temporary
           shutdown => 2000,
           type => worker, % worker | supervisor
           modules => [init@loki_h]},
    Receivers =
        [#{id => {Sched, loki_h},
           start => {loki_h, start_link, [Sched, LokiH, SysOps]},
           restart => permanent, % permanent | transient | temporary
           shutdown => 2000,
           type => worker, % worker | supervisor
           modules => [loki_h]} 
         || Sched <- lists:seq(1, erlang:system_info(schedulers_online))],
    {ok, {SupSpec, Receivers ++ [InitChild]}}.