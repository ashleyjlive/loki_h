%%%-------------------------------------------------------------------
%% @doc loki_h public API
%% @end
%%%-------------------------------------------------------------------

-module(app@loki_h).

-behaviour(application).

%%% -- public control --
-export([start/2, stop/1]).

%%% -- public interface --
-export([start_args/1, 
         start_arg/1,
         start_arg/2]).

-include("common.hrl").

%%% -- public control ----------------------------------------------------------
start(_StartType, _StartArgs) ->
    sup@loki_h:start_link().

stop(_State) ->
    ok.

%%% -- public interface --------------------------------------------------------
start_args([_|_]=RqArgs) ->
    Args = #{} = application:get_env(?APP, start_args, #{}),
    [start_arg(RqArg, Args) || RqArg <- RqArgs].

start_arg(K) ->
    start_arg(K, application:get_env(?APP, start_args, #{})).

%% internal functions
start_arg(config, #{}=Args0) ->
    DfltCfg = 
        #{job => "default", 
          target => undefined,
          max_bytes => 1000000,
          max_count => 1000,
          interval => 5000},
    Cfg0 = maps:get(config, Args0, #{}),
    maps:merge(DfltCfg, Cfg0);
start_arg(formatter, #{}=Args0) ->
    DfltFmtr =
        {logger_formatter, 
            #{single_line => false, 
              legacy_header => false}},
    maps:get(formatter, Args0, DfltFmtr);
start_arg(sys_ops, #{}=Args) ->
    maps:get(sys_ops, Args, []).