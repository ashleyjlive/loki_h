%%%-------------------------------------------------------------------
%% @doc loki_h public API
%% @end
%%%-------------------------------------------------------------------

-module(app@loki_h).

-behaviour(application).

%%% -- public control --
-export([start/2, stop/1]).

%%% -- public interface --
-export([start_args/0,
         start_args/1, 
         start_arg/1,
         start_arg/2]).

-include("common.hrl").

%%% -- public control ----------------------------------------------------------
start(_StartType, _StartArgs) ->
    sup@loki_h:start_link().

stop(_State) ->
    ok.

%%% -- public interface --------------------------------------------------------

-spec start_args() -> loki_h:cfg_t().
start_args() ->
    Args = #{} = application:get_env(?APP, start_args, #{}),
    ArgKys = [config, formatter, 
              filters, sys_ops],
    lists:foldl(
        fun(Ky, A) ->
            A#{Ky => start_arg(Ky, Args)}
        end, #{}, ArgKys).

start_args([_|_]=RqArgs) ->
    Args = #{} = application:get_env(?APP, start_args, #{}),
    [start_arg(RqArg, Args) || RqArg <- RqArgs].

start_arg(K) ->
    start_arg(K, application:get_env(?APP, start_args, #{})).

-spec start_arg(config, map())    -> loki_h:cfg_args_t();
               (formatter, map()) -> loki_h:cfg_fmtr_t();
               (filters, map())   -> loki_h:cfg_fltrs_t();
               (sys_ops, map())   -> loki_h:cfg_sys_ops_t().
%%%
%%  @doc Retrieves the start arguments for the loki_h application.
%%
start_arg(config, #{}=Args0) ->
    DfltCfg = 
        #{failure_strategy => crash,
          max_bytes => 1000000,
          max_count => 1000,
          interval => 5000},
    Cfg0 = maps:get(config, Args0, #{}),
    Cfg = maps:merge(DfltCfg, Cfg0),
    true = is_map_key(job, Cfg),
    Cfg;
start_arg(formatter, #{}=Args0) ->
    DfltFmtr =
        {logger_formatter, 
            #{single_line => false, 
              legacy_header => false}},
    maps:get(formatter, Args0, DfltFmtr);
start_arg(filters, #{}=Args0) ->
    maps:get(filters, Args0, []);
start_arg(sys_ops, #{}=Args) ->
    maps:get(sys_ops, Args, []).