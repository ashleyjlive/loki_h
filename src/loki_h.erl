-module(loki_h).

%%% -- public control --
-export([start_link/3,  
         stop/1,
         enter_link/2]).

%%% -- public interface --
-export([log/1]).

%%% -- private preprocess --
-include("../priv/status_codes.hrl").
-include("common.hrl").
-define(SUB_PATH, "loki/api/v1/push").
-include("loki_h.types.hrl").

-export_type([cfg_t/0,
              cfg_fmtr_t/0,
              cfg_fltrs_t/0,
              cfg_sys_ops_t/0,
              cfg_trgt_t/0,
              cfg_args_t/0,
              cfg_k_t/0,
              auth_t/0]).

-record(loki_target_r, {scheme :: http | https,
                        host :: unicode:chardata(),
                        port :: pos_integer(),
                        path :: unicode:chardata(),
                        url :: uri_string:uri_string(),
                        auth :: auth_t()}).

-record(loki_cfg_r, {failure_strategy :: crash | drop,
                     max_bytes :: pos_integer(),
                     max_count :: pos_integer(),
                     target :: #loki_target_r{},
                     format_mod :: module(),
                     format_args :: logger_formatter:config(),
                     job :: binary(),
                     interval :: pos_integer()}).

-type auth_t() :: {basic, Usr :: unicode:chardata(), 
                          Pwd :: unicode:chardata()}  |
                  {bearer, Tok :: unicode:chardata()} | undefined.

%%% -- public control ----------------------------------------------------------

start_link(Scheduler, #{}=Args, _SysOps) when is_integer(Scheduler) ->
    {ok, proc_lib:spawn_link(?MODULE, enter_link, [Scheduler, Args])}.

stop(Scheduler) when is_integer(Scheduler) ->
    proc_lib:stop(to_reg_name(Scheduler)).

%%% -- public interface --------------------------------------------------------

log(Event) ->
    to_reg_name(erlang:system_info(scheduler_id)) ! {?MODULE, log, Event}.

%%% -- internal ----------------------------------------------------------------

-spec enter_link(pos_integer(), _) -> no_return().
enter_link(Sched, #{formatter := Fmtr, config := #{}=Cfg}) ->
    register(to_reg_name(Sched), self()),
    do_loop(to_record(Fmtr, Cfg)).

-spec do_loop(#loki_cfg_r{}) -> no_return().
do_loop(#loki_cfg_r{}=Cfg) ->
    case receive_loop(Cfg, 0, 0, []) of
        [] ->
            do_loop(Cfg);
        Other ->
            Fmt = zlib:gzip(jsone:encode({[{streams, Other}]})),
            process_fmtd(Fmt, Cfg)
    end.

-spec process_fmtd(iodata(), #loki_cfg_r{}) -> no_return().
process_fmtd(Fmt, #loki_cfg_r{target = #loki_target_r{}=Trgt}=Cfg) ->
    case request(Trgt, Fmt) of
        {ok, {{_, ?'204_No_Content', _}, _, _}} ->
            do_loop(Cfg);
        {ok, {{_, C, _}, _, Err}} when 
            ?is_status_code_bad(C), Cfg#loki_cfg_r.failure_strategy =:= drop
        ->
            logger:warning(
                [{tag, logging},
                 {desc, "Failure to upload log data to Loki."},
                 {data, {C, Err}}]),
            do_loop(Cfg);
        {error, Err} when Cfg#loki_cfg_r.failure_strategy =:= drop ->
            logger:warning(
                [{tag, logging},
                 {desc, "Failure to upload log data to Loki."},
                 {data, Err}]),
            do_loop(Cfg)
    end.

receive_loop(#loki_cfg_r{max_count = Max}, _B, C, Acc) when C >= Max ->
    lists:reverse(Acc);
receive_loop(#loki_cfg_r{max_bytes = Max}, B, _C, Acc) when B >= Max ->
    lists:reverse(Acc);
receive_loop(#loki_cfg_r{format_mod = FmtMod, 
                         format_args = FmtArgs}=Cfg, B, C, Acc) 
    ->
    receive 
        {?MODULE, log, Log} ->
            #{level := Lvl, meta := #{time := MicroSeconds}}=Log,
            Msg = iolist_to_binary(FmtMod:format(Log, FmtArgs)),
            % TODO: ^ May not be needed
            NanoSeconds = integer_to_binary(MicroSeconds * 1000),
            Acc1 =
                [{[{stream, {[{instance, node()},
                              {timestamp, NanoSeconds},
                              {level, Lvl},
                              {job, Cfg#loki_cfg_r.job}]}},
                   {values, [[NanoSeconds, Msg]]}]}|Acc],
            receive_loop(Cfg, B + byte_size(Msg), C + 1, Acc1);
        Other ->
            error(Other)
    after Cfg#loki_cfg_r.interval ->
        lists:reverse(Acc)
    end.

to_record({M, A}, #{job := Job}=Cfg) ->
    #loki_cfg_r{
        failure_strategy = map_get(failure_strategy, Cfg),
        interval = map_get(interval, Cfg),
        target = to_trgt_record(map_get(target, Cfg)),
        max_bytes = map_get(max_bytes, Cfg),
        max_count = map_get(max_count, Cfg),
        format_mod = M,
        format_args = A,
        job = if is_list(Job) -> list_to_binary(Job); true -> Job end}.

to_trgt_record([_|_]=Url) ->
    to_trgt_record(#{url => Url});
to_trgt_record(<<_, _/binary>>=Url) ->
    to_trgt_record(#{url => Url});
to_trgt_record(#{url := Url}=Cfg) when is_list(Url); is_binary(Url) ->
    #{} = Prsd = uri_string:parse(Url),
    RawCfg = 
        maps:with(
            [fragment, host, path, port, query, scheme, userinfo], Cfg),
    Cfg1 = maps:merge(Prsd, RawCfg),
    to_trgt_record(Cfg1);
to_trgt_record(#{scheme := "http"}=Cfg) ->
    to_trgt_record(Cfg#{scheme := http});
to_trgt_record(#{scheme := "https"}=Cfg) ->
    to_trgt_record(Cfg#{scheme := https});
to_trgt_record(#{scheme := <<"http">>}=Cfg) ->
    to_trgt_record(Cfg#{scheme := http});
to_trgt_record(#{scheme := <<"https">>}=Cfg) ->
    to_trgt_record(Cfg#{scheme := https});
to_trgt_record(#{scheme := https, port := undefined}=Cfg) ->
    to_trgt_record(Cfg#{port => 443});
to_trgt_record(#{scheme := http, port := undefined}=Cfg) ->
    to_trgt_record(Cfg#{port => 80});
to_trgt_record(#{}=Cfg) ->
    #loki_target_r{url = recompose(Cfg),
                   scheme = map_get(scheme, Cfg),
                   host = map_get(host, Cfg),
                   port = map_get(port, Cfg),
                   path = maps:get(path, Cfg, ""),
                   auth = maps:get(auth, Cfg, undefined)}.

-spec recompose(cfg_trgt_t()) -> uri_string:uri_string().
%%%
%%  @doc Converts the raw Loki configuration target to a URI string.
%%
recompose(#{}=Cfg) ->
    case uri_string:recompose(
            #{scheme => atom_to_list(map_get(scheme, Cfg)),
              host => map_get(host, Cfg),
              port => map_get(port, Cfg),
              path => filename:join(maps:get(path, Cfg, ""), ?SUB_PATH)})
    of
        [_|_]=V ->
            V;
        <<_, _/binary>>=V ->
            V
        % NB: Don't allow error response.
    end.

request(#loki_target_r{url = Url}=Trgt, Bdy) ->
    httpc:request(
        post, {Url, append_auth_header(Trgt, [{"content-encoding", "gzip"}]), 
        "application/json", Bdy}, [], []).

-spec append_auth_header(#loki_target_r{}, httpc:headers()) -> httpc:headers().
%%%
%%  @doc Appends any authorization headers if configured.
%%
append_auth_header(#loki_target_r{auth = undefined}, Hdrs) ->
    Hdrs;
append_auth_header(#loki_target_r{auth = {basic, Usr, Pwd}}, Hdrs) ->
    [{"authorization", "Basic " ++ base64:encode(Usr ++ [$:|Pwd])}|Hdrs];
append_auth_header(#loki_target_r{auth = {bearer, Tok}}, Hdrs) ->
    [{"authorization", "Bearer " ++ Tok}|Hdrs].

-spec to_reg_name(pos_integer()) -> atom().
%%%
%%  @doc Returns the log receiver for the associated scheduler.
%%
to_reg_name(1) ->
    'recv@loki_h_1';
to_reg_name(2) ->
    'recv@loki_h_2';
to_reg_name(3) ->
    'recv@loki_h_3';
to_reg_name(4) ->
    'recv@loki_h_4';
to_reg_name(5) ->
    'recv@loki_h_5';
to_reg_name(6) ->
    'recv@loki_h_6';
to_reg_name(7) ->
    'recv@loki_h_7';
to_reg_name(8) ->
    'recv@loki_h_8';
to_reg_name(9) ->
    'recv@loki_h_9';
to_reg_name(10) ->
    'recv@loki_h_10';
to_reg_name(11) ->
    'recv@loki_h_11';
to_reg_name(12) ->
    'recv@loki_h_12';
to_reg_name(13) ->
    'recv@loki_h_13';
to_reg_name(14) ->
    'recv@loki_h_14';
to_reg_name(15) ->
    'recv@loki_h_15';
to_reg_name(16) ->
    'recv@loki_h_16';
to_reg_name(N) when N =< 1024 ->
    list_to_atom("recv@loki_h_" ++ N).