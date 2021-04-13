-module(loki_h).

%%% -- public control --
-export([start_link/3, stop/0]).

%%% -- public interface --
-export([log/1]).

%%% -- private preprocess --
-define(SUB_PATH, "/loki/api/v1/push").

-record(loki_target_r, {scheme :: http | https,
                        host :: unicode:chardata(),
                        port :: pos_integer(),
                        path :: unicode:chardata(),
                        url :: uri_string:uri_string(),
                        auth :: auth_t()}).

-record(loki_cfg_r, {max_bytes :: pos_integer(),
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
%%% 
start_link(Sched, Args, _SysOps) ->
    {ok, proc_lib:spawn_link(fun() -> enter_link(Sched, Args) end)}.

stop() ->
    exit(?MODULE, kill).

%%% -- public interface --------------------------------------------------------

log(Event) ->
    to_reg_name(erlang:system_info(scheduler_id)) ! {?MODULE, log, Event}.

%%% -- internal ----------------------------------------------------------------
%%% 
enter_link(Sched, Args) ->
    register(to_reg_name(Sched), self()),
    begin_proc(Args).

begin_proc(#{formatter := Fmtr, config := #{}=Cfg}) ->
    begin_loop(to_record(Fmtr, Cfg)).

begin_loop(#loki_cfg_r{}=Cfg) ->
    case loop(Cfg, 0, 0) of
        [] ->
            begin_loop(Cfg);
        Other ->
            Fmt = zlib:gzip(jsone:encode({[{streams, Other}]})),
            begin_loop1(Fmt, Cfg)
    end.

begin_loop1(Fmt, #loki_cfg_r{target = #loki_target_r{url = Url}=Trgt}=Cfg) ->
    {ok, _Rsp} = 
        httpc:request(
            post, 
            {Url, append_auth_header(Trgt, [{"content-encoding", "gzip"}]), 
             "application/json", Fmt}, [], []),
    %io:format("Posted ~p~n", [Rsp]),
    begin_loop(Cfg).

append_auth_header(#loki_target_r{auth = undefined}, Hdrs) ->
    Hdrs;
append_auth_header(#loki_target_r{auth = {basic, Usr, Pwd}}, Hdrs) ->
    [{"authorization", "Basic " ++ base64:encode(Usr ++ [$:|Pwd])}|Hdrs];
append_auth_header(#loki_target_r{auth = {bearer, Tok}}, Hdrs) ->
    [{"authorization", "Bearer " ++ Tok}|Hdrs].

loop(#loki_cfg_r{max_count = Max}, _Bytes, Count) when Count >= Max ->
    [];
loop(#loki_cfg_r{max_bytes = Max}, Bytes, _Count) when Bytes >= Max ->
    [];
loop(#loki_cfg_r{format_mod = FmtMod, 
                 format_args = FmtArgs}=Cfg, Bytes, Count) 
    ->
    receive 
        {?MODULE, log, Log} ->
            #{level := Lvl, meta := #{time := MicroSeconds}}=Log,
            Msg = iolist_to_binary(FmtMod:format(Log, FmtArgs)),
            % TODO: ^ May not be needed
            Size = byte_size(Msg),
            NanoSeconds = integer_to_binary(MicroSeconds * 1000),
            [{[{stream, {[{timestamp, NanoSeconds},
                         {level, Lvl},
                         {job, Cfg#loki_cfg_r.job}]}},
               {values, [[NanoSeconds, Msg]]}]}|loop(Cfg, Bytes + Size, Count + 1)];
        Other ->
            error(Other)
    after Cfg#loki_cfg_r.interval ->
        []
    end.

to_record({M, A}, #{job := Job}=Cfg) ->
    #loki_cfg_r{
        interval = map_get(interval, Cfg),
        target = to_trgt_record(map_get(target, Cfg)),
        max_bytes = map_get(max_bytes, Cfg),
        max_count = map_get(max_count, Cfg),
        format_mod = M,
        format_args = A,
        job = if is_list(Job) -> list_to_binary(Job); true -> Job end}.

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
to_trgt_record(#{}=Cfg) ->
    Trgt = 
        #loki_target_r{scheme = map_get(scheme, Cfg),
                       host = map_get(host, Cfg),
                       port = map_get(port, Cfg),
                       path = maps:get(path, Cfg, ""),
                       auth = maps:get(auth, Cfg, undefined)},
    Trgt#loki_target_r{url = recompose(Cfg)}.

recompose(#{scheme := Scheme}=Cfg) when is_atom(Scheme) ->
    recompose(Cfg#{scheme := atom_to_list(Scheme)});
recompose(#{}=Cfg) ->
    uri_string:recompose(
        #{scheme => map_get(scheme, Cfg),
          host => map_get(host, Cfg),
          port => map_get(port, Cfg),
          path => [maps:get(path, Cfg, ""), ?SUB_PATH]}).

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