-module(filename@loki_h).
-include("common.hrl").

-export([priv/0,
         join_priv/1,
         site_data/1, 
         basedir/2]).

priv() ->
    code:priv_dir(?APP).

join_priv(Paths) ->
    filename:join(priv(), Paths).

site_data(Path) ->
    basedir(site_data, Path).

basedir(Type, Path) ->
    case os:type() of
        {win32, _} ->
            basedir1(windows, Type, [?APP_STR, Path]);
        {Os, _} ->
            basedir1(Os, Type, [?APP_STR, Path])
    end.

basedir1(windows, site_data, Path) ->
    [_|_] = ProgramDta = os:getenv("PROGRAMDATA"),
    filename:join(
        [ProgramDta, ?AUTHOR_STR, "ERL", atom_to_list(node())|Path]);
basedir1(unix, Type, Path) ->
    filename:basedir(Type, Path, #{os => linux, author => ?AUTHOR_STR}).