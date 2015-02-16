-module(erlps).

-behaviour(application).
-behaviour(supervisor).

%% Public interfaces
-export([list/0]).

%% Debug interface
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Debug APIs
%% ===================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%% ===================================================================
%% Interface APIs
%% ===================================================================

list() -> list(os:type()).

list({win32,nt}) ->
    case os:find_executable("wmic") of
        false -> {error, wmic_not_found};
        _ ->
            case re:run(os:cmd("wmic process get ProcessId"),
                        "([0-9]+)\s*\r\r\n", [global, {capture, [1], list}]) of
                {match, Pids} ->
                    process_info_async(win, self(), Pids),
                    receive
                        {_, ProcessInfos} -> ProcessInfos
                    end
            end
    end;
list({unix,linux}) ->
    case re:run(os:cmd("ps ax -o pid"), "([0-9]+)[ \r\n]*",
                [global, {capture, [1], list}]) of
        {match, Pids} ->
            process_info_async(linux, self(), Pids),
            receive
                {_, ProcessInfos} -> ProcessInfos
            end
    end;
list(Os) ->
    {error, {unsupported, Os}}.

-define(BLK_SIZE, 50). % Least resposponse time setting

process_info_async(Os, Master, Pids) when length(Pids) > ?BLK_SIZE ->
    {Pids1, Pids2} = lists:split(length(Pids) div 2, Pids),
    LocalMaster = self(),
    Pids1Pid = spawn(fun() ->
                             process_info_async(Os, LocalMaster, Pids1)
                     end),
    Pids2Pid = spawn(fun() ->
                             process_info_async(Os, LocalMaster, Pids2)
                     end),
    Master ! {self(), pinfo_rx_loop({Pids1Pid, Pids2Pid}, [])};
process_info_async(Os, Master, Pids) when length(Pids) =< ?BLK_SIZE ->
    Master ! {self(), info_process(Os, Pids)}.

pinfo_rx_loop({undefined, undefined}, PInfos) -> PInfos;
pinfo_rx_loop({P1,P2}, PInfos) ->
    receive
        {P1, PInfs} ->
            pinfo_rx_loop({undefined, P2}, PInfs++PInfos);
        {P2, PInfs} ->
            pinfo_rx_loop({P1, undefined}, PInfs++PInfos)
    end.

info_process(Os, Pids) ->
    info_process(Os, [lists:flatten(P) || P <- Pids], []).
info_process(_Os, [], ProcessInfos) -> ProcessInfos;
info_process(Os, [Pid|Pids], ProcessInfos) ->
    info_process(Os, Pids,
                 [#{pid => list_to_integer(Pid),
                    name => fmt_field(Os, Pid, "Name")}
                 | ProcessInfos]).

fmt_field(linux, Pid, "Name") -> fmt_field(linux, Pid, "cmd");
fmt_field(linux, Pid, Field) ->
    re:replace(
      os:cmd(lists:flatten(["ps -p ", Pid, " -o ", Field])),
      lists:flatten(["(",string:to_upper(Field),"\s*[\r\n]+)|([ \r\n]+)"]),
      "", [global, {return, list}]);
fmt_field(win, Pid, Field) ->
    re:replace(
      os:cmd(lists:flatten(
               ["wmic process where ProcessId=\"",
                Pid, "\" get ", Field])),
      lists:flatten(["(",Field,"\s*\r\r\n)|([ \r\n]+)"]),
      "", [global, {return, list}]).
