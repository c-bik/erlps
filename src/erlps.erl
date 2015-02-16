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
                    wmic_process_info_async(self(), Pids),
                    receive
                        {_, ProcessInfos} -> ProcessInfos
                    end
            end
    end.

-define(BLK_SIZE, 50). % Least resposponse time setting

wmic_process_info_async(Master, Pids) when length(Pids) > ?BLK_SIZE ->
    {Pids1, Pids2} = lists:split(length(Pids) div 2, Pids),
    LocalMaster = self(),
    Pids1Pid = spawn(fun() ->
                             wmic_process_info_async(LocalMaster, Pids1)
                     end),
    Pids2Pid = spawn(fun() ->
                             wmic_process_info_async(LocalMaster, Pids2)
                     end),
    Master ! {self(), wmic_pinfo_rx_loop({Pids1Pid, Pids2Pid}, [])};
wmic_process_info_async(Master, Pids) when length(Pids) =< ?BLK_SIZE ->
    Master ! {self(), wmic_process_info(Pids)}.

wmic_pinfo_rx_loop({undefined, undefined}, PInfos) -> PInfos;
wmic_pinfo_rx_loop({P1,P2}, PInfos) ->
    receive
        {P1, PInfs} ->
            wmic_pinfo_rx_loop({undefined, P2}, PInfs++PInfos);
        {P2, PInfs} ->
            wmic_pinfo_rx_loop({P1, undefined}, PInfs++PInfos)
    end.

wmic_process_info(Pids) ->
    wmic_process_info([lists:flatten(P) || P <- Pids], []).
wmic_process_info([], ProcessInfos) -> ProcessInfos;
wmic_process_info([Pid|Pids], ProcessInfos) ->
    wmic_process_info(
      Pids, [#{ pid => list_to_integer(Pid),
                name => wmic_fmt_field(Pid, "Name")}
             | ProcessInfos]).

wmic_fmt_field(Pid, Field) ->
    re:replace(
      os:cmd(lists:flatten(
               ["wmic process where ProcessId=\"",
                Pid, "\" get ", Field])),
      lists:flatten(["(",Field,"\s*\r\r\n)|([ \r\n]+)"]),
      "", [global, {return, list}]).
