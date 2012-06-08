%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the webmachine_demo application.

-module(webmachine_demo_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip0 = case os:getenv("WEBMACHINE_IP") of
        false -> "0.0.0.0";
        Any -> Any
    end,

    ParsedIp = case Ip0 of
        any ->
            any;
        Ip when is_tuple(Ip) ->
            Ip;
        Ip when is_list(Ip) ->
            {ok, IpTuple} = inet_parse:address(Ip),
            IpTuple
    end,

    {ok, App} = application:get_application(?MODULE),
    {ok, Dispatch} = file:consult(filename:join([code:priv_dir(App),
                                                 "dispatch.conf"])),
    WebConfig = [
                 {ip, ParsedIp},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochicow,
           {webmachine_mochicow, start, [WebConfig]},
           permanent, 5000, worker, [webmachine_mochicow]},
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.
