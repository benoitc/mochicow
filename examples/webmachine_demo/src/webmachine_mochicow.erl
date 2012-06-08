-module(webmachine_mochicow).

-export([start/1, stop/0]).

start(Options) ->
    {DispatchList, Options1} = get_option(dispatch, Options),
    {ErrorHandler0, Options2} = get_option(error_handler, Options1),
    {EnablePerfLog, Options3} = get_option(enable_perf_logger, Options2),
    ErrorHandler =
        case ErrorHandler0 of
            undefined ->
                webmachine_error_handler;
            EH -> EH
        end,
    {LogDir, Options4} = get_option(log_dir, Options3),
    case whereis(webmachine_logger) of
      undefined ->
        webmachine_sup:start_logger(LogDir);
      _ ->
        ignore
    end,
    case EnablePerfLog of
        true ->
          case whereis(webmachine_perf_logger) of
            undefined ->
              application_set_unless_env(webmachine, enable_perf_logger, true),
              webmachine_sup:start_perf_logger(LogDir);
            _ ->
              ignore
          end;
        _ ->
            ignore
    end,
    {PName, Options5} = case get_option(name, Options4) of
      {undefined, _} -> {?MODULE, Options4};
      {PN, O5} -> {PN, O5}
    end,
    application_set_unless_env(webmachine, dispatch_list, DispatchList),
    application_set_unless_env(webmachine, error_handler, ErrorHandler),

    Loop = {webmachine_mochiweb, loop},
    cowboy:start_listener(PName, 100,
                          cowboy_tcp_transport, Options5,
                          mochicow_protocol, [{loop, Loop}]).

stop() ->
    {registered_name, PName} = process_info(self(), registered_name),
    cowboy:stop_listener(PName).


get_option(Option, Options) ->
    case lists:keytake(Option, 1, Options) of
       false -> {undefined, Options};
       {value, {Option, Value}, NewOptions} -> {Value, NewOptions}
    end.

application_set_unless_env(App, Var, Value) ->
    Current = application:get_all_env(App),
    CurrentKeys = proplists:get_keys(Current),
    case lists:member(Var, CurrentKeys) of
        true ->
            ok;
        false ->
            application:set_env(App, Var, Value)
    end.

