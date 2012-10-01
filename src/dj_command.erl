-module(dj_command).

-export([
        run/1
        ,run/3
        ,run_async/3
        ,gather_output/1
        ,gather_output/2
        ,gather_output/3
        ,log_output/2
        ,log_output/3
        ,log_json/2
    ]).

-define(OPTIONS, [
    binary
    ,exit_status
    ,in
    ,line
    ,stderr_to_stdout
    ,use_stdio
]).

-type command()            :: binary().
-type command_options()    :: [command_option()].
-type command_option()     :: binary() | {binary(), binary()}.
-type directory()          :: binary().
-type run_command_return() :: {ok, {exit_status(), command_outputs()}} |
                              {timeout, command_output()}.
-type exit_status()        :: integer().
-type command_outputs()    :: [command_output()].
-type command_output()     :: binary().

%%%
%%% API
%%%

%% @doc see run/3
run(Command) ->
    run(Command, <<"/tmp">>, []).

%% @doc run runs Command in Dir, ignoring Options.
%%
%% run is equivalent to in (immediate succession) calling run_async(C, D, O),
%% gather_output(P) and erlang:port_close(P) with the obtained port.
%%
%% returns: see gather_output/1
-spec run(Command, Dir, Options) -> Result when
    Command  :: command()
    ,Dir     :: directory()
    ,Options :: command_options()
    ,Result  :: run_command_return().
run(Command, Dir, Options) ->
    gather_output(run_async(Command, Dir, Options)).

%% @doc run_async runs Command in Dir, ignoring Options.
%%
%% returns: the port opened for Command.
%% @end
-spec run_async(Command, Dir, Options) -> port() when
    Command   :: command()
    ,Dir      :: directory()
    ,Options  :: command_options().
run_async(Command, Dir, _Options) ->
    CommandStr = binary_to_list(Command),
    DirStr     = binary_to_list(Dir),
    open_port({spawn, CommandStr}, [{cd, DirStr} | ?OPTIONS]).


%%% output gathering

%% TODO: see gather_output/2
gather_output(P) ->
    gather_output(P, infinity).

%% TODO: document this.
gather_output(P, Timeout) ->
    gather_output(P, Timeout, []).

%%% output logging

%% TODO: see log_output/3
log_output(P, LogHandle) ->
    log_output(P, LogHandle, infinity).

%% TODO: document this.
log_output(P, LogHandle, Timeout) ->
    do_log_output(P, LogHandle, Timeout).

%% append proplist as json to file
log_json(Props, LogHandle) ->
    JSON = jsx:encode(Props),
    file:write(LogHandle, JSON).

%%%
%%% internal
%%%

gather_output(P, Timeout, Acc) ->
    receive
        {P, {data, {eol, LineBin}}} ->
            gather_output(P, Timeout, [LineBin | Acc]);
        {P, {exit_status, ExitStatus}} ->
            run_cleanup(P),
            {ok, {ExitStatus, lists:reverse(Acc)}};
        _Discard ->
            % log/notify about discarding a port message?
            gather_output(P, Timeout, Acc)
    after
        Timeout ->
            run_cleanup(P),
            {timeout, Acc}
    end.

do_log_output(P, LogHandle, Timeout) ->
    receive
        {P, {data, {eol, LineBin}}} ->
            LineLog = << LineBin/binary, "\n" >>,
            file:write(LogHandle, LineLog),
            do_log_output(P, LogHandle, Timeout);
        {P, {exit_status, ExitStatus}} ->
            ExitStatusBin = list_to_binary(integer_to_list(ExitStatus)),
            ExitLog = << "============\nexited with status ",
                         ExitStatusBin/binary,
                         "\n\n" >>,
            file:write(LogHandle, ExitLog),
            run_cleanup(P),
            {ok, ExitStatus};
        _Discard ->
            % log/notify about discarding a port message?
            do_log_output(P, LogHandle, Timeout)
    after
        Timeout ->
            TimeoutBin = list_to_binary(integer_to_list(Timeout)),
            TimeoutLog = << "============\nCommand timed out after ",
                            TimeoutBin/binary,
                            " ms\n---\n\n" >>,
            file:write(LogHandle, TimeoutLog),
            run_cleanup(P),
            {error, timeout}
    end.

run_cleanup(P) ->
    case erlang:port_info(P) of
        undefined ->
            true;
        _ ->
            port_close(P)
    end.
