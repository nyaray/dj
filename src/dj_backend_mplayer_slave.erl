-module(dj_backend_mplayer_slave).

-compile([export_all]).

init(Args) ->
    MasterPID = proplists:get_value(<<"master">>, Args),
    FIFOPath = proplists:get_value(<<"fifo_path">>, Args),
    FIFOPort = dj_command:run(<< "tail -f ", FIFOPath >>),
    State = {MasterPID, FIFOPort},
    loop(State).

loop(State = {MasterPID, FIFOPort}) ->
    receive
        {FIFOPort, {data, {eol, LineBin}}} ->
            io:format("LineBin: ~p~n", [LineBin]),
            MasterPID ! {self(), {data, LineBin}},
            loop(State);
        {FIFOPort, {exit_status, ExitStatus}} ->
            io:format("ExitStatus: ~p~n", [ExitStatus]),
            MasterPID ! {self(), {exit_status, ExitStatus}};
        DiscardedMessage ->
            io:format("DiscardedMessage: ~p~n", [DiscardedMessage]),
            loop(State)
    end.
