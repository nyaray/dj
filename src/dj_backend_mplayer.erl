%%% @doc This module implements the dj backend protocol for mplayer.
-module(dj_backend_mplayer).


-behaviour(gen_server).


-record(mplayer_state, {
        slave_pid   = undefined               :: undefined | pid()
        ,slave_ref  = undefined               :: undefined | term()
        ,slave_fifo = <<"/tmp/mplayer.fifo">> :: binary()
}).


%%% API
-export([start_link/0]).


%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


%%%
%%% API
%%%


start_link() ->
    {ok, _PID} = gen_server:start({local, ?MODULE}, ?MODULE, [], []).


%%%
%%% gen_server callbacks
%%%


init(_Args) ->
    State = spawn_slave(#mplayer_state{}),
    {ok, State}.


handle_call({command, Command}, _Requester, State) ->
    Result = do_command(Command, State),
    {reply, Result, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, State) ->
    UpdatedState = spawn_slave(State),
    {noreply, UpdatedState};


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%
%%% Internal API
%%%


spawn_slave(State) ->
    {SlavePID, MonitorRef} = spawn_monitor(dj_backend_mplayer_slave, init, []),
    State#mplayer_state{
        slave_pid = SlavePID
        ,slave_ref = MonitorRef
    }.


do_command(Command, #mplayer_state{ slave_fifo = SlaveFIFO }) ->
    CommandBin = translate_command(Command),
    RunCommand = << "echo ", CommandBin/binary, " > ", SlaveFIFO/binary >>,
    dj_command:run(RunCommand).


%%%
%%% Utilities
%%%


translate_command(pause) -> <<"pause">>.

