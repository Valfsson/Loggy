%%
%% waits - prepared to receive msg
%% then sends msg to one of it's peers
%%
%%
%% sleep - >  how active the worker sends messages
%% jitter -> random delay between sending msg and log entry

-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).


stop(Worker) ->
    Worker ! stop.


init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
      {peers, Peers} ->                         %% can start all workers, then inform who their peer are
            loop(Name, Log, Peers, Sleep, Jitter, time:zero());
      stop ->
            ok
    end.


peers(Wrk, Peers)->
      Wrk! {peers, Peers}.


loop (Name, Log, Peers, Sleep, Jitter, Counter) ->
    Wait = random:uniform(Sleep),
    receive

      {msg, Time, Msg} ->
        Temptime= time:merge(Time, Counter),
        Newtime= time:inc(Name,Temptime),
        Log ! {log, Name, Newtime, {received, Msg}},           
        loop(Name, Log, Peers, Sleep, Jitter, Newtime);

      stop ->
          ok;

      Error ->
        Log ! {log, Name, time, {error, Error}}

      after Wait ->
        Selected = select(Peers),
        Time= time:inc(Name, Counter),
        Message= {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, Time)
    end.


select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) ->
    ok;
jitter(Jitter) ->
    timer:sleep(random:uniform(Jitter)).
