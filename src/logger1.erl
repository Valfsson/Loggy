%%
%% accepts and prints events on the screen
%% --> receive time-stamps on the messages
%%

-module(logger1).
-export([start/1, stop/1]).

%% given list of Nodes

start(Nodes) ->
  spawn_link(fun() ->
    init(Nodes) end).


stop(Logger) ->
    Logger ! stop.


%% list of Nodes that will send it messages, each Node has it own list of messages
init(Nodes) ->

  %%holds msg for each node that are not safe to print yet
  Queue=[],

  %% holds least messages from each Node
  Clock=time:clock(Nodes),

  loop(Queue, Clock).


%%handles messages
loop(Queue, Clock) ->
  receive
    {log, From, Time, Msg} ->

    %% updates Clock
    UpdatedClock=time:update(From,Time,Clock),

    %% sorts Clock
    SortedClock=lists:keysort(2,UpdatedClock),

    %%adds msg to NewQueue
    TempQueue= Queue++[{From,Time,Msg}],

    %% sorts Queue
    UpdatedQueue=lists:keysort(2,TempQueue),


    %% goes through the queue
    NewQueue=iterate(UpdatedQueue,SortedClock),

    loop(NewQueue, SortedClock);

    stop ->
      QSize=length(Queue),
      io:format("Queue size: ~w Left in Queue: ~w~n",[QSize,Queue]),
      ok
  end.




%%pint all the msg from Queue with equal or lower counters than ´MinTime´
iterate(Queue, Clock) ->

    case Queue of
      [{Name, Counter,{Action,{Message,Id}}} | _] ->

      %%finds events that are safe to print out
      case time:safe(Counter, Clock) of

          true ->
              log(Name, Counter, {Action,{Message,Id}}),

              QueueAfter = lists:delete({Name, Counter,{Action,{Message,Id}}}, Queue),
              iterate(QueueAfter, Clock);

          false ->
              %%io:format("False ~n"),
              Queue
          end;
        []  ->
            %%io:format("Empty list ~n"),
            Queue
    end.



log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
