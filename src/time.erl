-module(time).
-export([zero/0,inc/2, merge/2, leq/2, clock/1, update/3, safe/2, minTime/1]).


%% returns initial Lamport value

zero() ->
    0.


%% returns time T incremented by 1

inc(Name, T) ->
    T+1.


%% merges the two Lamport time stamps (finds the max value)
merge(Ti, Tj) ->
    lists:max([Ti,Tj]).



%% true if Ti <= to Tj (increase Tj. T1-msg, t2-mycounter),else Ti
leq(Ti, Tj) ->
  case (Ti =< Tj) of
    true -> Tj;
    false -> Ti
  end.



%% creates Clock list that holds Nodes
clock(Nodes) ->
      lists:map(fun(X)-> {X,0} end, Nodes).



%% updates Clock with last message from the Node
update(Node, Time, Clock) ->
      case lists:keyfind(Node, 1, Clock) of

            {_,_} ->
                  Temp = lists:keydelete(Node, 1, Clock),
                  NewClock = lists:append([{Node,Time}], Temp),
                  NewClock;

            false ->
                    Clock

      end.



%% determines if it is safe to print an event happened at Time. Returns true/false
safe(Time, Clock) ->

    MinCounter=minTime(Clock),

          case Time=<MinCounter of

            true -> true;
            false-> false

          end.


%% gives min timestamp in Clock list
minTime(Clock) ->
  [H|_] = Clock,
  {_,MinTime} = H,
  MinTime.
