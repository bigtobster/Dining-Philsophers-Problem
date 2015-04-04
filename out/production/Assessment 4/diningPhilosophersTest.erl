%%%-------------------------------------------------------------------
%%% @author Toby Leheup - TL258
%%% @copyright (C) 2014, <University of Kent>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2014 17:30
%%%-------------------------------------------------------------------
-module(diningPhilosophersTest).
-author("Toby Leheup").

%% API
-export([superTest/0]).

-spec superTest() -> ok.
superTest() ->
	Bool =  %collegeTest()               andalso
	        manySpawnTest()             andalso
	        reportingTest()             andalso
					chopstickTest()             andalso
					philosopherTest()           andalso
					requestChopsticksTest()     andalso
					dropChopstickTest()         ,
	timer:sleep(100),
	case Bool of
		true ->
			io:format("All Tests Passed!~n");
		false ->
			io:format("Some failed tests...~n")
	end.

%% -spec collegeTest() -> boolean().
%% collegeTest() ->
%% 	Philosophers = diningPhilosophers:college(),
%% 	case length(Philosophers) == 5 of
%% 		true ->
%% 			true;
%% 		false ->
%% 			io:format("collegeTest failed~n"),
%% 			false
%% 	end.

-spec manySpawnTest() -> boolean().
manySpawnTest() ->
	Reporter = spawn_link(diningPhilosophers, reporting, []),
	Procs = diningPhilosophers:manySpawn(chopstick, [5, Reporter]),
	case length(Procs) == 5 of
		true ->
			true;
		false ->
			io:format("manySpawnTest failed"),
			false
	end.

-spec reportingTest() -> boolean().
reportingTest() ->
	Proc = spawn_link(diningPhilosophers, reporting, []),
	Proc ! {up, 1, right, plato},
	Proc ! {down, 1, right, plato},
	true.

-spec chopstickTest() -> boolean().
chopstickTest() ->
	Reporting = spawn_link(diningPhilosophers, reporting, []),
	Chopstick = spawn_link(diningPhilosophers, chopstick, [1, Reporting]),
	Chopstick ! {up, right, plato, self()},
	receive
		{ok, Chopstick} ->
			Chopstick ! {up, right, plato, self()},
			receive
				{no, Chopstick} ->
					Chopstick ! {down, right, plato, self()},
					receive
						{ok, Chopstick} ->
							true
					after 50 ->
						io:format("chopstickTest failed @ ok 2~n"),
						false
					end
			after 50 ->
				io:format("chopstickTest failed @ no~n"),
				false
			end
	after 50 ->
		io:format("chopstickTest failed @ ok 1~n"),
		false
	end.

-spec philosopherTest() -> boolean().
philosopherTest() ->
	random:seed(now()),
	Randy = random:uniform(5),
	Reporter = spawn_link(diningPhilosophers, reporting, []),
	Chopsticks = diningPhilosophers:manySpawn(chopstick, [5, Reporter]),
	_Philosopher = spawn_link(diningPhilosophers, philosopher_init, [Randy, Reporter, Chopsticks]),
	timer:sleep(5000),
	true.

requestChopsticksTest() ->
	Reporter = spawn_link(diningPhilosophers, reporting, []),
	[C1, C2, _, _, _] = Chopsticks = diningPhilosophers:manySpawn(chopstick, [5, Reporter]),
	{Id1, Chopstick1} = diningPhilosophers:requestLeftChopstick(1, plato, Chopsticks),
	case ((Id1 == 1) andalso (C1 == Chopstick1)) of
		true ->
			{Id2, Chopstick2} = diningPhilosophers:requestRightChopstick(1, plato, Chopsticks),
			case ((Id2 == 2) andalso (C2 == Chopstick2)) of
				true ->
					true;
				false ->
					io:format("requestChopstickTest failed on Right...~n"),
					false
			end;
		false ->
			io:format("requestChopstickTest failed on Left...~n"),
			false
	end.

dropChopstickTest() ->
	Reporter = spawn_link(diningPhilosophers, reporting, []),
	Chopsticks = diningPhilosophers:manySpawn(chopstick, [5, Reporter]),
	{_, Chopstick} = diningPhilosophers:requestLeftChopstick(1, plato, Chopsticks),
	diningPhilosophers:dropChopstick(left, plato, Chopstick),
	true.
