%%%-------------------------------------------------------------------
%%% @author Toby Leheup - TL258
%%% @copyright (C) 2014, <University of Kent>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2014 18:34
%%%-------------------------------------------------------------------
-module(diningPhilosophers).
-author("Toby Leheup").

-export([college/0]).

%%%%%%%%%%%%%%%%%%%%%%%%% REMOVE ME %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-compile(export_all).

%% Lead process that creates all philosophers, chopsticks, plumbing, reporting etc
-spec college() -> list(pid()).
college() ->
	Reporter = spawn_link(?MODULE, reporting, []),
	Gateway = spawn_link(?MODULE, deadlockDetector, [0, Reporter]),
	Chopsticks = manySpawn(chopstick, [5, Gateway]),
	_Philosophers = manySpawn(philosopher_init, [5, Gateway, Chopsticks]).

%% Accessor function for manySpawn/3
-spec manySpawn(atom(), list()) -> list(pid()).
manySpawn(Process, Args) ->
	manySpawn(Process, Args, []).

%% Spawns N many times and returns the process collection generated
%% Useful for clean repetitive code
-spec manySpawn(atom(), list(), list()) -> list(pid()).
manySpawn(_, [0|_], List) ->
	List;
manySpawn(Process, [Counter | Args], List) ->
	Pid = spawn_link(?MODULE, Process, [Counter | Args]),
	manySpawn(Process, [Counter - 1 | Args], [Pid] ++ List).

%% Updates the user as to the internal happenings of the dining philosophers
%% Dumb message to output parser
-spec reporting() -> atom().
%% noinspection ErlangUnusedFunction
reporting() ->
	receive
		{up, Id, Hand, Philosopher} ->
			io:format("~p picked up chopstick ~p with his ~p hand~n", [Philosopher, Id, Hand]),
			reporting();
		{down, Id, Hand, Philosopher} ->
			io:format("~p dropped chopstick ~p from his ~p hand~n", [Philosopher, Id, Hand]),
			reporting();
		{thinking, Name} ->
			io:format("~p is thinking...~n", [Name]),
			reporting();
		{hungry, Name} ->
			io:format("~p is hungry!~n", [Name]),
			reporting();
		{eating, Name} ->
			io:format("~p is eating...~n", [Name]),
			reporting();
		{error, ErrorMsg} ->
			io:format("~p~n", [ErrorMsg])
	end.

%% Detects if the application has deadlocked
%% Shutsdown on deadlock detection
%% Forwards all messages to Reporter
-spec deadlockDetector(integer(), pid()) -> atom().
%% noinspection ErlangUnusedFunction
deadlockDetector(DiningPhilosophers, Reporter) when DiningPhilosophers < 5 ->
	receive
		{up, Id, left, Philosopher} ->
			Reporter ! {up, Id, left, Philosopher},
			deadlockDetector(DiningPhilosophers+1, Reporter);
		{down, Id, left, Philosopher} ->
			Reporter ! {down, Id, left, Philosopher},
			deadlockDetector(DiningPhilosophers-1, Reporter);
		Msg ->
			Reporter ! Msg,
			deadlockDetector(DiningPhilosophers, Reporter)
	end;
deadlockDetector(_, Reporter) ->
	Reporter ! {error, "Deadlock detected! Shutting down..."},
	% Allows terminal to be updated before quiting...
	timer:sleep(200),
	exit(deadlock).

%% Accessor function for chopstick/3
-spec chopstick(integer(), pid()) -> atom().
%% noinspection ErlangUnusedFunction
chopstick(Id, Gateway) ->
	chopstick(Id, Gateway, onTable).

%% Process that represents being a chopstick.
%% Takes messages from philosophers, checks for validity and updates the reporter accordingly
-spec chopstick(integer(), pid(), atom()) -> atom().
chopstick(Id, Gateway, Status) ->
	receive
		{up, Hand, Philosopher, Return} ->
			case Status of
				onTable ->
					Return ! {ok, self()},
					Gateway ! {up, Id, Hand, Philosopher},
					chopstick(Id, Gateway, inUse);
				inUse ->
					Return ! {no, self()},
					chopstick(Id, Gateway, Status)
			end;
		{down, Hand, Philosopher, Return} ->
			case Status of
				onTable ->
					Gateway ! {error, "Attempted to drop Chopstick that is already dropped!"},
					exit(1);
				inUse ->
					Return ! {ok, self()},
					Gateway ! {down, Id, Hand, Philosopher},
					chopstick(Id, Gateway, onTable)
			end
	end.

%% Initialiser for philosophers
%% Essentially just converts an id into a Philosopher name
-spec philosopher_init(integer(), pid(), list(pid())) -> atom().
philosopher_init(5, Gateway, Chopsticks) ->
	random:seed(now()),
	philosopher(zeno, Gateway, Chopsticks);
philosopher_init(4, Gateway, Chopsticks) ->
	random:seed(now()),
	philosopher(pythagoras, Gateway, Chopsticks);
philosopher_init(3, Gateway, Chopsticks) ->
	random:seed(now()),
	philosopher(horus, Gateway, Chopsticks);
philosopher_init(2, Gateway, Chopsticks) ->
	random:seed(now()),
	philosopher(euphrates, Gateway, Chopsticks);
philosopher_init(1, Gateway, Chopsticks) ->
	random:seed(now()),
	philosopher(atticus, Gateway, Chopsticks).

%% Process that represents being a Philosopher
%% Messages and holds chopstick processes, thinks, gets chopsticks and eats
%% Due to a wide range of random values, this process is UNLIKELY to deadlock but given enough time, it probably will
-spec philosopher(atom(), pid(), list(pid())) -> atom().
philosopher(Name, Gateway, Chopsticks) ->
	ThinkingTime = random:uniform(1001) - 1,
	Gateway ! {thinking, Name},
	timer:sleep(ThinkingTime),
	Gateway ! {hungry, Name},
	{LeftChopstickId, LeftChopstick} = requestLeftChopstick(random:uniform(5), Name, Chopsticks),
	{_RightChopstickId, RightChopstick} = requestRightChopstick(LeftChopstickId, Name, Chopsticks),
	EatingTime = random:uniform(1001) - 1,
	Gateway ! {eating, Name},
	timer:sleep(EatingTime),
	dropChopstick(left, Name, LeftChopstick),
	dropChopstick(right, Name, RightChopstick),
	philosopher(Name, Gateway, Chopsticks).

%% Persistently requests a chopstick for the left hand of a philosopher until success
-spec requestLeftChopstick(pos_integer(), atom(), list(pid())) -> {integer(), pid()}.
requestLeftChopstick(ChopstickIndex, PhilosopherName, Chopsticks) ->
	Chopstick = lists:nth(ChopstickIndex, Chopsticks),
	Chopstick ! {up, left, PhilosopherName, self()},
	receive
		{ok, Chopstick} ->
			{ChopstickIndex, Chopstick};
		{no, Chopstick} ->
			Timeout = random:uniform(50),
			timer:sleep(Timeout),
			requestLeftChopstick(random:uniform(5), PhilosopherName, Chopsticks)
	end.

%% Persistently requests a chopstick for the right hand of a philosopher until success
-spec requestRightChopstick(pos_integer(), atom(), list(pid())) -> {integer(), pid()}.
requestRightChopstick(LeftChopstickIndex, PhilosopherName, Chopsticks) ->
	ChopstickIndex = ((LeftChopstickIndex) rem 5) + 1,
	Chopstick = lists:nth(ChopstickIndex, Chopsticks),
	Chopstick ! {up, right, PhilosopherName, self()},
	receive
		{ok, Chopstick} ->
			{ChopstickIndex, Chopstick};
		{no, Chopstick} ->
			Timeout = random:uniform(50),
			timer:sleep(Timeout),
			requestRightChopstick(LeftChopstickIndex, PhilosopherName, Chopsticks)
	end.

%% Notifies a chopstick that it is no longer needed so it can change its status
-spec dropChopstick(atom(), atom(), pid()) -> boolean().
dropChopstick(Hand, Name, Chopstick) ->
	Chopstick ! {down, Hand, Name, self()},
	receive
		{ok, Chopstick} ->
			true
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CHALLENGE ANSWERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A)  Yes, this system will deadlock.
%%  B)  This system will deadlock fairly rarely due to the unlikely probability of each philosopher going to the
%%      table with all the other philosophers when the random time is set to 0-1000. It would occur eventually though.
%%      This can be proven by reducing the time that each philosopher spends thinking and eating to a much smaller
%%      number. Deadlock would occur when the philosophers go in groups - how often this would occur is hard to
%%      predict but roughly (1/500)^5 of a philosopher going to the table (guess!). This is because the philosopher
%%      would need to go roughly at the same time of another philosopher (within half a ms == 1000/2 = 500) and this
%%      would have to apply to each philosopher (^5).
%%  C)  Deadlock could be detected when all 5 of the philosophers are sitting at the table with a fork in their left
%%      hand. In this system, there is no possible recovery from this situation and the application exits. The
%%      detection occurs by counting the number of philosophers at the table in a non-eating state. An up request
%%      increments the counter and a down request decrements it.