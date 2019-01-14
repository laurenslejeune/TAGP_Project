prj_supervisor
=====

This Erlang application implements the digital twin simulation model provided in:
https://github.com/ValckenaersPaul/TAGP.
This application strives to implement universally usable unit test with EUnit and propery tests with PropEr.

Please note that, to be able to run property tests in this system, both rebar3 and PropEr need to be installed and added to the PATH.

Build
-----
Compiling the project:

    $ rebar3 compile
Removing .beam-files:

    $ rebar3 clean
Creating a digital twin:

    $ rebar3 shell
This command will be the equivalent of the $ erl command, namely in opening an Erlang shell.
The main supervisor will automatically be created. Interaction with the system can include:
```Erlang
singleSystemController:switchOnPumps().        %Starts all pumps in the system
singleSystemController:switchOffPumps().       %Stops all pumps in the system
singleSystemController:switchOnRandomPump().   %Starts random pump in system, if able
singleSystemController:switchOffRandomPump().  %Stops random pump in system, if able
singleSystemController:getSystemFlow().        %Gives flow of the system
singleSystemController:getSystemTemp().        %Gives temperature of the system
singleSystemController:crashRandomProcess().   %Crashes a random worker process in the system

digitalTwinController:switchOnPumps().         %Starts all pumps in the systems
digitalTwinController:switchOffPumps().        %Stops all pumps in the systems
digitalTwinController:switchOnRandomPump().    %Starts random pump in systems, if able
digitalTwinController:switchOffRandomPump().   %Stops random pump in systems, if able
digitalTwinController:getSystemFlow().         %Gives flow of the systems
digitalTwinController:getSystemTemp().         %Gives temperature of the systems
digitalTwinController:crashRandomProcess().    %Crashes a random worker process in the systems

%Due to registration, only one sytem OR one digital twin system may be active at any time
%The digital twin system contains 2 systems that are copies
```
Information concerning number of pipes, pumps, heat exchangers or whether to create a digital twin can be edited in *prj_supervisor_sup.erl*.



EUnit remarks
-----
In *src/buildSystem.erl*, a plethora of functions to generate arbitrary systems are implemented. A large portion of the functions is used for testing in *test/buildSystem_tests.erl*, where specific instructions and modules are tested. If a system passes these tests, the basic functioning is implemented correctly. 81 unit tests are implemented to test global and more specific characteristics of the code.

PropEr remarks
-----

All proper tests should run without error. Very occasionally, a fail may emerge due to:

- Timing mismatch between two twins: For example when measuring flow from 2 systems, and the systems are both in their transients, the can be on 2 different points in those transients. Normally, in a transient, if at time N the value is MAX - delta, then for N - 1 and N + 1 the values will be MIN + delta. This can cause a fail. **These mismatches appear to happen more oftenly in the gen_server implementation.**
- Since the system processes are not terminated after execution (no supervisors,...) only a maximum amount of tests is possible. Otherwise an crash will occur because the maximum amount of gen_server processes is reached.

These are the same for prj_gen_server.

Supervisor remarks
-----

Since the entire system is now implemented in a supervisor tree, it can reboot upon crash. While this adds robustness, it is important to note that its state will also be reset. That means that all progress is lost, and that the system starts at zero again.

An easy way to prove this is true, is by using the *crashRandomProcess()* function. This sends a nonesense message to a random process in the system, causing the system to crash. The system will then restart without human intervention.
