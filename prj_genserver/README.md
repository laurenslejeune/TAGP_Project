prj_genserver
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
Next, the digital twin must be created:
```Erlang
digitalTwin:create(Npipes,Npumps,Nhex). %Generates a digital twin system
digitalTwin:startPumps().               %Starts all pumps in the systems
digitalTwin:stopPumps().                %Stops all pumps in the systems
digitalTwin:getDigitalTwinData().       %Return a batch of data from the systems
digitalTwin:stopDigitalTwin().          %Stops the digital twin
%Due to registration, only one digital twin may be active at any time
```
Please note that the number of pipes, _Npipes_, needs to be larger than both the number of pumps _Npumps_ and the number of heat
 exchangers _Nhex_. Pipes can contain a pump and an heat exchanger simultaneously. Additionaly, also the flow meter may be added unto any pipe.
The differences for the heat exchangers are randomly generated, but exactly the same for both systems.

EUnit remarks
-----
In *src/buildSystem.erl*, a plethora of functions to generate arbitrary systems are implemented. A large portion of the functions is used for testing in *test/buildSystem_tests.erl*, where specific instructions and modules are tested. If a system passes these tests, the basic functioning is implemented correctly. 81 unit tests are implemented to test global and more specific characteristics of the code.

PropEr remarks
-----

All proper tests should run without error. Very occasionally, a fail may emerge due to:

- Timing mismatch between two twins: For example when measuring flow from 2 systems, and the systems are both in their transients, the can be on 2 different points in those transients. Normally, in a transient, if at time N the value is MAX - delta, then for N - 1 and N + 1 the values will be MIN + delta. This can cause a fail. **These mismatches appear to happen more oftenly in the gen_server implementation.**
- Since the system processes are not terminated after execution (no supervisors,...) only a maximum amount of tests is possible. Otherwise an crash will occur because the maximum amount of gen_server processes is reached.
