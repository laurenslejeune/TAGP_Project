Project TAGP
=====

This Erlang application implements the digital twin simulation model provided in:
https://github.com/ValckenaersPaul/TAGP.
This application strives to implement universally usable unit test with EUnit and propery tests with PropEr.

Please note that, to be able to run property tests in this system, both rebar3 and PropEr need to be installed and added to the PATH.

This README provides general information concering the overall project, the different folders are more specific about their role and use.

Project structure
-----
This project has 3 implementations:

1. prj_basic: 
prj_basic is the implementation of the basic assignment, namely to implement testing for the given code. Implementation of the digital twin, EUnit, PropEr, dialyer en typer have been applied.
2. prj_gen_server: 
prj_gen_server implements the same system as (1), but using only gen_servers. By using exactly the same tests as in (1), it can be proven that this gen_server implements the code correctly. The digital twin is also build-able in this version.
3. prj_supervisor: prj_supervisor finally implements the system of (2) using supervisors. This allows for robustness, as the system can be rebooted upon crash.

Additional implementations
-----
Additionally to the code provided for this assignment, additional simulation code was written to monitor the system flow and the system temperature, with according tests.
### System flow
To simulate the flow of the system, the module systemFlow.erl is used. By assuming that a pump delivers a flow between 0 and 10, and the the force that the pump can deliver is inversely correlated to this flow, and that that force is found with the influence-function of a pump, a mathematical approximation of the system flow is calculated. This is done calculating the current flow, adding to that flow the influence of the pipes in the system, and then calculating the force the pump can still deliver for that flow. Using that force, the new flow is calculated. This process is used iteratively to calculate the final flow after transients.
**Please note that this calculation is in no way reflective of the actual behaviour of pumps and flows in a system. This calculation merely strives to give a feel for the behaviour of a flow, given the behaviour of pumps and pipes that flow visits.** Also note that since this system always is constructed as a series circuit, only one flow can be present (due to laws of physics).

For the flow simulation, between the different projects, 2 implementations were made:
- systemFlow.erl always calculates the new flow of the circuit, in any implementation. The communication with this module differs between the different implementations however.
- getSystemFlow.erl manages communication with systemFlow.erl in prj_basic. In order to get the flow, one has to use this module. In this implementation, systemFlow.erl basically is a loop that continuously calculates the flow and then sends that flow to the getSystemFlow.erl process. That process continuously receives new flows, stores those, and responds to any request asking for the flow.
- In pjr_gen_server and prj_supervisor, systemFlow.erl is a gen_server. An updater.erl process continuously sends update_requests to the server (as gen_server:cast(..,..)), which the server then handles in its handle_cast() by calculating the new flow. That means that the server can be directly asked for the current flow in the system.

Both versions of systemFlow.erl and add-ons calculate flows exactly the same.

### System temperature
Very much like the flow of the system, the temperature of the system also is calculated during execution. This is done using the calculation formula provided in the original code (with two minor additions: to avoid division by zero in case of a flow of zero, the ?EPS macro was added. This macro represents a value very close to zero, but not zero. When the flow is zero, the change is nearly zero.). By randomly generating a difference-value between -1 and 1, each heat exchanger has an influence on the temperature of the system with the same order of magnitude. In more advanced implementations, the change of temperature will have to be limited: Currently, if the sum all differences is positive, the system temperature will keep on rising until it reaches an atronomically large value, and remain at that value. 

Very much like the flow, 2 implementations to calculate the temperature were made. One using systemTemp.erl and getSystemTemp.erl, and one using systemTemp.erl and updater.erl. They too work exactly the same.


### Plotter
Since Erlang is not very good at creating easy visualization, a small python program was written to plot flow and temperature after running a digital twin system. collectData.erl collects system flow and temperature during a 60 seconds time period and then stores that data in *data.csv*. *Plotter.py* then can be used to visualize this data using the following command (assuming python 3 or higher being installed):

    $ python3 ./plotter.py
This launches the program and creates a visualization.

Unit testing using EUnit
-----
### Running EUnit with rebar3
Command to run unit testing:

    $ rebar3 eunit
This command runs eunit for all tests in the project. Tests are functions that end with an "_test_" in their name. All unit tests in this project are stored in either *buildSystem_tests_.erl* or *testFunctions_tests.erl*.

*buildSystem_tests_.erl* runs all tests designed to test basic functioning of the modules provided in the above mentioned repo. These modules, as well as additional modules, are all tested for their basic functioning.

More in-depth analysis can be done using the following commands:

    $ rebar3 eunit -v
This provides a more in depth insight in the individual test cases and their results. When crashes or errors occur, this also gives more detailed error reports.

    $ rebar3 eunit --dir=test
This ensures only the test cases in the directory tests/ are run.

In order for a system to be able to function properly, all 81 tests must succeed. Sufficient documentation regarding these tests can be found in the test files.

### Useful links

Eunit documentation and guides can be found on:

[Learn you some Erlang guide](https://learnyousomeerlang.com/eunit#the-need-for-tests)

[Official documentation of EUnit](http://erlang.org/doc/apps/eunit/chapter.html)

Dialyzer and TypEr
-----
Using dialyzer and TypEr, the project files are analyzed. Dialyzer test can be run using:
    
    $ rebar3 dialyzer
All remarks made by the dialyzer have been resolved. Additionally, TypEr has been experimented with to add type-information in the code. This has been done in a limited fashion, as it does not provide much actual value. TypEr can be run using:

    $typer dir/and/file/to/test.erl


Property testing using PropEr
-----

### Getting started

Where unit tests strive to test a single, well defined case, property tests are used to pratically test all possible inputs. While literally testing every singe possibility would not be very efficient, property tests are pretty good at finding bugs. Running these tests can be done using the follwing command:
    
    $ rebar3 proper
This runs every property (every property-based test case) simple 100 times. If one of these 100 tests fails, the property fails. Since some properties are quite time-intensive to test, it may be beneficial to test a single property. This can be done using the following command:

    $ rebar3 -m module -p property
This command runs 100 tests for the property _property_ in module _module_. As a practical example, the command:
    
    $ rebar3 -m prop_base -p prop_test_digital_twin_generation
tests the property *prop_test_digital_twin_generation* 100 times. To specify the number of tests to run for the specified properties, the command:
    
    $ rebar3 proper -n 1000
will run 1000 tests for all properties. It is advised against doing this though, as it may overload the Erlang system. More advisable is running a higher number of tests for a single property.

Finally, whenever a property fails, shrinking data is provided. Shrinking strives to simplify input data to easily understand the failed test, in case of very complex input data. Since input data in our properties never exceeds 3 integers, shrinking is unnecessary and is best simply turned off. Additionally, it is quite simple to have tests cases output custom data that provide more insight as to why the test failed.

Turn off shrinking using:

    $ rebar3 proper --noshrink

### PropEr vs QuickCheck
While PropEr provides the ability for property testing, [QuickCheck (Lite)](http://www.quviq.com/products/erlang-quickcheck/) also provides this service. A case could be made for QuickCheck, but Proper has been chosen for the following reasons:

1. PropEr is open-source, QuickCheck is not.
2. PropEr is still being updated (see github page).

Both methods are supported by rebar3.

### Quick overview of proper tests
Due to lack of clear documentation in the test files, a short overview of the relevant proper tests will be given here. There are 8 relevant properties. 6 of these are found in */test/prop_base.erl*, the other two are in */test/prop_systemFlow_tests.erl* and */test/prop_systemTemp_tests.erl*.

In prop_base.erl:
- **prop_discover_circuit** tests the discover_circuit function of a fluidum.
- **prop_test_flow_influence** tests the flow influence function of a pump
- **prop_test_random_system_pump_always_on** test the systemFlow in a system where the pumps are always on by getting the flow in systemFlow and comparing that to the calculated values using simulation formulas in testFunctions.erl.
- **prop_test_random_system_pump_switched_off** tests if the flow in a system decreases after turning all pump off.
- **prop_test_random_system_pump_switched_on** tests if the flow in a system increases after turning all pumps on.
- **prop_test_digital_twin_generation** generates a digital twin for a given random system and tests of the two twins are compatible, that is, they correctly represent each other.

In prop_systemFlow_tests.erl:
- **prop_calculate_flow** tests the basic functionality of systemFlow within a small, clearly defined given system. It calculates the flow for a given time N with systemFlow, and then checks that flow using exact formulas.

In prop_systemTemp_tests.erl:
- **prop_test_system_temperature** does what **prop_calculate_flow** does, but then for a systemTemp process.

### Useful links
All relevant information on property testing can be found here:

[Github: Installation and getting started](https://github.com/proper-testing/proper)

[PropEr website](https://proper-testing.github.io/)

[PropEr Documentation](https://proper-testing.github.io/apidocs/)

[Online guide](https://propertesting.com/toc.html)
