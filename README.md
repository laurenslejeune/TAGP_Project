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

Unit testing using EUnit
-----
Command to run unit testing:

    $ rebar3 eunit
This command runs eunit for all tests in the project. Tests are functions that end with an "_test_" in their name. All unit tests in this project are stored in either *buildSystem_tests_.erl* or *testFunctions_tests.erl*.

*buildSystem_tests_.erl* runs all tests designed to test basic functioning of the modules provided in the above mentioned repo. These modules, as well as additional modules, are all tested for their basic functioning.

More in-depth analysis can be done using the following commands:

    $ rebar3 eunit -v
This provides a more in depth insight in the individual test cases and their results. When crashes or errors occur, this also gives more detailed error reports.

    $ rebar3 eunit --dir=test
This ensures only the test cases in the directory tests/ are run.

In order for a system to be able to function properly, all 81 tests must succeed.

Eunit documentation and guides can be found on:

https://learnyousomeerlang.com/eunit#the-need-for-tests

http://erlang.org/doc/apps/eunit/chapter.html

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
3. PropEr is supported in rebar3, QuickCheck is not.


### Useful links
All relevant information on property tests can be found here:

[Github: Installation and getting started](https://github.com/proper-testing/proper)

[PropEr website](https://proper-testing.github.io/)

[PropEr Documentation](https://proper-testing.github.io/apidocs/)

[Online guide](https://propertesting.com/toc.html)
