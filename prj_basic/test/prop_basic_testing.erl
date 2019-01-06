-module(prop_basic_testing).
-include_lib("proper/include/proper.hrl").

%Testing the functioning of macros:

prop_forall_test1() ->
    ?FORALL(Test,integer(0,10),testFunctions:is_odd(Test) or testFunctions:is_even(Test)).

prop_forall_test2() ->
    ?FORALL(_,atom,true).

prop_exists_test1()->
    %Test if there exists an odd integer between 0 and 10.
    ?EXISTS(Integer,integer(0,10),testFunctions:is_odd(Integer)).

% prop_setup_test1() ->
%     ?SETUP(fun test_setup/0,true).

% test_setup() ->
% 	io:format("This is a test setup~n",[]), ok.