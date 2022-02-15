############################################################################
##
#W  standard/transducer.tst
#Y  Copyright (C) 2017                                 Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("aaa package: standard/transducer.tst");
gap> LoadPackage("aaa", false);;

#T# Transducer
gap> f := Transducer(2, 2, [[3, 2], [1, 2], [3, 2]], [[[1], [0]], [[1, 1],
>                     [1, 0]], [1, []]]);
Error, aaa: Transducer: usage,
the fourth argument contains invalid output,
gap> f := Transducer(2, 2, [[3, 2], [1, 2], [3, 2]], [[[1], [0]], [[1, 1],
>                     [1, 0]], [[1]]]);
Error, aaa: Transducer: usage,
the size of the elements of the third or fourth argument does not coincide
with the first argument,
gap> f := Transducer(2, 2, [[3, 2], [1, 0], [3, 2]], [[[1], [0]], [[1, 1],
>                     [1, 0]], [[1], []]]);
Error, aaa: Transducer: usage,
the third argument contains invalid states,
gap> f := Transducer(3, 2, [[3, 2], [1, 2], [3, 2]], [[[1], [0]], [[1, 1],
>                     [1, 0]], [[1], []]]);
Error, aaa: Transducer: usage,
the size of the elements of the third or fourth argument does not coincide
with the first argument,
gap> f := Transducer(2, 2, [[3, 2], [3, 2]], [[[1], [0]], [[1, 1],
>                     [1, 0]], [[1], []]]);
Error, aaa: Transducer: usage,
the size of the third and fourth arguments must coincide,
gap> f := Transducer(2, 2, [], [[[1], [0]], [[1, 1], [1, 0]], [[1], []]]);
Error, aaa: Transducer: usage,
the third and fourth arguments must be non-empty,
gap> f := Transducer(2, 4, [[1, 2], [1, 3], [1, 1]],
> [[[0], []], [[1], []], [[2], [3]]]);;
gap> TransducerFunction(f, PeriodicList([1, 0], [0, 1]), 1);          
[ [ 1, 0, / 1 ], 0 ]
gap> f!.TransducerFunction(1, 1);
Error, aaa: Transducer: usage,
the first argument must be a dense list
or a periodic list
gap> f!.TransducerFunction([1], 0);
Error, aaa: Transducer: usage,
the second argument must be a positive integer,
gap> f!.TransducerFunction([1], 6);
Error, aaa: Transducer: usage,
the second argument must not be greater than 3,
gap> f!.TransducerFunction([2], 3);
Error, aaa: Transducer: usage,
the first argument must be a list of integers in [ 0 .. 1 ],
gap> T := Transducer(2, 2, [[2, 2], [2, 2]], [[[], []], [[0],[0]]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 2 states.>
gap> M := Transducer(2, 2, [[2, 2], [2, 2]], [[PeriodicList([], [0]), PeriodicList([], [0])], [[], []]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 2 states.>

#T# IdentityTransducer
gap> T := IdentityTransducer(1);
<transducer with input alphabet on 1 symbol, output alphabet on 1 symbol, and 
1 state.>
gap> T := IdentityTransducer(2);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 1 state.>
gap> EqualTransducers(T, Transducer(2, 2, [[1, 1]], [[[0], [1]]]));
true
gap> T := IdentityTransducer(3);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 1 state.>
gap> EqualTransducers(T, Transducer(3, 3, [[1, 1, 1]], [[[0], [1], [2]]]));
true
gap> T := IdentityTransducer(4);
<transducer with input alphabet on 4 symbols, output alphabet on 
4 symbols, and 1 state.>
gap> EqualTransducers(T, Transducer(4, 4, [[1, 1, 1, 1]],
> [[[0], [1], [2], [3]]]));
true

#T# AlphabetChangeTransducer
gap> T := Transducer(2, 4, [[1, 2], [1, 3], [1, 1]],
> [[[0], []], [[1], []], [[2], [3]]]);;
gap> EqualTransducers(T, AlphabetChangeTransducer(2, 4));
true
gap> T3to5 := AlphabetChangeTransducer(3, 5);
<transducer with input alphabet on 3 symbols, output alphabet on 
5 symbols, and 4 states.>
gap> T5to3 := AlphabetChangeTransducer(5, 3);
<transducer with input alphabet on 5 symbols, output alphabet on 
3 symbols, and 2 states.>
gap> T3to3 := T3to5 * T5to3;
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 8 states.>
gap> T5to5 := T5to3 * T3to5;
<transducer with input alphabet on 5 symbols, output alphabet on 
5 symbols, and 8 states.>
gap> T3to3 = T3to3^0;
true
gap> T5to5 = T5to5^0;
true

#T# RandomTransducer
gap> T := RandomTransducer(3, 5);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 5 states.>
gap> T := RandomTransducer(4, 2);
<transducer with input alphabet on 4 symbols, output alphabet on 
4 symbols, and 2 states.>
gap> T := RandomTransducer(2, 5);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 5 states.>

#
gap> STOP_TEST("aaa package: standard/transducer.tst");
