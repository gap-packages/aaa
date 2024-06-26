############################################################################
##
#W  standard/toperations.tst
#Y  Copyright (C) 2017                                 Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("aaa package: standard/toperations.tst");
gap> LoadPackage("aaa", false);;

#T# InverseGNSTransducer
gap> f := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], [1]],
>                      [[0, 0], [], [1]], [[0, 2], [2], [0, 1]]]);;
gap> g := InverseGNSTransducer(f);;
gap> w := GNSTransducerFunction(f, [0, 1], 1)[1];
[ 2, 0 ]
gap> GNSTransducerFunction(g, w, 1)[1];
[ 0, 1 ]
gap> T := GNSTransducer(2, 2, [[2, 4], [3, 6], [3, 2], [5, 7], [5, 4], [6, 6],  
>  [7, 7]], [[[0], []], [[0, 1], [1, 0, 1]], [[1, 1, 1], [1, 0]], [[0, 0],
>  [0, 1, 0]], [[0, 0, 0], [1, 1]], [[0], [1]], [[0], [1]]]);;
gap> T^-1;
Error, aaa: ^: usage,
the given transducer must be bijective

#T# GNSTransducerProduct
gap> f := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], [1]],
>                      [[0, 0], [], [1]], [[0, 2], [2], [0, 1]]]);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 3 states.>
gap> t := AlphabetChangeGNSTransducer(2, 3);
<transducer with input alphabet on 2 symbols, output alphabet on 
3 symbols, and 2 states.>
gap> GNSTransducerProduct(t, t);            
Error, aaa: GNSTransducerProduct: usage,
the output alphabet of the first argument must be the input alphabet
of the second argument,
gap> t^2;
Error, aaa: ^: usage,
the given transducer must have the same domain and range
gap> ff := GNSTransducerProduct(f, f);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 9 states.>

#T# RemoveIncompleteResponseFromStates
gap> T := GNSTransducer(2, 2, [[2, 2], [3, 3], [3, 3]], [[[0], [1]], [[0], [1]], [[0], [0]]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 3 states.>
gap> M:= RemoveIncompleteResponseFromStates(T);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 4 states.>
gap> OutputFunction(M);
[ [ [ 0 ], [ 1 ] ], [ [ 0 ], [ 1 ] ], [ [/ 0 ], [ 1, / 0 ] ], [ [  ], [  ] ] ]
gap> TransitionFunction(M);
[ [ 3, 3 ], [ 3, 3 ], [ 4, 4 ], [ 4, 4 ] ]
gap> t := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], []],
>                           [[1, 0, 0], [1], [1]], [[0, 2], [2], [0]]]);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 3 states.>
gap> p := RemoveIncompleteResponseFromStates(t);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 4 states.>
gap> GNSTransducerFunction(t, [2], 1)[1]; GNSTransducerFunction(t, [1], 2)[1];
[  ]
[ 1 ]
gap> GNSTransducerFunction(p, [2], 2)[1];
[ 1 ]
gap> T := GNSTransducer(2, 2, [[1, 2], [2, 2]], [[[1], [1, 1]], [[1], [1, 1]]]);;
gap> R := RemoveIncompleteResponseFromStates(T);;
gap> OutputFunction(R);
[ [ [/ 1 ], [/ 1 ] ], [ [  ], [  ] ], [ [  ], [  ] ] ]
gap> TransitionFunction(R);
[ [ 2, 3 ], [ 2, 3 ], [ 3, 3 ] ]
gap> T := GNSTransducer(2, 3, [[2, 3], [2, 3], [2, 3]], [[[2, 0], [2]],
> [[1, 1, 0], [1, 1]], [[0], [0, 1, 1]]]);;
gap> R := RemoveIncompleteResponseFromStates(T);;
gap> OutputFunction(R);
[ [ [ 2, / 0, 1, 1 ], [ 2, / 0, 1, 1 ] ], [ [  ], [  ] ], [ [  ], [  ] ], 
  [ [  ], [  ] ] ]
gap> TransitionFunction(R);
[ [ 3, 4 ], [ 3, 4 ], [ 3, 4 ], [ 3, 4 ] ]
gap> T := GNSTransducer(3, 3, [[1, 2, 2], [3, 2, 3], [1, 3, 3]], [[[2, 2],
> [2, 2, 2, 2, 1, 1, 0, 1], [2, 0]], [[], [0, 1], [2]], [[1], [1], []]]);;
gap> RemoveIncompleteResponseFromStates(T);
Error, aaa: RemoveIncompleteResponseFromStates: usage,
the given transducer must be nondegenerate 

#T# RemoveInaccessibleStates
gap> f := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], [1]],
>                      [[0, 0], [], [1]], [[0, 2], [2], [0, 1]]]);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 3 states.>
gap> ff := GNSTransducerProduct(f, f);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 9 states.>
gap> m := RemoveInaccessibleStates(ff);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 6 states.>

#T# CopyGNSTransducerWithInitialState
gap> f := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], [1]],
>                      [[0, 0], [], [1]], [[0, 2], [2], [0, 1]]]);;
gap> p := CopyGNSTransducerWithInitialState(f, 3);;
gap> GNSTransducerFunction(f, [0, 1, 0], 3);
[ [ 0, 2, 0, 2 ], 1 ]
gap> GNSTransducerFunction(p, [0, 1, 0], 1);
[ [ 0, 2, 0, 2 ], 2 ]
gap> p := CopyGNSTransducerWithInitialState(f, 4);; 
Error, aaa: ChangeInitialState: usage,
the second argument is not a state of the first argument,

#T# IsInjectiveGNSTransducer
gap> T := GNSTransducer(2, 2, [[2, 4], [3, 6], [3, 2], [5, 7], [5, 4], [6, 6],
>  [7, 7]], [[[0], []], [[0, 1], [1, 0, 1]], [[1, 1, 1], [1, 0]], [[0, 0],
>  [0, 1, 0]], [[0, 0, 0], [1, 1]], [[0], [1]], [[0], [1]]]);;
gap> IsInjectiveGNSTransducer(T);
false
gap> f := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], [1]],
> [[0, 0], [], [1]], [[0, 2], [2], [0, 1]]]);;
gap> IsInjectiveGNSTransducer(f);
true
gap> T := GNSTransducer(2, 2, [[3, 2], [4, 4], [4, 4], [4, 4]], [[[], []],
> [[0, 1], [1, 1]], [[0, 0], [1, 0]], [[0], [1]]]);;
gap> IsInjectiveGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[1, 2], [3, 4], [1, 5], [1, 6], [3, 4], [1, 6]],
> [[[0, 1, 0], []], [[1, 1], [0]], [[0, 1, 0], []], [[], [1, 0, 1, 0]],
> [[1], [0]], [[], [1, 0]]]);;
gap> IsInjectiveGNSTransducer(T);
true
gap> T := GNSTransducer(2, 4, [[1, 2], [1, 3], [1, 1]], [[[0], []], [[1], []],
> [[2], [3]]]);;
gap> IsInjectiveGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[3, 3], [2, 3], [3, 2]], [[[0, 1], []],
> [[1], [1, 0, 0, 1, 0, 1]], [[1, 1], [0, 1]]]);;
gap> IsInjectiveGNSTransducer(T);
false
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 1]], [[[0], [1]], [[], []]]);;
gap> IsInjectiveGNSTransducer(T);
false
gap> T := GNSTransducer(2, 2, [[1, 1]], [[[], []]]);;
gap> IsInjectiveGNSTransducer(T);
Error, aaa: IsInjectiveGNSTransducer: usage,
the given transducer must be nondegenerate 

#T# IsSurjectiveGNSTransducer
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsSurjectiveGNSTransducer(T);
true
gap> P := GNSTransducer(2, 2, [[3, 4], [3, 2], [1, 3], [2, 4]], [[[1], [0]],
> [[], [1]], [[1], [0]], [[1, 0], [1]]]);;
gap> IsSurjectiveGNSTransducer(P);
false
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 1]], [[[0], [1]], [[], []]]);;
gap> IsSurjectiveGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[3, 3], [2, 3], [3, 2]], [[[0, 1], []],
> [[1], [1, 0, 0, 1, 0, 1]], [[1, 1], [0, 1]]]);;
gap> IsSurjectiveGNSTransducer(T);
false
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 1]], [[[0], [1]], [[], []]]);;
gap> IsSurjectiveGNSTransducer(T);
true
gap> T := GNSTransducer(5, 3, [[1, 1, 1, 1, 1], [1, 1, 1, 1, 2]],
> [[[0], [1], [2, 0], [2, 1], [2, 2]], [[1], [2, 0], [2, 1],
> [2, 2, 0], [2, 2]]]);;
gap> IsSurjectiveGNSTransducer(T);
true
gap> T := GNSTransducer(3, 3, [[3, 2, 3], [1, 3, 1], [1, 3, 1]],
> [[[1, 1], [0], [2]], [[1], [1], []], [[2, 0], [0, 1, 0], []]]);;
gap> IsSurjectiveGNSTransducer(T);
false
gap> T := GNSTransducer(2, 2, [[1, 1]], [[[], []]]);;
gap> IsSurjectiveGNSTransducer(T);
Error, aaa: IsSurjectiveGNSTransducer: usage,
the given transducer must be nondegenerate 

#T# GNSTransducerImageAutomaton
gap> T := GNSTransducer(3, 3, [[3, 2, 3], [1, 3, 1], [1, 3, 1]],
> [[[1, 1], [0], [2]], [[1], [1], []], [[2, 0], [0, 1, 0], []]]);;
gap> String(GNSTransducerImageAutomaton(T));
"Automaton(\"epsilon\",7,\"012@\",[ [ [ 2 ], [ ], [ 6 ], [ ], [ 1 ], [ ], [ 3 \
] ], [ [ 4 ], [ 1, 3 ], [ ], [ 3 ], [ ], [ 7 ], [ ] ], [ [ 3 ], [ ], [ 5 ], [ \
], [ ], [ ], [ ] ], [ [ ], [ 1 ], [ 1 ], [ ], [ ], [ ], [ ] ] ],[ 1 ],[ 1 .. 7\
 ]);;"
gap> T := GNSTransducer(2, 2, [[2, 3], [5, 1], [4, 5], [2, 5], [3, 3]],
> [[[0], [0]], [[0, 1, 0, 0, 0, 1], [0, 0, 0]], [[], [0]],
> [[], []], [[0], [0]]]);;
gap> String(GNSTransducerImageAutomaton(T));
"Automaton(\"epsilon\",12,\"01@\",[ [ [ 2, 3 ], [ 6, 11 ], [ 5 ], [ ], [ 3 ], \
[ ], [ 8 ], [ 9 ], [ 10 ], [ ], [ 12 ], [ 1 ] ], [ [ ], [ ], [ ], [ ], [ ], [ \
7 ], [ ], [ ], [ ], [ 5 ], [ ], [ ] ], [ [ ], [ ], [ 4 ], [ 2, 5 ], [ ], [ ], \
[ ], [ ], [ ], [ ], [ ], [ ] ] ],[ 1 ],[ 1 .. 12 ]);;"
gap> T := GNSTransducer(3, 4, [[1, 1, 2], [1, 1, 3], [1, 1, 1]],
> [[[0], [1], []], [[2], [3,0], [3]], [[1], [2], [3]]]);;
gap> String(GNSTransducerImageAutomaton(T));
"Automaton(\"epsilon\",4,\"0123@\",[ [ [ 1 ], [ ], [ ], [ 1 ] ], [ [ 1 ], [ ],\
 [ 1 ], [ ] ], [ [ ], [ 1 ], [ 1 ], [ ] ], [ [ ], [ 3, 4 ], [ 1 ], [ ] ], [ [ \
2 ], [ ], [ ], [ ] ] ],[ 1 ],[ 1 .. 4 ]);;"

#T# GNSTransducerConstantStateOutputs
gap> T := GNSTransducer(2, 2, [[1, 2], [2, 2]], [[[1], [1, 1]], [[1], [1, 1]]]);;
gap> GNSTransducerConstantStateOutputs(T);
[ [ 1, 2 ], [ [/ 1 ], [/ 1 ] ] ]
gap> T := GNSTransducer(2, 3, [[2, 3], [2, 3], [2, 3]], [[[2, 0], [2]],
> [[1, 1, 0], [1, 1]], [[0], [0, 1, 1]]]);;
gap> GNSTransducerConstantStateOutputs(T);
[ [ 1, 2, 3 ], [ [ 2, / 0, 1, 1 ], [/ 1, 1, 0 ], [/ 0, 1, 1 ] ] ]
gap> T := GNSTransducer(2, 2, [[3, 3], [1, 1], [2, 1]], [[[1], []],
> [[0, 1, 1, 1], [0]], [[0], [1]]]);;
gap> GNSTransducerConstantStateOutputs(T);
[ [  ], [  ] ]
gap> T := GNSTransducer(5, 3, [[1, 1, 1, 1, 1], [1, 1, 1, 1, 2]],
> [[[0], [1], [2, 0], [2, 1], [2, 2]], [[1], [2, 0], [2, 1],
> [2, 2, 0], [2, 2]]]);;
gap> GNSTransducerConstantStateOutputs(T);
[ [  ], [  ] ]

#T# IsDegenerateGNSTransducer
gap> T := GNSTransducer(2, 2, [[2, 2], [1, 3], [2, 1]], [[[0], [0]],
> [[0, 1, 0, 0, 0, 1], []], [[0, 0, 0], [0]]]);;
gap> IsDegenerateGNSTransducer(T);
false
gap> T := GNSTransducer(3, 3, [[1, 2, 2], [3, 2, 3], [1, 3, 3]], [[[2, 2],
> [2, 2, 2, 2, 1, 1, 0, 1], [2, 0]], [[], [0, 1], [2]], [[1], [1], []]]);;
gap> IsDegenerateGNSTransducer(T);
true
gap> T := GNSTransducer(3, 3, [[1, 1, 3], [3, 1, 1], [2, 3, 2]],
> [[[0, 2, 2], [0], [2, 2]], [[], [0], [0]], [[2], [1, 2], []]]);;
gap> IsDegenerateGNSTransducer(T);
true
gap> T := GNSTransducer(5, 3, [[1, 1, 1, 1, 1], [1, 1, 1, 1, 2]],
> [[[0], [1], [2, 0], [2, 1], [2, 2]], [[1], [2, 0], [2, 1],
> [2, 2, 0], [2, 2]]]);;
gap> IsDegenerateGNSTransducer(T);
false

#T# CombineEquivalentStates
gap> T := GNSTransducer(3, 4, [[1, 2, 2], [3, 2, 2], [3, 2, 2]],
> [[[1], [], [1, 0]], [[2], [2], [2]], [[1], [], [1, 0]]]);;
gap> CombineEquivalentStates(T);
<transducer with input alphabet on 3 symbols, output alphabet on 
4 symbols, and 2 states.>
gap> T := GNSTransducer(3, 3, [[2, 3, 2], [2, 1, 4], [1, 4, 2], [4, 2, 3]],
> [[[2], [], [2]], [[2], [0, 1], [1, 2, 0]], [[0], [], [1]], [[], [1], [0]]]);;
gap> CombineEquivalentStates(T);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 4 states.>
gap> T := GNSTransducer(2, 2, [[2, 4], [3, 2], [5, 4], [1, 5], [1, 4]],
> [[[0], [1, 0]], [[], [1]], [[0], [1 , 0]], [[], [1]], [[], [1]]]);;
gap> CombineEquivalentStates(T);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 2 states.>
gap> T := GNSTransducer(2, 2, [[1, 4], [3, 2], [5, 4], [3, 5], [1, 4]],
> [[[0], [1, 0]], [[], [1]], [[0], [1 , 0]], [[], [1]], [[], [1]]]);;
gap> CombineEquivalentStates(T);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 5 states.>

#T# MinimalGNSTransducer
gap> T := GNSTransducer(3, 3, [[2, 2, 1], [4, 3, 1], [4, 2, 3], [1, 1, 4]],
> [[[2], [2, 0], [2]], [[2, 2, 0], [], [0, 1]], [[], [0], [2]],
> [[2, 1], [1, 0, 1], [1, 2]]]);;
gap> M := MinimalGNSTransducer(T);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 5 states.>
gap> OutputFunction(M);
[ [ [ 2 ], [ 2, 0 ], [ 2, 2 ] ], [ [ 2, 2, 0 ], [  ], [ 0, 1, 2 ] ], 
  [ [  ], [ 0 ], [ 2 ] ], [ [ 2, 1, 2 ], [ 1, 0, 1, 2 ], [ 1, 2 ] ], 
  [ [  ], [ 0 ], [ 2 ] ] ]
gap> TransitionFunction(M);
[ [ 2, 2, 3 ], [ 4, 5, 3 ], [ 2, 2, 3 ], [ 3, 3, 4 ], [ 4, 2, 5 ] ]
gap> T := GNSTransducer(2, 2, [[2, 2], [3, 1], [3, 3], [5, 2], [2, 1]],
> [[[1, 0], [0, 0]], [[1], []], [[0], [1]], [[1], [1]], [[], [0, 0]]]);;
gap> M := MinimalGNSTransducer(T);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 3 states.>
gap> OutputFunction(M);
[ [ [ 1, 0 ], [ 0, 0 ] ], [ [ 1 ], [  ] ], [ [ 0 ], [ 1 ] ] ]
gap> TransitionFunction(M);
[ [ 2, 2 ], [ 3, 1 ], [ 3, 3 ] ]
gap> T := GNSTransducer(2, 2, [[1, 1]], [[[], []]]);;
gap> MinimalGNSTransducer(T);
Error, aaa: MinimalGNSTransducer: usage,
the given transducer must be nondegenerate 
gap> T := GNSTransducer(2, 2, [[2, 2], [2, 2]], [[[], []], [[0],[0]]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 2 states.>
gap> M := MinimalGNSTransducer(T);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 2 states.>

#T# IsomorphicInitialGNSTransducers
gap> T := GNSTransducer(2, 3, [[1, 3], [2, 3], [3, 3]], [[[1], [2]], [[1], [2]],
> [[0, 0], [1, 0]]]);;
gap> T2 := CopyGNSTransducerWithInitialState(T, 2);;
gap> T3 := CopyGNSTransducerWithInitialState(T, 3);;
gap> T4 := CopyGNSTransducerWithInitialState(T3, 3);;
gap> T5 := GNSTransducer(2, 2, [[1, 1], [2, 2], [3, 3]], [[[], []], [[], []], [[], []]]);;
gap> T6 := GNSTransducer(3, 2, [[1, 1, 1], [2, 2, 2], [3, 3, 3]], [[[], [], []], [[], [], []], [[], [], []]]);;
gap> IsomorphicInitialGNSTransducers(T, T2);
true
gap> IsomorphicInitialGNSTransducers(T, T3);
false
gap> IsomorphicInitialGNSTransducers(T, T4);
true
gap> IsomorphicInitialGNSTransducers(T, T5);
false
gap> IsomorphicInitialGNSTransducers(T, T6);
false

#T# OmegaEquivalentTransduces "="
gap> T := GNSTransducer(2, 2, [[2, 2], [3, 1], [3, 3], [5, 2], [2, 1]],
> [[[1, 0], [0, 0]], [[1], []], [[0], [1]], [[1], [1]], [[], [0, 0]]]);;
gap> M := MinimalGNSTransducer(T);;
gap> OmegaEquivalentGNSTransducers(T, M);
true
gap> T = M;
true
gap> T := GNSTransducer(2, 3, [[1, 3], [2, 3], [3, 3]], [[[1], [2]], [[1], [2]],
> [[0, 0], [1, 0]]]);;
gap> T2 := CopyGNSTransducerWithInitialState(T, 2);;
gap> T3 := CopyGNSTransducerWithInitialState(T, 3);;
gap> T4 := CopyGNSTransducerWithInitialState(T3, 3);;
gap> OmegaEquivalentGNSTransducers(T, T2);
true
gap> OmegaEquivalentGNSTransducers(T, T3);
false
gap> OmegaEquivalentGNSTransducers(T, T4);
true
gap> T = T4;
true
gap> T := GNSTransducer(3, 4, [[1, 3, 2], [2, 1, 4], [1, 1, 1], [3, 2, 1]],
> [[[1], [3], [0]], [[1, 1], [], [3, 0]], [[1, 3], [2], [3, 2]],
> [[0], [0], [0]]]);;
gap> T2 := GNSTransducer(3, 4, [[1, 2, 2], [2, 1, 4], [1, 1, 1], [3, 2, 1]],
> [[[1], [3], [0]], [[1, 1], [], [3, 0]], [[1, 3], [2], [3, 2]],
> [[0], [0], [0]]]);;
gap> OmegaEquivalentGNSTransducers(T, T2);
false
gap> T = T2;
false

#T# EqualGNSTransducers
gap> T := GNSTransducer(2, 3, [[1, 3], [2, 3], [3, 3]], [[[1], [2]], [[1], [2]],
> [[0, 0], [1, 0]]]);;
gap> T2 := CopyGNSTransducerWithInitialState(T, 2);;
gap> T3 := CopyGNSTransducerWithInitialState(T, 3);;
gap> T4 := CopyGNSTransducerWithInitialState(T3, 3);;
gap> EqualGNSTransducers(T, T2);
true
gap> EqualGNSTransducers(T, T3);
false
gap> EqualGNSTransducers(T, T4);
false

#T# IsBijectiveGNSTransducer
gap> T := GNSTransducer(2, 2, [[2, 4], [3, 6], [3, 2], [5, 7], [5, 4], [6, 6],
>  [7, 7]], [[[0], []], [[0, 1], [1, 0, 1]], [[1, 1, 1], [1, 0]], [[0, 0],
>  [0, 1, 0]], [[0, 0, 0], [1, 1]], [[0], [1]], [[0], [1]]]);;
gap> IsBijectiveGNSTransducer(T);
false
gap> f := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], [1]],
> [[0, 0], [], [1]], [[0, 2], [2], [0, 1]]]);;
gap> IsBijectiveGNSTransducer(f);
true
gap> T := GNSTransducer(2, 2, [[3, 2], [4, 4], [4, 4], [4, 4]], [[[], []],
> [[0, 1], [1, 1]], [[0, 0], [1, 0]], [[0], [1]]]);;
gap> IsBijectiveGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[1, 2], [3, 4], [1, 5], [1, 6], [3, 4], [1, 6]],
> [[[0, 1, 0], []], [[1, 1], [0]], [[0, 1, 0], []], [[], [1, 0, 1, 0]],
> [[1], [0]], [[], [1, 0]]]);;
gap> IsBijectiveGNSTransducer(T);
false
gap> T := GNSTransducer(2, 4, [[1, 2], [1, 3], [1, 1]], [[[0], []], [[1], []],
> [[2], [3]]]);;
gap> IsBijectiveGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[3, 3], [2, 3], [3, 2]], [[[0, 1], []],
> [[1], [1, 0, 0, 1, 0, 1]], [[1, 1], [0, 1]]]);;
gap> IsBijectiveGNSTransducer(T);
false
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 1]], [[[0], [1]], [[], []]]);;
gap> IsBijectiveGNSTransducer(T);
false
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsBijectiveGNSTransducer(T);
true
gap> P := GNSTransducer(2, 2, [[3, 4], [3, 2], [1, 3], [2, 4]], [[[1], [0]],
> [[], [1]], [[1], [0]], [[1, 0], [1]]]);;
gap> IsBijectiveGNSTransducer(P);
false
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 1]], [[[0], [1]], [[], []]]);;
gap> IsBijectiveGNSTransducer(T);
false

#T# Powers
gap> T := GNSTransducer(2, 4, [[1, 2], [1, 3], [1, 1]], [[[0], []], [[1], []],
> [[2], [3]]]);;
gap> EqualGNSTransducers(T^1, T);
true
gap> T^-1;
<transducer with input alphabet on 4 symbols, output alphabet on 
2 symbols, and 1 state.>
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 1]], [[[0], [1]], [[], []]]);;
gap> T2 := GNSTransducer(2, 2, [[1, 4], [1, 3], [1, 1], [2, 2]],
> [[[0], [1]], [[], []], [[], []], [[], []]]);;
gap> EqualGNSTransducers(T^2, T2);
true
gap> f := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], [1]],
> [[0, 0], [], [1]], [[0, 2], [2], [0, 1]]]);;
gap> f^-3 * f^2 = f^-1;
true

#T# IsMinimalGNSTransducer
gap> T := GNSTransducer(3, 3, [[3, 4, 3], [1, 3, 1], [1, 3, 3], [2, 2, 3]],
> [[[2], [2], [0]], [[2, 0, 2, 1], [0, 0], []], [[], [2, 0], [1]],
> [[], [2], [1, 1, 0, 1, 0]]]);;
gap> IsMinimalGNSTransducer(T);
true
gap> M := MinimalGNSTransducer(T);;
gap> IsMinimalGNSTransducer(M);
true
gap> IsMinimalGNSTransducer(CopyGNSTransducerWithInitialState(M, 2));
true
gap> T := GNSTransducer(3, 3, [[3, 2, 1], [3, 3, 1], [2, 2, 1]],
> [[[1, 0], [], [0]], [[], [], [0]], [[2], [2, 2, 1], [2, 2]]]);;
gap> IsMinimalGNSTransducer(T);
false

#T# IsSynchronousGNSTransducer
gap> T := GNSTransducer(3, 3, [[1, 2, 1], [3, 3, 3], [1, 3, 2]],
> [[[1], [2], [1]], [[0], [1], [2]], [[0], [0], [1]]]);;
gap> IsSynchronousGNSTransducer(T);
true
gap> T := GNSTransducer(3, 3, [[1, 2, 1], [3, 3, 3], [1, 3, 2]],
> [[[1], [2], [1]], [[0], [1], [2, 1]], [[0], [0], [1]]]);;
gap> IsSynchronousGNSTransducer(T);
false
gap> T := GNSTransducer(2, 2, [[1, 2], [4, 3], [1, 2], [2, 1]],
> [[[1], [1]], [[1], [0]], [[0], [0]], [[1], [0]]]);;
gap> IsSynchronousGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[1, 2], [4, 3], [1, 2], [2, 1]],
> [[[], [1]], [[1], [0]], [[0], [1]], [[0], [0]]]);;
gap> IsSynchronousGNSTransducer(T);
false

#T# Conjugation
gap> T := GNSTransducer(3, 3, [[2, 3, 2], [1, 2, 1], [1, 2, 1]],
> [[[1, 1], [0], [2]], [[2, 0], [0, 1, 0], []], [[1], [1], []]]);;
gap> 3to2 := AlphabetChangeGNSTransducer(3, 2);;
gap> 2to3 := AlphabetChangeGNSTransducer(2, 3);;
gap> (T^3to2)^2to3 = T;
true
gap> C1 := GNSTransducer(3, 3, [[1, 1, 2], [1, 3, 2], [1, 1, 2]], [[[2], [0], [1]],
> [[0, 0], [], [1]], [[0, 2], [2], [0, 1]]]);;
gap> C2 := GNSTransducer(3, 3, [[1, 1, 1]], [[[0], [2], [1]]]);;
gap> T^(C1*C2)= (T^C1)^C2;
true
gap> T := GNSTransducer(2, 2, [[2, 4], [3, 6], [3, 2], [5, 7], [5, 4], [6, 6],
>  [7, 7]], [[[0], []], [[0, 1], [1, 0, 1]], [[1, 1, 1], [1, 0]], [[0, 0],
>  [0, 1, 0]], [[0, 0, 0], [1, 1]], [[0], [1]], [[0], [1]]]);;
gap> T^T;
fail

#T# GNSTransducerOrder
gap> T := IdentityGNSTransducer(5);;
gap> GNSTransducerOrder(T);
1
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> GNSTransducerOrder(T);
2
gap> T := GNSTransducer(5, 5, [[1, 1, 1, 1, 1]], [[[1], [0], [3], [4], [2]]]);;
gap> GNSTransducerOrder(T);
6
gap> GNSTransducerOrder(T^-1);
6
gap> GNSTransducerOrder(T^-2);
3
gap> T := GNSTransducer(2, 2, [[3, 3], [2, 3], [3, 2]], [[[0, 1], []],
> [[1], [1, 0, 0, 1, 0, 1]], [[1, 1], [0, 1]]]);;
gap> GNSTransducerOrder(T);
Error, aaa: GNSTransducerOrder: usage,
the given transducer must be bijective

#T# GNSTransducerSynchronizingLength
gap> T := IdentityGNSTransducer(4);;
gap> GNSTransducerSynchronizingLength(T);
0
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> GNSTransducerSynchronizingLength(T);
2
gap> T := GNSTransducer(3, 3, [[1, 2, 1], [3, 3, 3], [1, 3, 2]],
> [[[1], [2], [1]], [[0], [1], [2, 1]], [[0], [0], [1]]]);;
gap> GNSTransducerSynchronizingLength(T);
infinity

#T# IsSynchronizingGNSTransducer
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsSynchronizingGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
gap> IsSynchronizingGNSTransducer(T);
true
gap> T := GNSTransducer(3, 3, [[1, 2, 1], [3, 3, 3], [1, 3, 2]],
> [[[1], [2], [1]], [[0], [1], [2, 1]], [[0], [0], [1]]]);;
gap> IsSynchronizingGNSTransducer(T);
false

#T# IsBisynchronizingGNSTransducer
gap> T := IdentityGNSTransducer(4);;
gap> IsBisynchronizingGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsBisynchronizingGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
gap> IsBisynchronizingGNSTransducer(T);
false
gap> T := GNSTransducer(3, 3, [[1, 2, 1], [3, 3, 3], [1, 3, 2]],
> [[[1], [2], [1]], [[0], [1], [2, 1]], [[0], [0], [1]]]);;
gap> IsBisynchronizingGNSTransducer(T);
false

#
gap> STOP_TEST("aaa package: standard/toperations.tst");
