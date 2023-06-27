#############################################################################
##
#W  transducer.gi
#Y  Copyright (C) 2017                                 Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods that relate to the objects in this package,
# including appropiate ViewObj functions.

InstallMethod(ViewObj, "for a transducer",
[IsGNSTransducer],
function(T)
  local state, sym1, sym2;
  if T!.States = 1 then
    state := "state";
  else
    state := "states";
  fi;
  if T!.InputAlphabet = 1 then
    sym1 := "symbol";
  else
    sym1 := "symbols";
  fi;
  if T!.OutputAlphabet = 1 then
    sym2 := "symbol";
  else
    sym2 := "symbols";
  fi;

  Print("<transducer with input alphabet on ", T!.InputAlphabet, " ", sym1,
        ", output alphabet on ", T!.OutputAlphabet, " ", sym2, ", and ",
        T!.States, " ", state, ".>");
end);

InstallMethod(GNSTransducer, "for two positive integers and two dense lists",
[IsPosInt, IsPosInt, IsDenseList, IsDenseList],
function(inalph, outalph, tranfunc, outfunc)
  local transducer, T, outputfinstates, i, j, state;

  if IsEmpty(tranfunc) or IsEmpty(outfunc) then
    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                  "the third and fourth arguments must be non-empty,");
  elif Size(tranfunc) <> Size(outfunc) then
    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                  "the size of the third and fourth arguments must coincide,");
  elif ForAny(tranfunc, x -> not IsDenseList(x) or
              ForAny(x, y -> not y in [1 .. Size(tranfunc)])) then
    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                  "the third argument contains invalid states,");
  elif ForAny(outfunc, x -> not IsDenseList(x) or
              ForAny(x, y -> not (IsDenseList(y) or IsPeriodicList(y)) or
                     ForAny(y, z -> not z in [0 .. outalph - 1]))) then
    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                  "the fourth argument contains invalid output,");
  elif ForAny(tranfunc, x -> Size(x) <> inalph) or
       ForAny(outfunc, x -> Size(x) <> inalph) then
    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                  "the size of the elements of the third or fourth argument ",
                  "does not coincide\nwith the first argument,");
  fi;

  outputfinstates := Set([]);
  for i in [1 .. Size(outfunc)] do
    for j in [1 .. Size(outfunc[i])] do
      if IsPeriodicList(outfunc[i][j]) and not Period(outfunc[i][j]) = [] then
        CompressPeriodicList(outfunc[i][j]);
	AddSet(outputfinstates, tranfunc[i][j]);
      fi;
      outfunc[i][j] := PeriodicList(outfunc[i][j]);
    od;
  od;

  for state in outputfinstates do
    if ForAny(outfunc[state], x -> not x = []) then
       ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                "the given object has output after an infinite ",
                "periodic output,");
    fi;
    for i in tranfunc[state] do
      if not i in outputfinstates then
        Add(outputfinstates, i);
      fi;
    od;
  od;

  transducer := function(input, state)
                  local ialph, oalph, tfunc, ofunc, output, n, statesvisited,
                        loopstart, loopend, newstate, reachlooppart, looppart;
                  ialph := [0 .. inalph - 1];
                  oalph := [0 .. outalph - 1];
                  tfunc := tranfunc;
                  ofunc := outfunc;
                  output := PeriodicList([]);

                  if not (IsDenseList(input) or IsPeriodicList(input)) then
                    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                                  "the first argument must be a dense list\n",
				  "or a periodic list");
                  elif not IsPosInt(state) then
                    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                                  "the second argument must be a positive ",
                                  "integer,");
                  elif state > Size(tfunc) then
                    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                                  "the second argument must not be greater ",
                                  "than ", Size(tfunc), ",");
                  elif ForAny(input, x -> not x in ialph) then
                    ErrorNoReturn("aaa: GNSTransducer: usage,\n",
                                  "the first argument must be a list of ",
                                  "integers in ", ialph, ",");
                  fi;

		  if IsPeriodicList(input) and not Period(input) = [] then
                      statesvisited := [];
		      newstate := transducer(PrePeriod(input), state)[2];
                      loopstart := Position(statesvisited, newstate);
                      while loopstart = fail do
			Add(statesvisited, newstate);
			newstate := transducer(Period(input), newstate)[2];
                        loopstart := Position(statesvisited, newstate);
		      od;
		      loopend := Length(statesvisited);
                      reachlooppart := Concatenation(PrePeriod(input), 
                                                     Concatenation(List([1 .. loopstart], 
                                                                         x -> Period(input))));
                      looppart := Concatenation(List([loopstart .. loopend], x -> Period(input)));
                      output := PeriodicList(transducer(reachlooppart, state)[1], 
                                              transducer(looppart, newstate)[1]);
		      CompressPeriodicList(output);
                      return [output, 0];
                  fi;
		  
                  for n in input do
                    Append(output, ofunc[state][n + 1]);
                    state := tfunc[state][n + 1];
                  od;

                  return [output, state];
                end;

  T :=Objectify(NewType(NewFamily("GNSTransducer"), IsGNSTransducer and
                 IsAttributeStoringRep), rec(InputAlphabet := inalph,
                                             OutputAlphabet := outalph,
                                             States := Size(tranfunc),
                                             TransitionFunction := tranfunc,
					     ReachableWithInfiniteOutput := outputfinstates,
                                             OutputFunction := outfunc,
                                             GNSTransducerFunction := transducer));

  return T;
end);

InstallMethod(GNSTransducerFunction, "for a transducer, a dense list and a posint",
[IsGNSTransducer, IsList, IsPosInt],
function(T, input, state)
  return T!.GNSTransducerFunction(input, state);
end);

InstallMethod(TransitionFunction, "for a transducer",
[IsGNSTransducer],
function(T)
  return T!.TransitionFunction;
end);

InstallMethod(OutputFunction, "for a transducer",
[IsGNSTransducer],
function(T)
  return T!.OutputFunction;
end);

InstallMethod(States, "for a transducer",
[IsGNSTransducer],
function(T)
  return [1 .. T!.States];
end);

InstallMethod(NrStates, "for a transducer",
[IsGNSTransducer],
function(T)
  return T!.States;
end);

InstallMethod(InputAlphabet, "for a transducer",
[IsGNSTransducer],
function(T)
  return [0 .. T!.InputAlphabet - 1];
end);

InstallMethod(OutputAlphabet, "for a transducer",
[IsGNSTransducer],
function(T)
  return [0 .. T!.OutputAlphabet - 1];
end);

InstallMethod(NrOutputSymbols, "for a transducer",
[IsGNSTransducer],
function(T)
  return T!.OutputAlphabet;
end);

InstallMethod(NrInputSymbols, "for a transducer",
[IsGNSTransducer],
function(T)
  return T!.InputAlphabet;
end);

InstallMethod(IdentityGNSTransducer, "for a positive integer",
[IsPosInt],
function(AlphSize)
  return GNSTransducer(AlphSize,AlphSize,[List([1 .. AlphSize], x -> 1)],
                    [List([0 .. AlphSize - 1], x-> [x])]);
end);

InstallMethod(AlphabetChangeGNSTransducer, "for two positive integers",
[IsPosInt, IsPosInt],
function(n,m)
  local 2tok;
  2tok := function(k)
    local i, outputfunction, transitionfunction;
    if k = 2 then
      return IdentityGNSTransducer(2);
    fi;
    transitionfunction := List([2 .. k], x-> [1, x]);
    transitionfunction[k - 1][2] := 1;
    outputfunction := List([0 .. k - 2], x -> [[x],[]]);
    outputfunction[k-1][2] := [k-1];
    return GNSTransducer(2, k, transitionfunction, outputfunction);
  end;
  return InverseGNSTransducer(2tok(n)) * 2tok(m);
end);

InstallMethod(RandomGNSTransducer,"gives random transducers",
[IsPosInt,IsPosInt],
function(AlphSize,NrStates)
	local i, j, k, OutputLength, Pi, Lambda;
	Pi:= [];
	Lambda:= [];
	for i in [1 .. NrStates] do
	   Add(Pi,[]);
	   Add(Lambda,[]);
	   for j in [1 .. AlphSize] do
		Add(Pi[i],Random([1 .. NrStates]));
		OutputLength:= 0;
		if not Random([1,2,3,4]) = 1 then
			OutputLength := OutputLength + 1;
			while Random([1,2]) = 1 do
				OutputLength := OutputLength + 1;
			od;
		fi;
		Add(Lambda[i],[]);
		for k in [1 .. OutputLength] do
			Add(Lambda[i][j],Random([0 .. AlphSize - 1]));
		od;
	   od;
	od;
	return GNSTransducer(AlphSize,AlphSize,Pi,Lambda);
end);
