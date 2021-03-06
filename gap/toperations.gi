#############################################################################
##
#W  toperations.gi
#Y  Copyright (C) 2017                               Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for operations that relate to transducers.

InstallMethod(InverseTransducer, "for a transducer",
[IsTransducerOrRTransducer],
function(T)
  local newstates, ntfunc, nofunc, n, x, q, word, preimage, newstate, tdcrf,
        readletters;
  ntfunc := [[]];
  nofunc := [[]];
  newstates := [[[], 1]];

  n := 0;
  for q in newstates do
    n := n + 1;
    if IsRTransducer(T) and n = 1 then
      readletters := OutputRoots(T);
    else
      readletters := OutputAlphabet(T);
    fi;
    for x in readletters do
      word := [];
      Append(word, q[1]);
      Append(word, [x]);
      preimage := GreatestCommonPrefix(PreimageConePrefixes(word, q[2], T));
      tdcrf := TransducerFunction(T, preimage, q[2]);

      newstate := [Minus(word, tdcrf[1]), tdcrf[2]];

      if not newstate in newstates then
        Add(newstates, newstate);
        Add(ntfunc, []);
        Add(nofunc, []);
      fi;

      ntfunc[n][x + 1] := Position(newstates, newstate);
      nofunc[n][x + 1] := preimage;
    od;
  od;
  if IsTransducer(T) then
    return Transducer(NrOutputSymbols(T), NrInputSymbols(T), ntfunc, nofunc);
  else
    return RTransducer(NrOutputRoots(T), NrInputRoots(T),
                       NrOutputSymbols(T), NrInputSymbols(T), ntfunc, nofunc);
  fi;
end);

InstallMethod(TransducerProduct, "for two transducers",
[IsTransducerOrRTransducer, IsTransducerOrRTransducer],
function(tdcr1, tdcr2)
  local newstates, newstate, ntfun, nofun, tducerf, word, x, y, q, n;
  newstates := [];
  ntfun := [];
  nofun := [];

  if NrOutputSymbols(tdcr1) <> NrInputSymbols(tdcr2) then
    ErrorNoReturn("aaa: TransducerProduct: usage,\n",
                  "the output alphabet of the first argument must be the ",
                  "input alphabet\nof the second argument,");
  fi;
  if not IsRTransducer(tdcr1) = IsRTransducer(tdcr2) then
  ErrorNoReturn("aaa: TransducerProduct: usage,\n",
                  "one of the transducers has roots and the other",
                  "doesn't,");
  fi;
  if IsRTransducer(tdcr1) and OutputRoots(tdcr1) <> InputRoots(tdcr2) then
    ErrorNoReturn("aaa: TransducerProduct: usage,\n",
                  "the output roots of the first argument must be the ",
                  "input roots\nof the second argument,");
  fi;

  for x in States(tdcr1) do
    for y in States(tdcr2) do
      Add(newstates, [x, y]);
      Add(ntfun, []);
      Add(nofun, []);
    od;
  od;

  n := 0;
  for q in newstates do
    n := n + 1;
    if IsRTransducer(tdcr1) and
       ((q[1] = 1 and not q[2] in RootStates(tdcr2))
        or (q[2] = 1 and not q[1] in RootStates(tdcr1))) then
      for x in [0 .. Size(OutputFunction(tdcr1)[q[1]]) - 1] do
        ntfun[n][x + 1] := 2;
        nofun[n][x + 1] := [];
      od;
    else
      for x in [0 .. Size(OutputFunction(tdcr1)[q[1]]) - 1] do
        word := OutputFunction(tdcr1)[q[1]][x + 1];
        tducerf := TransducerFunction(tdcr2, word, q[2]);
        newstate := [TransitionFunction(tdcr1)[q[1]][x + 1], tducerf[2]];
        ntfun[n][x + 1] := Position(newstates, newstate);
        nofun[n][x + 1] := tducerf[1];
      od;
    fi;
  od;

  if IsRTransducer(tdcr1) then
    return RTransducer(NrInputRoots(tdcr1), NrOutputRoots(tdcr2),
                       NrInputSymbols(tdcr1), NrOutputSymbols(tdcr2), ntfun,
                       nofun);
  else
    return Transducer(NrInputSymbols(tdcr1), NrOutputSymbols(tdcr2), ntfun,
                      nofun);
  fi;
end);

InstallMethod(\*, "for two transducers",
[IsTransducerOrRTransducer, IsTransducerOrRTransducer],
TransducerProduct);

InstallMethod(\^, "for a transducer and a positive integer",
[IsTransducerOrRTransducer, IsInt],
function(T, n)
  local flag, tducer, x;
  if n = 1 then
    return CopyTransducerWithInitialState(T, 1);
  fi;
  if n < 0 then
    if not IsBijectiveTransducer(T) then
      ErrorNoReturn("aaa: ^: usage,\n",
                  "the given transducer must be bijective");
    fi;
    return InverseTransducer(T)^-n;
  fi;
  if not InputAlphabet(T) = OutputAlphabet(T) then
    ErrorNoReturn("aaa: ^: usage,\n",
                  "the given transducer must have the same domain and range");
  fi;
  if IsRTransducer(T) then
    if not InputRoots(T) = OutputRoots(T) then
      ErrorNoReturn("aaa: ^: usage,\n",
                  "the given transducer must have the same domain and range");
    fi;
    if n = 0 then
      return IdentityRTransducer(NrInputRoots(T), NrInputSymbols(T));
    fi;
  else
    if n = 0 then
     return IdentityTransducer(Size(InputAlphabet(T)));
    fi;
  fi;

  tducer := CopyTransducerWithInitialState(T, 1);

  for x in [1 .. n - 1] do
    tducer := tducer * T;
  od;
 
  return tducer;
end);

InstallMethod(EqualTransducers, "for a pair of transducers",
[IsTransducer, IsTransducer],
function(T1,T2)
  return OutputFunction(T1)=OutputFunction(T2) and TransitionFunction(T1)=TransitionFunction(T2);
end);

InstallMethod(\^, "for a transducer and a bijective transducer",
[IsTransducer, IsTransducer],
function(T1,T2)
  if not IsBijectiveTransducer(T2) then
    return fail;
  fi;
  return T2^-1 * T1 * T2;
end);

InstallMethod(RemoveStatesWithIncompleteResponse, "for a transducer",
[IsTransducerOrRTransducer],
function(T)
  local ntfunc, nofunc, n, x, const, target, pos, badpref, neededcopies,
        i, stufftowrite, out, edgestopushfrom, edge, pushstring;

  if IsDegenerateTransducer(T) then
    ErrorNoReturn("aaa: RemoveStatesWithIncompleteResponce: usage,\n",
                  "the given transducer must be nondegenerate ");
  fi;

  const := List(TransducerConstantStateOutputs(T),x-> List(x,y->y));
  for i in [1 .. Size(const[2])] do
    const[2][i]:= const[2][i]{[1 .. Size(const[2][i])-2]};
    const[2][i]:= SplitString(const[2][i],"(");
    const[2][i][1]:= List([1 .. Size(const[2][i][1])], x -> Int(const[2][i][1]{[x]}));
    const[2][i][2]:= List([1 .. Size(const[2][i][2])], x -> Int(const[2][i][2]{[x]}));
  od;
  edgestopushfrom := [];
  ntfunc := [];
  nofunc := [];
  for x in [1 .. NrStates(T) + 1] do
    Add(ntfunc, []);
    Add(nofunc, []);
  od;
  for x in [0 .. Size(OutputFunction(T)[1]) - 1] do
    target := TransducerFunction(T, [x], 1)[2];
    ntfunc[1][x + 1] := target + 1;
    pos := Position(const[1], target);
    if pos = fail then
      nofunc[1][x + 1] := ImageConeLongestPrefix([x], 1, T);
    else
      out := TransducerFunction(T,[x],1)[1];
      nofunc[1][x + 1] := Concatenation(out, const[2][pos][1]);
      if out <> [] and const[2][pos][1] = [] and out[Size(out)] = const[2][pos][2][Size(const[2][pos][2])] then
        Add(edgestopushfrom, [1,x+1]);
      fi;
    fi;
  od;
  for n in [2 .. NrStates(T) + 1] do
    if not n - 1 in const[1] then
      for x in [0 .. Size(OutputFunction(T)[n - 1]) - 1] do
        target := TransducerFunction(T, [x], n - 1)[2];
        ntfunc[n][x + 1] := target + 1;
        pos := Position(const[1], target);
        if pos = fail then
          nofunc[n][x + 1] := Minus(ImageConeLongestPrefix([x], n - 1, T),
                                    ImageConeLongestPrefix([], n - 1, T));
        else
          badpref := ImageConeLongestPrefix([], n - 1, T);
          out := TransducerFunction(T, [x], n - 1)[1];
          neededcopies := Int(Ceil(Float(((Size(badpref) - Size(const[2][pos][1]))/Size(const[2][pos][2])))));
          neededcopies := Maximum(neededcopies, 0);
          stufftowrite := Concatenation(out, const[2][pos][1], Concatenation(ListWithIdenticalEntries(neededcopies, const[2][pos][2])));
          nofunc[n][x + 1] := Minus(stufftowrite, badpref);
          if nofunc[n][x + 1] <> [] and nofunc[n][x + 1][Size(nofunc[n][x + 1])]=const[2][pos][2][1] then
            Add(edgestopushfrom, [n, x + 1]);
          fi;
        fi;
      od;
    else
      for x in InputAlphabet(T) do;
        ntfunc[n][x + 1] := n;
        nofunc[n][x + 1] := const[2][Position(const[1],n - 1)][2];
      od;
    fi;
  od;
  for edge in edgestopushfrom do
    Add(ntfunc,ListWithIdenticalEntries(Size(InputAlphabet(T)),Size(ntfunc) + 1));
    pushstring := ShallowCopy(nofunc[ntfunc[edge[1]][edge[2]]][1]);
    out := nofunc[edge[1]][edge[2]];
    while out <> [] and out[Size(out)] = pushstring[Size(pushstring)] and not
          (IsRTransducer(T) and edge[1] in RootStates(T) and Size(out) = 1) do
      Remove(out);
      pushstring := Concatenation([pushstring[Size(pushstring)]],pushstring{[1 .. Size(pushstring)-1]});
    od;
    Add(nofunc, ListWithIdenticalEntries(Size(InputAlphabet(T)), pushstring));
    ntfunc[edge[1]][edge[2]]:= Size(ntfunc);
  od;

  if IsRTransducer(T) then
    return RTransducer(NrInputRoots(T), NrOutputRoots(T), NrInputSymbols(T),
                      NrOutputSymbols(T), ntfunc, nofunc);
  else
    return Transducer(NrInputSymbols(T), NrOutputSymbols(T), ntfunc, nofunc);
  fi;
end);

InstallMethod(RemoveInaccessibleStates, "for a transducer",
[IsTransducer],
function(T)
  local states, newq, newl, new, q, n, x;

  states := [1];
  newq := [[]];
  newl := [[]];
  n := 0;

  for q in states do
    n := n + 1;
    for x in InputAlphabet(T) do
      new := TransducerFunction(T, [x], q);

      if not new[2] in states then
        Add(states, new[2]);
        Add(newq, []);
        Add(newl, []);
      fi;

      newq[n][x + 1] := Position(states, new[2]);
      newl[n][x + 1] := new[1];
    od;
  od;

  return Transducer(NrInputSymbols(T), NrOutputSymbols(T), newq, newl);
end);

InstallMethod(CopyTransducerWithInitialState,
"for a transducer and a positive integer",
[IsTransducerOrRTransducer, IsPosInt],
function(T, i)
  local new, newq, newl, q, states, x, n, accessiblestates, newq2;
  states := ShallowCopy(States(T));

  if not i in states then
    ErrorNoReturn("aaa: ChangeInitialState: usage,\n",
                  "the second argument is not a state of the first argument,");
  fi;
  if IsRTransducer(T) and i <> 1 and i in RootStates(T) then
    ErrorNoReturn("aaa: ChangeInitialState: usage,\n",
                  "the state must be the initial state or a non root state,");
  fi;


  newq := [];
  newl := [];
  n := 0;

  for x in states do
    Add(newq, []);
    Add(newl, []);
  od;

  Sort(states, function(x, y)
                 return x = i;
               end);

  for q in states do
    n := n + 1;
    for x in [0 .. Size(OutputFunction(T)[q]) - 1] do
      new := TransducerFunction(T, [x], q);
      newq[n][x + 1] := Position(states, new[2]);
      newl[n][x + 1] := new[1];
    od;
  od;

  if IsTransducer(T) then
    return Transducer(NrInputSymbols(T), NrOutputSymbols(T), newq, newl);
  else
    if i = 1 then
      return RTransducer(NrInputRoots(T), NrOutputRoots(T), NrInputSymbols(T),
                         NrOutputSymbols(T), newq, newl);
    fi;
    accessiblestates := [1];
    for q in accessiblestates do
      for x in newq[q] do
        if not x in accessiblestates then
          Add(accessiblestates, x);
        fi;
      od;
    od;
    newq2 := List(accessiblestates,
                  x -> List(newq[x], y -> Position(accessiblestates, y)));
    return Transducer(NrInputSymbols(T), NrOutputSymbols(T), newq2,
                      List(accessiblestates, x -> newl[x]));

  fi;
end);

InstallMethod(RemoveEquivalentStates, "for a transducer",
[IsTransducer],
function(T)
  local states, n, Eq, Reps, q, p, Eqclass, new, newq, newl, x, seen, dmy, nsr,
        ns;
  ns := NrStates(T);
  nsr := 0;
  dmy := CopyTransducerWithInitialState(T, 1);
  Eqclass := function(y)
               local class;
                 for class in Eq do
                   if y in class then
                     return Minimum(class);
                   fi;
                 od;
             end;

  while nsr < ns do
    ns := NrStates(dmy);
    states := [1 .. ns];
    n := 0;
    Eq := [];
    Reps := [];
    newq := [];
    newl := [];
    seen := [];
    for q in states do
      if not q in seen then
        n := n + 1;
        Add(Eq, []);
        Add(Reps, q);
        for p in states do
          if not p in seen then
            if TransitionFunction(dmy)[q] = TransitionFunction(dmy)[p] and
                OutputFunction(dmy)[q] = OutputFunction(dmy)[p] then
              Add(Eq[n], p);
              Add(seen, p);
            fi;
          fi;
        od;
      fi;
    od;
    n := 0;
    for q in Reps do
      n := n + 1;
      Add(newq, []);
      Add(newl, []);

      for x in InputAlphabet(dmy) do
        new := TransducerFunction(dmy, [x], q);
        newl[n][x + 1] := new[1];
        newq[n][x + 1] := Position(Reps, Eqclass(new[2]));
      od;
    od;
    dmy := Transducer(NrInputSymbols(dmy), NrOutputSymbols(dmy), newq, newl);
    nsr := NrStates(dmy);
  od;
  return dmy;
end);

InstallMethod(IsInjectiveTransducer, "for a transducer",
[IsTransducerOrRTransducer],
function(t)
 local T, state, CurrentDigraph, D, tuple, out1, out2, out, newvertex, vertex, letter;

 if IsDegenerateTransducer(t) then
   ErrorNoReturn("aaa: IsInjectiveTransducer: usage,\n",
                  "the given transducer must be nondegenerate ");
 fi;

 if IsTransducer(t) then
   T := RemoveInaccessibleStates(t);
 else
   T := CopyTransducerWithInitialState(t, 1);
 fi;
 for state in States(T) do
   CurrentDigraph := [[],[]];
   for tuple in UnorderedTuples([0 .. Size(OutputFunction(T)[state]) - 1], 2) do
     if not tuple[1] = tuple[2] then
        out1 := TransducerFunction(T,[tuple[1]],state);
        out2 := TransducerFunction(T,[tuple[2]],state);
        if IsPrefix(out1[1],out2[1]) then
          newvertex := [[out1[2],[]],[out2[2],Minus(out1[1],out2[1])]];
        elif IsPrefix(out2[1],out1[1]) then
          newvertex := [[out2[2],[]],[out1[2],Minus(out2[1],out1[1])]];
        else
          continue;
        fi;
        if newvertex[1] = newvertex[2] then
           SetIsInjectiveTransducer(T, false);
           return false;
        fi;
        if not newvertex in CurrentDigraph[1] then
          Add(CurrentDigraph[1],newvertex);
          Add(CurrentDigraph[2], []);
        fi;
     fi;
   od;
   for vertex in CurrentDigraph[1] do
     for letter in InputAlphabet(T) do
        out := TransducerFunction(T,[letter],vertex[2][1]);
        if IsPrefix(vertex[2][2],out[1]) then
          newvertex := [vertex[1],[out[2],Minus(vertex[2][2],out[1])]];
        elif IsPrefix(out[1],vertex[2][2]) then
          newvertex := [[out[2],[]],[vertex[1][1],Minus(out[1],vertex[2][2])]];
        else
          continue;
        fi;
        if newvertex[1] = newvertex[2] then
           SetIsInjectiveTransducer(T, false);
           return false;
        fi;
        if not newvertex in CurrentDigraph[1] then
          Add(CurrentDigraph[1],newvertex);
          Add(CurrentDigraph[2],[]);
        fi;
        Add(CurrentDigraph[2][Position(CurrentDigraph[1],vertex)],Position(CurrentDigraph[1],newvertex));
     od;
   od;
   D := Digraph(CurrentDigraph[2]);
   if DigraphHasLoops(D) or DigraphGirth(D) < infinity then
     return false;
   fi;
 od;
 SetIsInjectiveTransducer(T, true);
 return true;
end);

InstallMethod(IsSurjectiveTransducer, "for a transducer",
[IsTransducerOrRTransducer],
function(T)
  local usefulstates, prefixcodes, imagetrees, completeblocks, finalimagetree,
  currentblocks, containsantichain, currentword, x, flag, y, minwords, tyx,
  pos, keys, subtree, check, pos2, prefix, block, state, imagekeys, outroots,
  minword, answer;

  if IsDegenerateTransducer(T) then
    ErrorNoReturn("aaa: IsSurjectiveTransducer: usage,\n",
                  "the given transducer must be nondegenerate ");
    fi;

  if IsRTransducer(T) then
    outroots := NrOutputRoots(T);
  else
    outroots := NrOutputSymbols(T);
  fi;

  imagetrees := States(T);
  completeblocks := [];
  usefulstates := [1];
  prefixcodes := [];

  containsantichain := function(list, n, r)
    return IsCompleteAntichain(MinimalWords(list), n, r);
  end;

  for x in usefulstates do
    flag := true;
    Add(prefixcodes, []);
    currentword := [];
    while flag do
      while IsEmpty(TransducerFunction(T, currentword, x)[1]) do
        Add(currentword, 0);
      od;
      Add(prefixcodes[Position(usefulstates, x)], StructuralCopy(currentword));
      while ((IsTransducer(T) or x > 1 or Size(currentword) > 1) and
            currentword[Size(currentword)] = NrInputSymbols(T) - 1) or
            (IsRTransducer(T) and x = 1 and Size(currentword) = 1 and
            currentword[Size(currentword)] = NrInputRoots(T) - 1) do
        Remove(currentword);
        if IsEmpty(currentword) then
          break;
        fi;
      od;

      if not IsEmpty(currentword) then
        currentword[Size(currentword)] := currentword[Size(currentword)] + 1;
      else
        flag := false;
      fi;
    od;
    for y in prefixcodes[Size(prefixcodes)] do;
      tyx := TransducerFunction(T, y, x);
      if not tyx[2] in usefulstates then
         Add(usefulstates, tyx[2]);
      fi;
    od;
    imagetrees[x] := [];
    for y in prefixcodes[Position(usefulstates, x)] do
      tyx := TransducerFunction(T, y, x);
      pos := Position(List(imagetrees[x], y -> y[1]), tyx[1]);
      if not pos = fail then
        AddSet(imagetrees[x][pos][2], tyx[2]);
      else
        AddSet(imagetrees[x], [tyx[1], [tyx[2]]]);
      fi;
    od;
  od;

  finalimagetree := [[[], [1]]];
  currentblocks := [[[], [[[], [1]]]]];
  keys := [[]];
  while (not IsSubset(completeblocks, List(currentblocks, x -> x[2]))) and
      containsantichain(keys, NrOutputSymbols(T), outroots) do
    for block in currentblocks do
      if not block[2] in completeblocks then
        break;
      fi;
    od;
    keys := List(finalimagetree, x -> x[1]);
    for state in finalimagetree[Position(keys, block[1])][2] do
      imagekeys := StructuralCopy(List(imagetrees[state], x -> x[1]));
      for prefix in imagekeys do
        keys := List(finalimagetree, x -> x[1]);
        pos := Position(keys, Concatenation(block[1], prefix));
        if not pos = fail then
          pos2 := Position(imagekeys, prefix);
          Append(finalimagetree[pos][2], imagetrees[state][pos2][2]);
          finalimagetree[pos][2] := Set(finalimagetree[pos][2]);
        else
          pos2 := Position(imagekeys, prefix);
          Add(finalimagetree, [StructuralCopy(Concatenation(block[1], prefix)),
              StructuralCopy(imagetrees[state][pos2][2])]);
        fi;
      od;
    od;
    keys := List(finalimagetree, x -> x[1]);
    pos := Position(keys, block[1]);
    Remove(finalimagetree, pos);
    Remove(currentblocks, Position(currentblocks, block));
    AddSet(completeblocks, block[2]);
    minwords := [];
    check := false;
    keys := List(finalimagetree, x -> x[1]);
    prefix := StructuralCopy(block[1]);
    for x in keys do
      if IsPrefix(x, prefix) then
        for y in [1 .. Size(minwords)] do
          if IsPrefix(minwords[y], x) then
            minwords[y] := StructuralCopy(x);
          elif IsPrefix(x, minwords[y]) then
            check := true;
          fi;
        od;

        if not check then
          Add(minwords, StructuralCopy(x));
        fi;
        check := false;
      fi;
      minwords := Set(minwords);
    od;
    for minword in minwords do
      subtree := [];
      for x in [1 .. Size(keys)] do
         if IsPrefix(keys[x], minword) then
           Add(subtree, [Minus(keys[x], minword),
               StructuralCopy(finalimagetree[x][2])]);
         fi;
      od;
      Add(currentblocks, [ShallowCopy(minword), StructuralCopy(subtree)]);
    od;
    keys := List(finalimagetree, x -> x[1]);
  od;

  answer := containsantichain(keys, NrOutputSymbols(T), outroots);
  SetIsSurjectiveTransducer(T, answer);
  return answer;
end);

InstallMethod(TransducerImageAutomaton, "for a transducer", 
[IsTransducerOrRTransducer],
function(T)
  local numberofstates, i, transitiontable, currentnewstate, j, k, autalph;
  numberofstates := Size(States(T));
  for i in Concatenation(OutputFunction(T)) do
    if not Size(i)=0 then
      numberofstates := numberofstates + Size(i) - 1;
    fi;
  od;

  if IsRTransducer(T) then
    autalph := Maximum(NrOutputSymbols(T), NrOutputRoots(T));
  else
    autalph := NrOutputSymbols(T);
  fi;

  transitiontable := List([1 ..  autalph + 1],
                           x -> List([1 .. numberofstates], y-> []));

  currentnewstate := Size(States(T)) + 1;
  for i in States(T) do
    for j in [0 .. Size(OutputFunction(T)[i]) - 1] do
      if Size(OutputFunction(T)[i][j+1]) > 1 then
         Add(transitiontable[OutputFunction(T)[i][j+1][1]+1][i],currentnewstate);
         for k in [2 .. Size(OutputFunction(T)[i][j+1])-1] do
           AddSet(transitiontable[OutputFunction(T)[i][j+1][k]+1][currentnewstate],currentnewstate + 1);
           currentnewstate := currentnewstate + 1;
         od;
           AddSet(transitiontable[OutputFunction(T)[i][j+1][Size(OutputFunction(T)[i][j+1])]+1][currentnewstate],TransducerFunction(T,[j],i)[2]);
           currentnewstate := currentnewstate + 1;
      fi;
      if Size(OutputFunction(T)[i][j+1]) = 1 then 
          AddSet(transitiontable[OutputFunction(T)[i][j+1][1]+1][i],TransducerFunction(T,[j],i)[2]);
      fi;
      if Size(OutputFunction(T)[i][j+1]) < 1 then 
          AddSet(transitiontable[autalph + 1][i],TransducerFunction(T,[j],i)[2]);
      fi;
    od;
  od;
  return Automaton("epsilon", numberofstates, 
                   Concatenation(List([0 .. autalph - 1],x->String(x)[1]),"@"), 
                   transitiontable, [1], [1 .. numberofstates]);
end);

InstallMethod(TransducerConstantStateOutputs, "for a transducer",
[IsTransducerOrRTransducer],
function(T)
  local constantstates, constantstateoutputs, currentstate, state,
  automatonhasbeenbuilt, stateisnotconstant, tuple, out1, out2, A, MinA,
  newstatenr, badstates, TMat, path, pos, root, circuit, next, Adata;
  constantstates := [];
  constantstateoutputs := [];
  automatonhasbeenbuilt := false;
  for state in States(T) do
    stateisnotconstant := false;
    for tuple in UnorderedTuples([0 .. Size(OutputFunction(T)[state]) - 1], 2) do
       if not tuple[1] = tuple[2] then
         out1 := TransducerFunction(T,[tuple[1]],state);
         out2 := TransducerFunction(T,[tuple[2]],state);
         if not (IsPrefix(out1[1],out2[1]) or IsPrefix(out2[1],out1[1])) then
           stateisnotconstant:= true;
           break;
         fi;
       fi;
    od;
    if stateisnotconstant then
      continue;
    fi;
    if not automatonhasbeenbuilt then
      A := TransducerImageAutomaton(T);
      Adata := [NumberStatesOfAutomaton(A), AlphabetOfAutomaton(A), TransitionMatrixOfAutomaton(A)];
      automatonhasbeenbuilt := true;
    fi;
    MinA := MinimalAutomaton(Automaton("epsilon",Adata[1],Adata[2],Adata[3],[state],[1 .. Adata[1]]));
    newstatenr := NumberStatesOfAutomaton(MinA);
    badstates := Filtered([1 .. newstatenr], x-> not x in FinalStatesOfAutomaton(MinA));
    if not Size(badstates) = 1 then
      continue;
    fi;
    TMat := TransitionMatrixOfAutomaton(MinA);
    currentstate := InitialStatesOfAutomaton(MinA)[1];
    path := [];
    pos := fail;
    while pos = fail do
      next := Filtered([1 .. Size(TMat)], x-> TMat[x][currentstate]<>badstates[1]);
      if not Size(next) = 1 then
        stateisnotconstant := true;
        break;
      fi;
      Add(path,[currentstate,next[1]]);
      currentstate := TMat[next[1]][currentstate];
      pos := Position(List(path,x->x[1]),currentstate);
    od;
    if not Size(path) = newstatenr -1 then
      continue;
    fi;
    if stateisnotconstant then
      continue;
    fi;
    Add(constantstates,state);
    root := Concatenation(List(path{[1 .. pos-1]}, x->String(x[2]-1)));
    circuit := Concatenation(List(path{[pos .. Size(path)]}, x-> String(x[2]-1)));
    Add(constantstateoutputs, Concatenation(root,"(",circuit,")*"));
  od;
  return [constantstates,constantstateoutputs];
end);

InstallMethod(IsDegenerateTransducer, "for a transducer",
[IsTransducerOrRTransducer],
function(T)
  local D;
  D := FixedOutputDigraph(T, []);
  return DigraphHasLoops(D) or DigraphGirth(D) < infinity;
end);

InstallMethod(FixedOutputDigraph, "for a transducer",
[IsTransducerOrRTransducer, IsDenseList],
function(T, word)
	local Out, OutNeigh;
	Out := States(T);
	OutNeigh := function(s)
		local Output, i;
		Output := [];
		for i in [0 .. Size(OutputFunction(T)[s]) - 1] do
			if TransducerFunction(T,[i],s)[1] = word then
				Add(Output,TransducerFunction(T,[i],s)[2]);
			fi;
		od;
		return Output;
	end;
	Apply(Out, OutNeigh);
	return Digraph(Out);
end);

QuotientTransducer := function(T,EqR, wantoutputs)
  local Classes, class, i, Pi, Lambda, initialclass;
  Classes:=ShallowCopy(EquivalenceRelationPartition(EquivalenceRelationByPairs(Domain(States(T)),EqR)));

  class := function(q)
        local j;
        for j in [1 .. Length(Classes)] do
                if q in Classes[j] then
                        return j;
                fi;
        od;
	return fail;
  end;
  for i in States(T) do
	if class(i)=fail then
		Add(Classes,[i]);
	fi;
  od;
  for i in Classes do
	if 1 in i then
          initialclass := i;
        fi;
  od;
  Remove(Classes,Position(Classes,initialclass));
  Classes := Concatenation([initialclass],Classes);
  Pi:= ShallowCopy(Classes);
  Lambda := ShallowCopy(Classes);
  Apply(Pi,x -> TransitionFunction(T)[x[1]]);
  if wantoutputs then
    Apply(Lambda, x-> OutputFunction(T)[x[1]]);
  else
    Apply(Lambda,
          x-> ListWithIdenticalEntries(Size(OutputFunction(T)[x[1]]), []));
  fi;
  for i in Pi do
        Apply(i,class);
  od;
  if IsTransducer(T) then
    return Transducer(NrInputSymbols(T), NrOutputSymbols(T), Pi, Lambda);
  elif IsRTransducer(T) then
    return RTransducer(NrInputRoots(T), NrOutputRoots(T),
                       NrInputSymbols(T), NrOutputSymbols(T), Pi, Lambda);
  fi;
end;

InstallMethod(CombineEquivalentStates, "for a transducer",
 [IsTransducerOrRTransducer],
function(T)
  local  x, EqRelation, i, tuple, NewTuple, b, flag;
  EqRelation := Filtered(UnorderedTuples(States(T), 2),
                       x -> OutputFunction(T)[x[1]] = OutputFunction(T)[x[2]]);
  flag := true;
  while flag do
    flag := false;
    for tuple in EqRelation do
      if IsRTransducer(T) and (tuple[1] = 1 or
         (tuple[1] in RootStates(T)) <> (tuple[2] in RootStates(T))) then
          Remove(EqRelation,Position(EqRelation,tuple));
          flag := true;
          break;
      else
        for i in InputAlphabet(T) do
          NewTuple := [TransducerFunction(T, [i], tuple[1])[2],
                       TransducerFunction(T, [i], tuple[2])[2]];
          Sort(NewTuple);
          if not NewTuple in EqRelation then
            Remove(EqRelation,Position(EqRelation,tuple));
            flag := true;
            break;
          fi;
        od;
      fi;
    od;
  od;
  return QuotientTransducer(T,EqRelation, true);
end);

InstallMethod(MinimalTransducer, "for a transducer",
[IsTransducerOrRTransducer],
function(T)
  local output;
   if IsDegenerateTransducer(T) then
    ErrorNoReturn("aaa: MinimalTransducer: usage,\n",
                  "the given transducer must be nondegenerate ");
  fi;
  output := T;
  if IsTransducer(T) then
    output := RemoveInaccessibleStates(output);
  fi;
  output := RemoveStatesWithIncompleteResponse(output);
  if IsTransducer(T) then
    output := RemoveInaccessibleStates(output);
  fi;
  output := CombineEquivalentStates(output);
  SetIsMinimalTransducer(output, true);
  return output;
end);

InstallMethod(IsomorphicInitialTransducers, "for a pair of transducer",
[IsTransducerOrRTransducer, IsTransducerOrRTransducer],
function(T1,T2)
  local D1, D2, perm, Dtemp, i, orderedstates1, orderedstates2, state, target,
        inaccessiblestates1, inaccessiblestates2;
  if IsTransducer(T1) <> IsTransducer(T2) then
    ErrorNoReturn("aaa: IsomorphicInitialTransducers: usage,\n",
                  "the given transducers must be of the same type");
  fi;
  if not States(T1) = States(T2) then
    return false;
  fi;
  if not InputAlphabet(T1)=InputAlphabet(T2) then
    return false;
  fi;
  if not OutputAlphabet(T1)= OutputAlphabet(T2) then
    return false;
  fi;
  if IsRTransducer(T1) and InputRoots(T1) <> InputRoots(T2) then
    return false;
  fi;
  if IsRTransducer(T1) and OutputRoots(T1) <> OutputRoots(T2) then
    return false;
  fi;
  D1 := List([1 .. Size(States(T1))], x -> [OutputFunction(T1)[x],
                                            TransitionFunction(T1)[x]]);
  D2 := List([1 .. Size(States(T2))], x -> [OutputFunction(T2)[x],
                                            TransitionFunction(T2)[x]]);
  orderedstates1 := [1];
  orderedstates2 := [1];
  for state in orderedstates1 do
    for target in D1[state][2] do
      if not target in orderedstates1 then
        Add(orderedstates1, target);
      fi;
    od;
  od;

  for state in orderedstates2 do
    for target in D2[state][2] do
      if not target in orderedstates2 then
        Add(orderedstates2, target);
      fi;
    od;
  od;

  if Size(orderedstates1) <> Size(orderedstates2) then
    return false;
  fi;

  inaccessiblestates1 := ShallowCopy(States(T1));
  SubtractSet(inaccessiblestates1, orderedstates1);

  inaccessiblestates2 := ShallowCopy(States(T2));
  SubtractSet(inaccessiblestates2, orderedstates2);

  for i in SymmetricGroup(Size(inaccessiblestates1)) do
    perm := function(state)
      if state in orderedstates1 then
        return orderedstates2[Position(orderedstates1, state)];
      fi;
      return inaccessiblestates2[Position(inaccessiblestates1, state)^i];
    end;
    perm := PermList(List(States(T1), perm));
    Dtemp := StructuralCopy(List([1 .. Size(States(T1))],x -> D1[x^(perm^-1)]));
    for i in [1 .. Size(Dtemp)] do
      Apply(Dtemp[i][2], x -> x ^ (perm));
    od;
    if Dtemp = D2 then
      return true;
    fi;
  od;
  return false;
end);

InstallMethod(OmegaEquivalentTransducers, "for a pair of transducers",
[IsTransducerOrRTransducer, IsTransducerOrRTransducer],
function(T1, T2)
  local M1, M2;
  M1:= MinimalTransducer(T1);
  M2:= MinimalTransducer(T2);
  return IsomorphicInitialTransducers(M1, M2);
end);

InstallMethod(\=, "for two transducers",
[IsTransducerOrRTransducer, IsTransducerOrRTransducer],
OmegaEquivalentTransducers);

InstallMethod(IsBijectiveTransducer, "for a transducer",
[IsTransducerOrRTransducer], T -> IsInjectiveTransducer(T)
                                  and IsSurjectiveTransducer(T));

InstallMethod(IsMinimalTransducer, "for a transducer",
[IsTransducerOrRTransducer],
function(T)
  return IsomorphicInitialTransducers(T, MinimalTransducer(T));
end);

InstallMethod(IsSynchronousTransducer, "for a transducer",
[IsTransducer], T -> ForAll(OutputFunction(T), x-> ForAll(x, y -> Size(y)=1)));

InstallMethod(TransducerOrder, "for a transducer",
[IsTransducer],
function(T)
  local p;
  if not IsBijectiveTransducer(T) then
    ErrorNoReturn("aaa: TransducerOrder: usage,\n",
                  "the given transducer must be bijective");
  fi;
  p := 1;
  while not T^p = T^0 do
    p := p + 1;
  od;
  return p;
end);

InstallMethod(TransducerSynchronizingLength, "for a transducer", [IsTransducerOrRTransducer],
function(T)
	local count, CopyT, TempT, flag;
	flag := true;
        CopyT := CopyTransducerWithInitialState(T,1);
	count := -1;
	while flag do
		count := count + 1;
		TempT := QuotientTransducer(CopyT,Filtered(Cartesian(States(CopyT), States(CopyT)),x-> TransitionFunction(CopyT)[x[1]]=TransitionFunction(CopyT)[x[2]]), false);
		flag := (States(CopyT) <> States(TempT));
		CopyT := TempT;
	od;
	if IsTransducer(T) and States(CopyT) = [1] then
		return count;
	fi;
        if IsRTransducer(T) and States(CopyT) = [1, 2] then
                return count + 1;
        fi;

	return infinity;
end);

InstallMethod(IsSynchronizingTransducer, "for a transducer",
[IsTransducerOrRTransducer], T -> TransducerSynchronizingLength(T)<infinity);

InstallMethod(IsBisynchronizingTransducer, "for a transducer",
[IsTransducer],
function(T)
  return IsBijectiveTransducer(T) and IsSynchronizingTransducer(T) and IsSynchronizingTransducer(InverseTransducer(T));
end);

InstallMethod(IsLipschitzTransducer, "for a transducer",
[IsTransducer],
function(T)
  local s, statepath, letterpath, currentstate, nonconstantstates;
  nonconstantstates := States(T);
  SubtractSet(nonconstantstates, TransducerConstantStateOutputs(T)[1]);
  for s in nonconstantstates do;
    statepath := [s];
    letterpath := [0];
    currentstate :=  TransducerFunction(T, [0], s)[2];
    while statepath <> [] do
      if currentstate = s and
        Size(TransducerFunction(T, letterpath, s)[1]) < Size(letterpath) then
        return false;
      fi;
      if not currentstate in statepath then
        Add(letterpath, 0);
        Add(statepath, currentstate);
      else
        letterpath[Size(letterpath)] := letterpath[Size(letterpath)] + 1;
        while letterpath <> [] and
              letterpath[Size(letterpath)] = NrInputSymbols(T) do;
          Remove(letterpath);
          Remove(statepath);
          if letterpath <> [] then
            letterpath[Size(letterpath)] := letterpath[Size(letterpath)] + 1;
          fi;
        od;
      fi;
      currentstate := TransducerFunction(T, letterpath, s)[2];
    od;
  od;
  return true;
end);

InstallMethod(IsomorphicTransducers, "for a pair of transducers",
[IsTransducer,IsTransducer],
function(T1,T2)
  return ForAny(States(T2), x -> IsomorphicInitialTransducers(T1,
                                 CopyTransducerWithInitialState(T2,x)));
end);

InstallMethod(TransducerCore, "for a transducer",
[IsTransducerOrRTransducer],
function(T)
  local SLen;
  SLen := TransducerSynchronizingLength(T);
  if SLen = infinity then
    ErrorNoReturn("aaa: TransducerCore: usage,\n",
                  "the transducer must be synchronizing ");
  fi;
  if IsTransducer(T) then
    return RemoveInaccessibleStates(CopyTransducerWithInitialState(T,
              TransducerFunction(T, ListWithIdenticalEntries(SLen, 0), 1)[2]));
  else
    return CopyTransducerWithInitialState(T,
              TransducerFunction(T, ListWithIdenticalEntries(SLen, 0), 1)[2]);
  fi;
end);

InstallMethod(IsCoreTransducer, "for a transducer",
[IsTransducer], T -> IsSynchronizingTransducer(T)
and NrStates(T) = NrStates(TransducerCore(T)));

InstallMethod(CoreProduct, "for a pair of transducers", [IsTransducer, IsTransducer],
function(C1,C2)
  return TransducerCore(MinimalTransducer(C1*C2));
end);

InstallMethod(ImageAsUnionOfCones, "for a transducer",
[IsTransducer],
function(T)
  local A, NrS, Pairs, Alph, Cones, GoodStates, TMat, Word, StatePath,
        letter, pos, target;
  if IsDegenerateTransducer(T) then
    ErrorNoReturn("aaa: ImageAsUnionOfCones: usage,\n",
                  "the given transducer must be nondegenerate ");
  fi;
  Alph := OutputAlphabet(T);
  A  := MinimalAutomaton(TransducerImageAutomaton(T));
  NrS  := NumberStatesOfAutomaton(A);
  if NrS = 1 then
    return [[]];
  fi;
  Cones := [];
  GoodStates := FinalStatesOfAutomaton(A);
  if not Size(GoodStates) = NrS - 1 then
    return fail;
  fi;
  TMat := TransitionMatrixOfAutomaton(A);
  Word := [];
  StatePath := [InitialStatesOfAutomaton(A)[1]];
  letter := 0;
  repeat
    target := TMat[letter + 1][StatePath[Size(StatePath)]];
    pos := Position(StatePath, target);
    if pos = fail then
      Add(Word, letter);
      Add(StatePath, target);
      letter := 0;
    elif pos = Size(StatePath) then
      if Size(Set(List(TMat, x -> x[target]))) > 1 then
        return fail;
      fi;
      if target in GoodStates then
        Add(Cones, ShallowCopy(Word));
      fi;
      letter := Word[Size(Word)] + 1;
      Remove(Word);
      Remove(StatePath);
    else
      return fail;
    fi;
    while Word <> [] and letter = Size(Alph) do
      letter := Word[Size(Word)] + 1;
      Remove(Word);
      Remove(StatePath);
    od;
  until letter = Size(Alph);
  return Cones;
end);

InstallMethod(HasClopenImage, "for a Transducer",
[IsTransducer],
function(T)
 return ImageAsUnionOfCones(T) <> fail;
end);

InstallMethod(IsCompletableCore, "for a transducer",
[IsTransducer],
function(T)
  return not IsDegenerateTransducer(T)
         and IsInjectiveTransducer(T) and HasClopenImage(T)
         and IsCoreTransducer(T);
end);

InstallMethod(CoreCompletion, "for a transducer",
[IsTransducer],
function(C)
  local MC, imagecone, preimagecones, viablecombination, toutput, Pi, Lambda,
        currentstate, cone, i, state, letter, pair, temp,
        NumberRootStates, numberofletters, currentletter;
  if not IsCompletableCore(C) then
    ErrorNoReturn("aaa: CoreCompletion: usage,\n",
                  "this transducer is not a completable core");
  fi;

  MC := CopyTransducerWithInitialState(C, 1);
  imagecone := ImageAsUnionOfCones(MC)[1];
  preimagecones := [];
  for cone in Set(PreimageConePrefixes(imagecone, 1, MC)) do
    if ForAll(preimagecones, x -> not IsPrefix(cone, x)) then
      Add(preimagecones, cone);
    fi;
  od;

  NumberRootStates := Size(preimagecones);

  temp := StructuralCopy(preimagecones);
  preimagecones := [];
  for i in [0 .. NrInputSymbols(C) - 2] do
    for cone in temp do
      Add(preimagecones, [i, StructuralCopy(cone)]);
    od;
  od;

  viablecombination := [];
  for cone in preimagecones do
    toutput := TransducerFunction(MC, cone[2], 1);
    Add(viablecombination,
        [Concatenation([cone[1]], Minus(toutput[1], imagecone)), toutput[2]]);
  od;

  Pi := [];
  Lambda := [];
  for i in [2 .. NumberRootStates] do
    Add(Pi, [i]);
    Add(Lambda,[[]]);
  od;
  Add(Pi, []);
  Add(Lambda,[[]]);
  Append(Pi, ShallowCopy(TransitionFunction(MC)));
  for state in [NumberRootStates + 1 .. Size(Pi)] do
    Apply(Pi[state], x -> x + NumberRootStates);
  od;
  Append(Lambda, ShallowCopy(OutputFunction(MC)));

  currentstate := 1;
  while not viablecombination = [] do
    currentletter := Size(Pi[currentstate]);
    if currentstate = 1 then
      numberofletters := NrInputSymbols(C) - 1;
    else
      numberofletters := NrInputSymbols(C);
    fi;
    while currentletter < numberofletters do
      pair := Remove(viablecombination);
      Pi[currentstate][currentletter + 1] := pair[2] + NumberRootStates;
      Lambda[currentstate][currentletter + 1] := pair[1];
      currentletter := currentletter + 1;
    od;
    currentstate := currentstate + 1;
  od;
  return RTransducer(NrInputSymbols(C) - 1, NrInputSymbols(C) - 1,
                    NrInputSymbols(C), NrInputSymbols(C), Pi, Lambda);
end);

InstallMethod(ActionOnNecklaces, "for a positive integer and a transducer",
[IsPosInt, IsTransducerOrRTransducer],
function(necklacesize, T)
  local X, x, i, j, permutation;
  if not IsSynchronizingTransducer(T) then
    return fail;
  fi;
  X := PrimeNecklaces(NrInputSymbols(T), necklacesize);
  permutation := [];
  for i in [1 .. Size(X)] do
    x := X[i]^T;
    for j in [1 .. Size(X)] do
      if ShiftEquivalent(X[j], x) then
        Add(permutation, j);
      fi;
    od;
    if not Size(permutation) = i then
      return fail;
    fi;
  od;
  return PermList(permutation);
end);

InstallMethod(InOn, "for a Transducer",
[IsTransducer],
function(T)
  if IsCompletableCore(T) then
    return IsSynchronizingTransducer(CoreCompletion(T)^-1);
  else
    return false;
  fi;
end);

InstallMethod(InLn, "for a Transducer",
[IsTransducer], x -> InOn(x) and IsLipschitzTransducer(x));

InstallMethod(CanonicalAnnotation, "for a Transducer",
[IsTransducer],
function(T)
  local slen, tout, zerostate, word, annotation;
  if not InLn(T) then
    return fail;
  fi;
  slen := TransducerSynchronizingLength(T);
  zerostate := TransducerFunction(T, ListWithIdenticalEntries(slen, 0), 1)[2];
  annotation := [];
  annotation[zerostate] := 0;
  for word in Tuples(InputAlphabet(T), slen) do
    tout := TransducerFunction(T, word, 1);
    annotation[tout[2]] := Size(tout[1]) - slen;
  od;
  return annotation;
end);

InstallMethod(LnBlockCodeTransducer, "for a transducer", [IsTransducer],
function(T)
  local ann, slen, wlen, f, maxdiff, i, j;
  if not InLn(T) then
    return fail;
  fi;
  ann := CanonicalAnnotation(T);
  slen := TransducerSynchronizingLength(T);
  wlen := slen + Maximum(ann) - Minimum(ann);
  f := function(word)
    local writtenword, sstate;
    sstate := TransducerFunction(T, word{[1 .. slen]}, 1)[2];
    writtenword := TransducerFunction(T, word{[slen + 1 .. wlen + 1]}, sstate)[1];
    return [writtenword[Maximum(ann) - ann[sstate] + 1]];
  end;
  return BlockCodeTransducer(NrInputSymbols(T), wlen, f);
end);

InstallMethod(MinSyncSync, "for a Transducer",
[IsTransducer],
function(T)
   local out, lambda, i, state, letter;
   if not IsCoreTransducer(T) and IsSynchronousTransducer(T) then
     return fail;
   fi;
   out := CombineEquivalentStates(T);
   for i in OutputFunction(out) do
     if Size(Set(i)) > 1 then
       return out;
     fi;
   od;
   lambda := [];
   for state in States(out) do
     Add(lambda, []);
     for letter in InputAlphabet(T) do
       Add(lambda[state],
           OutputFunction(out)[TransitionFunction(out)[state][letter + 1]][1]);
     od;
   od;
   return MinSyncSync(Transducer(NrInputSymbols(out), NrOutputSymbols(out),
                                 TransitionFunction(out), lambda));
end);

InstallMethod(OnInverse, "for a Transducer",
[IsTransducer],
function(T)
  if not InOn(T) then
    return fail;
  fi;
  return TransducerCore(MinimalTransducer(CoreCompletion(T)^-1));
end);

InstallMethod(\+, "for a Transducer", [IsTransducer, IsTransducer],
function(T1,T2)
  if not InOn(T1) and InOn(T2) then
    return fail;
  fi;
  return CoreProduct(T1, T2);
end);

InstallMethod(\-, "for a pair of Transducers", [IsTransducer, IsTransducer],
function(T1, T2)
  if not InOn(T1) and InOn(T2) then
    return fail;
  fi;
  return T1 + OnInverse(T2);
end);

InstallMethod(\*, "for an integer and a transducer", [IsInt, IsTransducer],
function(n, T)
  local k, t;
  if not InOn(T) then
    return fail;
  fi;
  if n = 0 then
    return IdentityTransducer(NrInputSymbols(T));
  fi;
  t := CopyTransducerWithInitialState(T, 1);
  if n < 0 then
    n := -n;
    t := OnInverse(t);
  fi;
  k := 1;
  while 2*k <= n do
    k := 2*k;
    t := t+t;
  od;
  return t + (n-k)*T;
end);

InstallMethod(ASProd, "for a pair of transducers",
[IsTransducer, IsTransducer],
function(T1, T2)
  if not (IsSynchronousTransducer(T1) and IsCoreTransducer(T1) and
          IsSynchronousTransducer(T2) and IsCoreTransducer(T2)) then
    return fail;
  fi;
  return TransducerCore(CombineEquivalentStates(RemoveInaccessibleStates(T1*T2)));
end);

InstallMethod(Onlessthan, "for a pair of transducers",
[IsTransducer, IsTransducer],
function(T1,T2)
  local M1, M2, S1, S2, q1, q2, word, w1, w2, i;
  if not (InOn(T1) and InOn(T2)) then
    return fail;
  fi;
  if NrInputSymbols(T1) < NrInputSymbols(T2) then
    return true;
  elif NrInputSymbols(T2) < NrInputSymbols(T1) then
    return false;
  fi;
  M1 := TransducerCore(MinimalTransducer(T1));
  M2 := TransducerCore(MinimalTransducer(T2));
  if IsomorphicTransducers(M1, M2) then
    return false;
  fi;
  S1 := TransducerSynchronizingLength(M1);
  S2 := TransducerSynchronizingLength(M2);
  if S1 < S2 then
    return true;
  elif S2 < S1 then
    return false;
  fi;
  if NrStates(M1) < NrStates(M2) then
    return true;
  elif NrStates(M2) < NrStates(M1) then
    return false;
  fi;
  q1 := TransducerFunction(M1, List([1 .. S1], x -> 0), 1)[2];
  q2 := TransducerFunction(M2, List([1 .. S2], x -> 0), 1)[2];
  word := [0];
  while true do
    w1 := TransducerFunction(M1, word, q1)[1];
    w2 := TransducerFunction(M2, word, q2)[1];
    if Size(w1) < Size(w2) then
      return true;
    elif Size(w2) < Size(w1) then
      return false;
    fi;
    if w1 < w2 then
      return true;
    elif w2 < w1 then
      return false;
    fi;
    for i in [0 .. Size(word) - 1] do
      if word[Size(word) - i] <> NrInputSymbols(T1) - 1 then
        word[Size(word) - i] := word[Size(word) - i] + 1;
        break;
      else
        word[Size(word) - i] := 0;
      fi;
    od;
    if i = Size(word) - 1 and word[1] = 0 then
      Add(word, 0);
    fi;
  od;
end);

InstallMethod(LnToLnk, "for a transducer and a positive integer",
[IsTransducer, IsPosInt],
function(T, k)
  local blockcode, NewToOld, OldToNew, f, SLen;
  if not InLn(T) then
    return fail;
  fi;
  if k = 1 then
    return T;
  fi;
  NewToOld := function(n)
   return List([0 .. (k - 1)], x -> Int(RemInt(n, NrInputSymbols(T) ^ (x + 1))
                                                / (NrInputSymbols(T) ^ x)));
  end;
  OldToNew := function(l)
    return Sum(List([0 .. Size(l) - 1],
               y -> l[y + 1] * (NrInputSymbols(T) ^ y)));
  end;
  if not IsSynchronousTransducer(T) then
    blockcode := CombineEquivalentStates(LnBlockCodeTransducer(T));
  else
    blockcode := T;
  fi;

  SLen := TransducerSynchronizingLength(blockcode);
  f := function(word)
    local oldin, oldout;
    oldin := Concatenation(List(word, x -> NewToOld(x)));
    oldout := TransducerFunction(blockcode,
                                 oldin{[Size(oldin) - k + 1 .. Size(oldin)]},
                                 TransducerFunction(blockcode,
                                       oldin{[1 .. Size(oldin) - k]}, 1)[2])[1];
    return [OldToNew(oldout)];
  end;
  return BlockCodeTransducer(NrInputSymbols(T)^k, Int(SLen/2) + 2, f);
end);

InstallMethod(OnOrder, "for a transducer",
[IsTransducer],
function(T)
  local T1, power;
  T1 := T;
  power := 1;
  if NrInputSymbols(T) =2 and Order(ActionOnNecklaces(10, T)) > 100 then
    return infinity;
  fi;
  while not IsomorphicTransducers(0*T, T1) do
    T1 := T1 + T;
    power := power + 1;
    if NrStates(T1) > 100 then
      return infinity;
    fi;
  od;
  return power;
end);

InstallMethod(StateSynchronizingWords, "for a transducer",
[IsTransducer],
function(T)
  local outputs, tuple;
  if not IsCoreTransducer(T) then
    return fail;
  fi;
  outputs := List(States(T), x-> []);

  for tuple in Tuples(InputAlphabet(T), TransducerSynchronizingLength(T)) do
    Add(outputs[TransducerFunction(T, tuple, 1)[2]], tuple);
  od;

  return outputs;
end);

InstallMethod(SynchronousLn, "for a transducer",
[IsTransducer],
function(T)
  if not InLn(T) then
    return fail;
  fi;
  if IsSynchronousTransducer(T) then
    return MinSyncSync(T);
  fi;
  return MinSyncSync(LnBlockCodeTransducer(T));
end);

InstallMethod(HomeomorphismStates, "for a transducer",
[IsTransducer],
function(T)
  local state, output;
  output := [];
  for state in States(T) do
    if IsBijectiveTransducer(CopyTransducerWithInitialState(T, state)) then
      Add(output, state);
    fi;
  od;
  return output;
end);

InstallMethod(InterestingNumbers, "for a transducer",
[IsTransducer],
function(T)
  local words, outputs;
  outputs := [NrStates(T), TransducerSynchronizingLength(T) + 1, Maximum(List(Concatenation(OutputFunction(T)), x->Size(x)))];
  Add(outputs, Float(outputs[3]/outputs[1]));
  words := Tuples(InputAlphabet(T), outputs[2]);
  Apply(words, x-> Size(x^T));
  Add(outputs, Minimum(words));
  Add(outputs, outputs[5]/ outputs[2]);
  return outputs;
end);

InstallMethod(GyrationValues, "for a transducer",
[IsTransducer, IsDenseList],
function(T, x)
  local i_x, r_x, currentword;
  if not (IsSynchronousTransducer(T) and InOn(T)) then
	ErrorNoReturn("aaa: GyrationValues: usage,\n",
                  "the given transducer must be an element",
                  "of autshift,");
  fi;
  if ForAny(x, y -> not y in [0 .. NrInputSymbols(T)]) then
    ErrorNoReturn("aaa: GyrationValues: usage,\n",
                  "the given word must be in the alphabet",
                  "of the given transducer,");
  fi;
  i_x:=1;
  currentword := x^T;
  while not ShiftEquivalent(x, currentword) do
    i_x := i_x + 1;
    currentword := currentword^T;
  od;
  r_x := 0;
  while x <> Concatenation(currentword{[r_x +1 .. Size(x)]}, currentword{[1 .. r_x]}) do
    r_x := r_x + 1;
  od;
  return [i_x, r_x];
end);

InstallMethod(GyrationAtLevel, "for a transducer",
[IsTransducer, IsPosInt],
function(T, l)
  local gyr_l;
  if not (IsSynchronousTransducer(T) and InOn(T)) then
        ErrorNoReturn("aaa: GyrationAtLevel: usage,\n",
                  "the given transducer must be an element",
                  "of autshift,");
  fi;
  gyr_l := Sum(List(PrimeNecklaces(NrInputSymbols(T), l), x -> GyrationValues(T, x)[2]));
  while gyr_l >= l do
    gyr_l := gyr_l - l;
  od;
  return gyr_l;
end);

InstallMethod(LnShayoHomomorphism, "for a transducer",
[IsTransducer],
function(T)
  local n, img;
  if not InLn(T) then
    return fail;
  fi;
  n := NrInputSymbols(T);
  img := Size(ImageAsUnionOfCones(T));
  while IsInt(img/n) do
    img := Int(img/n);
  od;
  return img;
end);

InstallMethod(OnShayoHomomorphism, "for a transducer",
[IsTransducer],
function(T)
  if not InOn(T) then
    return fail;
  fi;
  return RemInt(Size(ImageAsUnionOfCones(T)), NrInputSymbols(T));
end);

InstallMethod(OneSidedDecomposition, "for a transducer",
[IsTransducer],
function(T)
  local T2, factor, makefactor, inverse, i, j, k, output, nrep, nrelated, Bstates, BTransitions, Boutputs, state;
  if not InLn(T) then
    return fail;
  fi;
  for i in States(T) do
    if not Size(Set(OutputFunction(T)[i])) = NrInputSymbols(T) then
      return fail;
    fi;
  od;

  makefactor := function(T)
    inverse := SynchronousLn(OnInverse(T));

    for i in States(inverse) do
      if not Size(Set(OutputFunction(inverse)[i])) = NrInputSymbols(inverse) then
        return fail;
      fi;
    od;

    for j in States(inverse) do
      for k in [j + 1 .. NrStates(inverse)] do
        if ForAll(InputAlphabet(inverse), x -> TransitionFunction(inverse)[j][Position(OutputFunction(inverse)[j], [x])] = TransitionFunction(inverse)[k][Position(OutputFunction(inverse)[k], [x])]) then
          inverse := CopyTransducerWithInitialState(inverse, j);
          inverse := CopyTransducerWithInitialState(inverse, k);
        break;
        fi;
      od;
    od;

    nrelated:= function(q_1,q_2,n)
      if n = 0 then
        return q_1=q_2;
      fi;
      return ForAll([1 .. NrInputSymbols(inverse)], y -> nrelated(TransitionFunction(inverse)[q_1][y], TransitionFunction(inverse)[q_2][y], n-1));
    end;
  
    nrep := function(q, n)
      for j in States(inverse) do
        if nrelated(j, q, n) then
          return j;
        fi;
      od;
    end;

    for i in States(T) do
      if nrelated(1, 2, i) then
        break;
      fi;
    od;

    i:= i-1;
 
    Bstates := List(States(inverse), x -> nrep(x, i));
    Bstates := Set(Bstates);
    BTransitions := [];
    for state in Bstates do
      Add(BTransitions, List(TransitionFunction(inverse)[state], y-> nrep(y, i)));
    od;
  
    Boutputs := List(Bstates, x-> List([0 .. NrInputSymbols(inverse)-1], y-> [y]));
    for j in [0 .. NrInputSymbols(inverse)-1] do
      Boutputs[2][Position(OutputFunction(inverse)[2], [j])] := [Position(OutputFunction(inverse)[1], [j]) - 1];
    od;

    for j in [1 .. Size(BTransitions)] do
      for k in [1 .. Size(BTransitions[j])] do
        BTransitions[j][k] := Position(Bstates, nrep(BTransitions[j][k], i));
      od;
    od;
  
    return Transducer(NrInputSymbols(inverse), NrOutputSymbols(inverse), BTransitions, Boutputs);
  end;  
  output := [];

  T2:= CopyTransducerWithInitialState(T, 1);
  while not NrStates(T2) = 1 do
    factor := makefactor(T2);
    Add(output, OnInverse(factor));
    T2 := T2 + factor;
  od;
  if not IsomorphicTransducers(T2, IdentityTransducer(NrInputSymbols(T2))) then
    Add(output, T2);
  fi;
  
  return Reversed(output);
end);
