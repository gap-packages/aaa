#foldings


#InstallMethod(LineFolding, "for a dense list",
#[IsDigraph, IsInt, IsInt],
#function(D, p, f)
#  local i, j, edgetransformation, vertextransformation, vertices, pos, temp,
#        currentedge, newourneighbours;
#  if not p > 0 and f > 0 then
#    return fail;
#  fi;
#  vertices := PathsOfGivenLength(D, p + f);
#
#  edgetransformation := [];
#  vertextransformation := [1 .. Maximum(Size(vertices), DigraphNrVertices(D))];
#
#  newoutneighbours := [];
#  for i in [1 .. Size(vertices)] do
#    currentvertexoutneighbours := [];
#    currentedge = 0;
#    vertextransformation[i] := vertex[i][2*p + 1];
#    for j in OutNeighbours(D)[vertices[i][Size(vertices[i])]] do
#      currentedge := currentedge + 1;
#      temp := Flat([vertices[i], currentedge, j]);
#      Add(edgetransformation, temp[2*p + 2]); 
#      pos := Position(vertices, temp{[3 .. 2*(p+f) + 3]});
#      Add(currentvertexneighbours, pos);
#    od;
#    Add(newoutneighbours, currentvertexoutneighbours);
#    od;
#
#  return [Digraph(newoutneighbours), D, Transformation(vertextransformation),
#          Transformation(edgetransformation)];
#
#end);

InstallMethod(PathsOfGivenLength, "for a digraph and a non-negative integer",
[IsDigraph, IsInt],
function(D, n)
  local i, j, potentialpaths, paths, newpath;
  if n = 0 then
    return DigraphVertices(D);
  fi;
  potentialpaths := Tuples([1 .. DigraphNrEdges(D)], n);
  paths := [];
  for i in potentialpaths do
    newpath := [DigraphEdges(D)[i[1]][1]];
    for j in [1 .. Size(i)] do
      if not newpath[Size(newpath)] = DigraphEdges(D)[i[j]][1] then
        break;
      fi;
      Add(newpath, i[j]);
      Add(newpath, DigraphEdges(D)[i[j]][2]);
    od;
    if Size(newpath) = 2*n+1 then
      Add(paths, ShallowCopy(newpath));
    fi;
  od;
  return paths;
end);

InstallMethod(TransducerRangeFolding, "for an synchronous Ln representative",
[IsTransducer],
function(T)
  local D, vertex, edge, edgemap;
  if not InLn(T) then
    return fail;
  fi;
  if not IsSynchronousTransducer(T) then
    return fail;
  fi; 
  D := Digraph(TransitionFunction(T));
  edgemap := [];
  for vertex in DigraphVertices(D) do
    for edge in [1 .. Size(OutNeighbours(D)[vertex])] do
      Add(edgemap, OutputFunction(T)[vertex][edge][1] + 1);
    od;
  od;
  return [D, Digraph([List([1 .. NrOutputSymbols(T)], x-> 1)]),
         Transformation(List([1 .. DigraphNrVertices(D)]), x-> 1),
         Transformation(edgemap)];
end);

InstallMethod(IsDigraphCongruence, "for a digraph and a pair of equivalence relations",
[IsDigraph, IsBinaryRelation, IsBinaryRelation],
function(D, Veqiv, Eeqiv)
  local edge, edges, otheredge;

  if not IsEquivalenceRelation(Veqiv) then
    return false;
  fi;

  if not IsEquivalenceRelation(Eeqiv) then
    return false;
  fi;

  if not DegreeOfBinaryRelation(Veqiv) = DigraphNrVertices(D) then
    return false;
  fi;

  if not DegreeOfBinaryRelation(Eeqiv) = DigraphNrEdges(D) then
    return false;
  fi;

  edges := DigraphEdges(D);

  for edge in [1 .. DigraphNrEdges(D)] do
    for otheredge in edge^Eeqiv do
      if not edges[edge][1] in edges[otheredge][1]^Veqiv then
        return false;
      fi;
      if not edges[edge][2] in edges[otheredge][2]^Veqiv then
        return false;
      fi;
    od;
  od;

  return true;
end);

InstallMethod(QuotientFromDigraphCongruence, "for a digraph and a digraph congruence",
[IsDigraph, IsBinaryRelation, IsBinaryRelation],
function(D, Veqiv, Eeqiv)
  local edges, newedges, newvertices, outneighbourfinder, outneighbours,
        i, output;
  if not IsDigraphCongruence(D, Veqiv, Eeqiv) then
    return fail;
  fi;
  edges := List(DigraphEdges(D), x-> [x[1]^Veqiv, x[2]^Veqiv]);
  newvertices := EquivalenceRelationPartition(Veqiv);
  newedges := EquivalenceRelationPartition(Eeqiv);
  Sort(newedges,
       function(a , b)
         return edges[a[1]] <= edges[b[1]]; end);

  outneighbourfinder := function(vertex)
    output := [];
    for i in newedges do
      if edges[newedges[i][1]][1]=vertex then
         Add(output, Position(newvertices, edges[newedges[i][1]][2]));
      fi;
    od;
  end;

  outneighbours := List(newvertices, outneighbourfinder);

  return [D, Digraph(outneighbours),
          Transformation(List(DigraphVertices(D),
                              x-> Position(newvertices, x^Veqiv))),
          Transformation(List([1 .. DigraphNrEdges(D)],
                              x-> Position(newedges, x^Eeqiv)))];

end);

RemoveBrackets := function(word)
    local i, newword, lbracket, rbracket, nestcount, index;
    # pylint: disable = too-many-branches
    if word = "" then
        return "";
    fi;
    #if the number of left brackets is different from the number of right
    #brackets they can't possibly pair up
    if not Size(Filtered(word, x -> x = '(')) =
           Size(Filtered(word, x -> x = ')')) then
        ErrorNoReturn("invalid bracket structue");
    fi;
    #if the ^ is at the end of the string there is no exponent.
    #if the ^ is at the start of the string there is no base.
    if word[1] = '^' or word[Size(word)] = '^' then
        ErrorNoReturn("invalid power structure");
    fi;
    #checks that all ^s have an exponent.
    for index in [1 .. Size(word)] do
        if word[index] = '^' then
            if not word[index + 1] in "0123456789" then
                ErrorNoReturn("invalid power structure");
            fi;
        fi;
    od;
    #i acts as a pointer to positions in the string.
    newword := "";
    i := 1;
    while i <= Size(word) do
        #if there are no brackets the character is left as it is.
        if not word[i] = '(' then
            Add(newword, word[i]);
        else
            lbracket := i;
            rbracket := -1;
            #tracks how 'deep' the position of i is in terms of nested
            #brackets
            nestcount := 0;
            i := i + 1;
            while i <= Size(word) do
                if word[i] = '(' then
                    nestcount := nestcount + 1;
                elif word[i] = ')' then
                    if nestcount = 0 then
                        rbracket := i;
                        break;
                    else
                        nestcount := nestcount - 1;
                    fi;
                fi;
                i := i+1;
            od;
            #as i is always positive, if rbracket is -1 that means that
            #the found left bracket has no corresponding right bracket.
            #note: if this never occurs then every left bracket has a
            #corresponding right bracket and as the number of each bracket
            #is equal every right bracket has a corresponding left bracket
            #and the bracket structure is valid.
            if rbracket = -1 then
                ErrorNoReturn("invalid bracket structure");
            fi;
            #if rbracket is not followed by ^ then the value inside the
            #bracket is appended (recursion is used to remove any brackets
            #in this value)
            if rbracket = Size(word) or (not word[rbracket + 1] = '^') then
                Append(newword,
                       RemoveBrackets(word{[lbracket + 1 .. rbracket - 1]}));
            #if rbracket is followed by ^ then the value inside the
            #bracket is appended the given number of times
            else
                i := i + 2;
                while i <= Size(word) do
                    if word[i] in "0123456789" then
                        i := i + 1;
                    else
                        break;
                    fi;
                od;
                Append(newword, 
                       Concatenation(ListWithIdenticalEntries(
                       Int(word{[rbracket + 2 .. i - 1]}), 
                       RemoveBrackets(word{[lbracket + 1 .. rbracket - 1]}))));
                i := i - 1;
            fi;
        fi;   
        i := i + 1;
    od;
    return newword;

end;

RemovePowers := function(word)
    local index, i, newword, base_position;
    if word = "" then
        return "";
    fi;
    if word[1] = '^' or word[Size(word)] = '^' then
        ErrorNoReturn("invalid power structure");
    fi;
    #checks that all ^s have an exponent.
    for index in [1 .. Size(word)] do
        if word[index] = '^' then
            if not word[index + 1] in "0123456789" then
                ErrorNoReturn("invalid power structure");
            fi;
            if word[index - 1] in "0123456789^" then
                ErrorNoReturn("invalid power structure");
            fi;

        fi;
    od;
    newword := "";
    i := 1;
    while i <= Size(word) do
        #if last character reached there is no space for exponentiation.
        if i = Size(word) then
            Add(newword, word[i]);
            i := i + 1;
        #if the character is not being powered then it is left as it is.
        elif not word[i + 1] = '^' then
            Add(newword, word[i]);
            i := i + 1;
        else
            base_position := i;
            i := i + 2;
            if i <= Size(word) then
                #extracts the exponent from the string.
                while word[i] in "0123456789" do
                    i := i + 1;
                    if i > Size(word) then
                        break;
                    fi;
                od;
            fi;
            Append(newword,
                   ListWithIdenticalEntries(
                   Int(word{[base_position + 2 .. i - 1]}),
                   word[base_position]));
        fi;
    od;
    return newword;
    end;
