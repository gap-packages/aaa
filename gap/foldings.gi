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
