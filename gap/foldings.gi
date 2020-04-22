#foldings

#checks if the given object is a folding stored as a 4-tuple domain, range, a list
#wose ith element is the position of the image of the ith vertex of the domain
#in the vertex list of the range and a list
#whose ith element is the position of the image of the ith edge of the dom in the edge
#list of the range
InstallMethod(IsFolding, "for a dense list",
[IsDenseList],
function(F)
  local temp, badedgepairs, optiontree, newoptions, currentpath,
        goodconfigurations, newoptionstemp, newoption, CoDomEdges,
        DomEdges, DomEdgesByInVertex, DomEdgesByOutVertex, edgepath,
        configurationstocheck, currentconfiguration, i, goodedgepairs,
        newpair, CoDomEdgesByInVertex, CoDomEdgesByOutVertex,
        newconfiguration, currentpair, failedpairs, pathcoverededges,
        coverededges, loopable;

#checking input has correct format
  if not Size(F) = 4 then return false; fi;
  if not IsDigraph(F[1]) then return false; fi;
  if not IsDigraph(F[2]) then return false; fi;
  if not IsDenseList(F[3]) then return false; fi;
  if not IsDenseList(F[4]) then return false; fi;

#checking functions have the correct domain and codomain
  if not Size(F[3]) = DigraphNrVertices(F[1]) then return false; fi;
  if not IsSubset(DigraphVertices(F[2]), F[3]) then return false; fi;
  if not Size(F[4]) = DigraphNrEdges(F[1]) then return false; fi;
  if not IsSubset([1 .. DigraphNrEdges(F[2])], F[3]) then return false; fi;

#checking that the function is a digraph homomorphism
  for i in [1 .. DigraphNrEdges(F[1])] do
    temp := DigraphEdges(F[1])[i];
    if not DigraphEdges(F[2])[F[4][i]] = [F[3][temp[1]], F[3][temp[2]]] then
      return false;
    fi;
  od;

  DomEdges := DigraphEdges(F[1]);
  CoDomEdges := DigraphEdges(F[2]);

  DomEdgesByInVertex := List(DigraphVertices(F[1]), x -> Filtered([1 .. Size(DomEdges)], y -> DomEdges[y][1] = x));
  CoDomEdgesByInVertex := List(DigraphVertices(F[2]), x -> Filtered([1 .. Size(CoDomEdges)], y -> CoDomEdges[y][1] = x));
  DomEdgesByOutVertex := List(DigraphVertices(F[1]), x -> Filtered([1 .. Size(DomEdges)], y -> CoDomEdges[y][2] = x));
  CoDomEdgesByOutVertex := List(DigraphVertices(F[2]), x -> Filtered([1 .. Size(CoDomEdges)], y -> CoDomEdges[y][2] = x));

#checking forwards injective
  badedgepairs := Filtered(Cartesian([1 .. Size(DomEdges)], [1 .. Size(DomEdges)]), x -> (x[1] < x[2]) and (F[4][x[1]] = F[4][x[2]]));
  goodedgepairs := [];
  failedpairs := [];
  while not badedgepairs = [] do
    newpair := Remove(badedgepairs);
    optiontree := [[newpair]];
    currentpath := [1];
    while not optiontree = [] do
      currentpair := optiontree[Size(currentpath)][currentpath[Size(currentpath)]];
      newoptions := Cartesian(DomEdgesByInVertex[DomEdges[currentpair[1]][2]], DomEdgesByInVertex[DomEdges[currentpair[2]][2]]);
      newoptions := Filtered(newoptions, x -> (F[4][x[1]] = F[4][x[2]]) and (x[1] < x[2]) and (not x in goodedgepairs));
      if newoptions = [] then
        currentpath[Size(currentpath)] := currentpath[Size(currentpath)] + 1;
        while currentpath <> [] and currentpath[Size(currentpath)] > Size(optiontree[Size(currentpath)]) do
          goodedgepairs := Union(goodedgepairs, Remove(optiontree));
          Remove(currentpath);
          if Size(currentpath) > 0 then
            currentpath[Size(currentpath)] := currentpath[Size(currentpath)] + 1;
          fi;
        od;
      else
        Add(optiontree, newoptions);
        Add(currentpath, 1);
      fi;
      #when we know we have a problem
      edgepath := List([1 .. Size(currentpath)], x -> optiontree[x][currentpath[x]]);
      if not Size(Set(edgepath)) = Size(edgepath) then
        Add(failedpairs, optiontree[1][1]);
        break;
      fi;
    od;
  od;

#checking backwards injective
  badedgepairs := failedpairs;
  goodedgepairs := [];
  while not badedgepairs = [] do
    newpair := Remove(badedgepairs);
    optiontree := [[newpair]];
    currentpath := [1];
    while not optiontree = [] do
      currentpair := optiontree[Size(currentpath)][currentpath[Size(currentpath)]];
      newoptions := Cartesian(DomEdgesByOutVertex[DomEdges[currentpair[1]][2]], DomEdgesByOutVertex[DomEdges[currentpair[2]][2]]);
      newoptions := Filtered(newoptions, x -> (F[4][x[1]] = F[4][x[2]]) and (x[1] < x[2]) and (not x in goodedgepairs));
      if newoptions = [] then
        currentpath[Size(currentpath)] := currentpath[Size(currentpath)] + 1;
        while currentpath <> [] and currentpath[Size(currentpath)] > Size(optiontree[Size(currentpath)]) do
          goodedgepairs := Union(goodedgepairs, Remove(optiontree));
          Remove(currentpath);
          if Size(currentpath) > 0 then
            currentpath[Size(currentpath)] := currentpath[Size(currentpath)] + 1;
          fi;
        od;
      else
        Add(optiontree, newoptions);
        Add(currentpath, 1);
      fi;
      #when we know we have a problem
      edgepath := List([1 .. Size(currentpath)], x -> optiontree[x][currentpath[x]]);
      if not Size(Set(edgepath)) = Size(edgepath) then
        return false;
#        return [false, "inj2"];
      fi;
    od;
  od;


#checking surjective
#
  loopable := Filtered(DigraphVertices(F[2]), x-> IsReachable(F[2], x, x));
  coverededges := Filtered([1 .. Size(CoDomEdges)],
                  x -> ForAny(loopable, y-> IsReachable(F[2], CoDomEdges[x][2], y) and
                       ForAny(loopable, y-> IsReachable(F[2], y, CoDomEdges[x][1]))));


  goodconfigurations := [];
  configurationstocheck := List(coverededges, x -> [x, Filtered([1 .. Size(DomEdges)], y -> F[4][y] = x)]);
  while not configurationstocheck = [] do
    newconfiguration := Remove(configurationstocheck);
    optiontree := [[newconfiguration]];
    currentpath := [1];
    while not optiontree = [] do
      currentconfiguration := optiontree[Size(currentpath)][currentpath[Size(currentpath)]];
#     if currentconfiguration[2] <> [] then
      AddSet(goodconfigurations, StructuralCopy(currentconfiguration));
#     fi;
      newoptionstemp := Set(List(currentconfiguration[2], x -> DomEdges[x][2]));
      newoptionstemp := Union(List(newoptionstemp, x -> DomEdgesByInVertex[x]));
      newoptions := List(CoDomEdgesByInVertex[CoDomEdges[currentconfiguration[1]][2]], x -> [x, Filtered(newoptionstemp, y -> F[4][y] = x)]);
      newoptions := Filtered(newoptions, x -> not x in goodconfigurations);
      #finding an unwritable path
      if ForAny(newoptions, x-> x[2] = []) then
        return false;
#        return [false, "sur"];
      fi;

      if newoptions = [] then
        currentpath[Size(currentpath)] := currentpath[Size(currentpath)] + 1;
        while currentpath <> [] and currentpath[Size(currentpath)] > Size(optiontree[Size(currentpath)]) do
          Remove(currentpath);
          Remove(optiontree);
          if Size(currentpath) > 0 then
            currentpath[Size(currentpath)] := currentpath[Size(currentpath)] + 1;
          fi;
        od;
      else
       Add(optiontree, newoptions);
       Add(currentpath, 1);
      fi;     
    od;
  od;

  return true;
end);

#InstallMethod(LineFolding, "for a dense list",
#[IsDigraph, IsInt, IsInt],
#function(D, p, f)
#  if not p>0 and f>0 then
#    return fail;
#  fi;
#  edges := 
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


