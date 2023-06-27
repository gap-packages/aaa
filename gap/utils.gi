#############################################################################
##
#W  utils.gi
#Y  Copyright (C) 2017                               Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains utility functions.

InstallGlobalFunction(AaaMakeDoc,
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("aaa")[1]!.InstallationPath, "/doc"),
                "main.xml", ["../PackageInfo.g",
                             "title.xml",
                             "transducer.xml",
                             "toperations.xml",
                             "utils.xml",
                             "woperations.xml"], "aaa", "MathJax", "../../..");
  return;
end);

################################################################################

InstallMethod(DotGNSTransducer, "for a transducer",
[IsGNSTransducer],   #The first version of this function was written by Michael Torpey
function(transducer)
  local i, j, label, m, n, out, st, str, verts;

  verts := States(transducer);
  out   := TransitionFunction(transducer);
  m     := NrStates(transducer);
  str   := "//dot\n";

  label := List(verts, x -> String(x));

  Append(str, "digraph finite_state_machine{\n");
  Append(str, "rankdir=LR;\n");
  Append(str, "node [shape=circle]\n");

  for i in verts do
    Append(str, Concatenation(label[i], "\n"));
  od;

  for i in verts do
    n := 0;
    for j in out[i] do
      n := n + 1;
      st := "";
      Append(st,String(PrePeriod(OutputFunction(transducer)[i][n])));
      if not Period(OutputFunction(transducer)[i][n]) = [] then
        Append(st, "(");
        Append(st, String(Period(OutputFunction(transducer)[i][n])));
        Append(st, ")ᐜ");
      fi;
      RemoveCharacters(st, " [,]");
      Append(str, Concatenation(label[i], " -> ", label[j]));
      Append(str, Concatenation(" [label=\"", String(n - 1), "|", st, "\"]"));
      Append(str, "\n");
    od;
  od;
  Append(str, "}\n");
  return str;
end);

