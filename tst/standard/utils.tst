############################################################################
##
#W  standard/transducer.tst
#Y  Copyright (C) 2017                                 Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("aaa package: standard/utils.tst");
gap> LoadPackage("aaa", false);;

#T# DotGNSTransducer
gap> T := IdentityGNSTransducer(3);;
gap> DotGNSTransducer(T);
"//dot\ndigraph finite_state_machine{\nrankdir=LR;\nnode [shape=circle]\n1\n1 \
-> 1 [label=\"0|0\"]\n1 -> 1 [label=\"1|1\"]\n1 -> 1 [label=\"2|2\"]\n}\n"

#
gap> STOP_TEST("aaa package: standard/utils.tst");
