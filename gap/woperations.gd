#############################################################################
##
#W  woperations.gd
#Y  Copyright (C) 2017                               Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains the declaration of operations that relate to words accepted
# by transducers.

DeclareOperation("IsPrefix", [IsList, IsList]);
DeclareOperation("Minus", [IsList, IsList]);
DeclareOperation("PreimageConePrefixes", [IsDenseList, IsPosInt,
                                          IsGNSTransducer]);
DeclareOperation("GreatestCommonPrefix", [IsDenseList]);
DeclareOperation("ImageConeLongestPrefix", [IsDenseList, IsPosInt,
                                            IsGNSTransducer]);
DeclareOperation("MinimalWords", [IsDenseList]);
DeclareOperation("MaximalWords", [IsDenseList]);
DeclareOperation("IsCompleteAntichain", [IsDenseList, IsPosInt, IsPosInt]);
