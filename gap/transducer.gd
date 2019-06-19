#############################################################################
##
#W  transducer.gd
#Y  Copyright (C) 2017                               Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains the declarations of the representation of a transducer and
# that of objects that relate to this package. The appropiate ViewObj functions
# are defined in the transducer.gi file.

DeclareRepresentation("IsTransducer", IsComponentObjectRep and
                      IsAttributeStoringRep, ["InputAlphabet",
                                              "OutputAlphabet",
                                              "States",
                                              "TransitionFunction",
                                              "OutputFunction",
                                              "TransducerFunction"]);
DeclareOperation("Transducer", [IsPosInt, IsPosInt, IsDenseList, IsDenseList]);
DeclareOperation("RandomTransducer", [IsPosInt, IsPosInt]);
DeclareOperation("RandomBijectiveTransducer",[IsPosInt, IsPosInt]);
DeclareOperation("IdentityTransducer",[IsPosInt]);
DeclareOperation("TransducerFunction", [IsTransducer, IsDenseList, IsPosInt]);
DeclareOperation("OutputFunction", [IsTransducer]);
DeclareOperation("GeneratedTransducers", [IsPosInt]);
DeclareOperation("TransitionFunction", [IsTransducer]);
DeclareOperation("NrTransducers", [IsPosInt, IsPosInt]);
DeclareOperation("TransducerByNumber", [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("InputAlphabet", [IsTransducer]);
DeclareOperation("OutputAlphabet", [IsTransducer]);
DeclareOperation("States", [IsTransducer]);
DeclareOperation("NrStates", [IsTransducer]);
DeclareOperation("NrOutputSymbols", [IsTransducer]);
DeclareOperation("NrInputSymbols", [IsTransducer]);
