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

DeclareRepresentation("IsGNSTransducer", IsComponentObjectRep and
                      IsAttributeStoringRep,
                                             ["InputAlphabet",
                                              "OutputAlphabet",
                                              "States",
                                              "TransitionFunction",
                                              "OutputFunction",
                                              "GNSTransducerFunction"]);
DeclareOperation("GNSTransducer", [IsPosInt, IsPosInt, IsDenseList, IsDenseList]);
DeclareOperation("GNSTransducerFunction",
                 [IsGNSTransducer, IsList, IsPosInt]);
DeclareOperation("OutputFunction", [IsGNSTransducer]);
DeclareOperation("TransitionFunction", [IsGNSTransducer]);
DeclareOperation("InputAlphabet", [IsGNSTransducer]);
DeclareOperation("OutputAlphabet", [IsGNSTransducer]);
DeclareOperation("States", [IsGNSTransducer]);
DeclareOperation("NrStates", [IsGNSTransducer]);
DeclareOperation("NrOutputSymbols", [IsGNSTransducer]);
DeclareOperation("NrInputSymbols", [IsGNSTransducer]);
DeclareOperation("IdentityGNSTransducer", [IsPosInt]);
DeclareOperation("AlphabetChangeGNSTransducer", [IsPosInt, IsPosInt]);
DeclareOperation("RandomGNSTransducer", [IsPosInt, IsPosInt]);
