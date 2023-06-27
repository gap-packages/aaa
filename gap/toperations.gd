#############################################################################
##
#W  toperations.gd
#Y  Copyright (C) 2017                               Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains the declaration of operations that relate to transducers.

DeclareOperation("InverseGNSTransducer", [IsGNSTransducer]);
DeclareOperation("GNSTransducerProduct", [IsGNSTransducer, IsGNSTransducer]);
DeclareOperation("RemoveIncompleteResponseFromStates",[IsGNSTransducer]);
DeclareOperation("RemoveInaccessibleStates", [IsGNSTransducer]);
DeclareOperation("CopyGNSTransducerWithInitialState", [IsGNSTransducer, IsPosInt]);
DeclareAttribute("IsInjectiveGNSTransducer", IsGNSTransducer);
DeclareAttribute("IsSurjectiveGNSTransducer", IsGNSTransducer);
DeclareAttribute("IsBijectiveGNSTransducer", IsGNSTransducer);
DeclareAttribute("GNSTransducerImageAutomaton", IsGNSTransducer);
DeclareAttribute("GNSTransducerConstantStateOutputs", IsGNSTransducer);
DeclareAttribute("IsDegenerateGNSTransducer", IsGNSTransducer);
DeclareAttribute("IsMinimalGNSTransducer", IsGNSTransducer);
DeclareAttribute("CombineEquivalentStates", IsGNSTransducer);
DeclareAttribute("MinimalGNSTransducer", IsGNSTransducer);
DeclareAttribute("IsSynchronousGNSTransducer", IsGNSTransducer);
DeclareAttribute("GNSTransducerOrder", IsGNSTransducer);
DeclareOperation("IsomorphicInitialGNSTransducers", [IsGNSTransducer, IsGNSTransducer]);
DeclareOperation("OmegaEquivalentGNSTransducers", [IsGNSTransducer, IsGNSTransducer]);
DeclareOperation("EqualGNSTransducers", [IsGNSTransducer, IsGNSTransducer]);
DeclareOperation("\*", [IsGNSTransducer, IsGNSTransducer]);
DeclareOperation("\^", [IsGNSTransducer, IsInt]);
DeclareOperation("\^", [IsGNSTransducer, IsGNSTransducer]);
DeclareOperation("FixedOutputDigraph", [IsGNSTransducer, IsDenseList]);
DeclareAttribute("IsSynchronizingGNSTransducer", IsGNSTransducer);
DeclareAttribute("IsBisynchronizingGNSTransducer", IsGNSTransducer);
DeclareAttribute("GNSTransducerSynchronizingLength", IsGNSTransducer);
