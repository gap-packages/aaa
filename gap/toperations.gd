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

DeclareOperation("InverseTransducer", [IsTransducer]);
DeclareOperation("TransducerProduct", [IsTransducer, IsTransducer]);
DeclareOperation("RemoveStatesWithIncompleteResponse", [IsTransducer]);
DeclareOperation("RemoveInaccessibleStates", [IsTransducer]);
DeclareOperation("CopyTransducerWithInitialState", [IsTransducer, IsPosInt]);
DeclareOperation("RemoveEquivalentStates", [IsTransducer]);
DeclareAttribute("IsInjectiveTransducer", IsTransducer);
DeclareAttribute("IsSurjectiveTransducer", IsTransducer);
DeclareAttribute("EqualImagePrefixes", IsTransducer);
DeclareAttribute("TransducerSynchronizingLength", IsTransducer);
DeclareAttribute("IsDegenerateTransducer", IsTransducer);
DeclareAttribute("IsSynchronousTransducer", IsTransducer);
DeclareOperation("IsomorphicInitialTransducers",[IsTransducer, IsTransducer]);
DeclareOperation("OmegaEquivalentTransducers",[IsTransducer, IsTransducer]);
DeclareAttribute("CombineOmegaEquivalentStates", IsTransducer);
DeclareAttribute("Order", IsTransducer);
DeclareAttribute("ImageAutomaton",IsTransducer);
DeclareAttribute("MinimiseTransducer", IsTransducer);
DeclareOperation("\*", [IsTransducer, IsTransducer]);
DeclareOperation("\=", [IsTransducer, IsTransducer]);
DeclareOperation("\^", [IsTransducer, IsPosInt]);