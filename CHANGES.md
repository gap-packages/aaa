This file describes changes in the aaa package.

## Unreleased

- Rename transducers to GNS transducers, e.g. `IsTransducer` to
  `IsGNSTransducer` and `InverseTransducer` to `InverseGNSTransducer`
- Allow the output words of transducers to be periodic lists from the fr
  package
- Remove `Splash`, which the Digraphs package provides
- Require GAP >= 4.10, as Digraphs does
- Document the shorthand commands and revise the manual

## 0.1.0 (2022-02-09)

- Add `IsSynchronizingTransducer`, `IsBisynchronizingTransducer` and
  `TransducerSynchronizingLength`
- Make `TransducerConstantStateOutputs` return `fail` for degenerate
  transducers
- Update the manual

## 0.0.1 (2017-03-30)
