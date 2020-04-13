{ mkDerivation, attoparsec, base, base-compat, base-orphans
, base16-bytestring, bytestring, containers, deepseq, Diff
, directory, dlist, filepath, generic-deriving, ghc-prim, hashable
, hashable-time, integer-logarithms, primitive, QuickCheck
, quickcheck-instances, scientific, stdenv, tagged, tasty
, tasty-golden, tasty-hunit, tasty-quickcheck, template-haskell
, text, th-abstraction, time, time-compat, unordered-containers
, uuid-types, vector
}:

mkDerivation {
  pname = "aeson";
  version = "1.4.6.0";
  sha256 = "12s8nfsady47zlz94f7m978irwwj0l0v2x41hk8w1i14wb3b4gwj";
  libraryHaskellDepends = [
    attoparsec base base-compat bytestring containers deepseq dlist
    ghc-prim hashable primitive scientific tagged template-haskell text
    th-abstraction time time-compat unordered-containers uuid-types
    vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers Diff directory dlist filepath
    generic-deriving ghc-prim hashable hashable-time integer-logarithms
    QuickCheck quickcheck-instances scientific tagged tasty
    tasty-golden tasty-hunit tasty-quickcheck template-haskell text
    time time-compat unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
