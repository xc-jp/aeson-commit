{ mkDerivation, array, base, directory, pretty, process, QuickCheck
, stdenv, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "Diff";
  version = "0.4.0";
  sha256 = "1is9y5rlqyxacnj6kbi6h9laym5shp699r0hkj5p9d6qi84sr43j";
  libraryHaskellDepends = [ array base pretty ];
  testHaskellDepends = [
    array base directory pretty process QuickCheck test-framework
    test-framework-quickcheck2
  ];
  description = "O(ND) diff algorithm in haskell";
  license = stdenv.lib.licenses.bsd3;
}
