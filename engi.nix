{ mkDerivation, aeson, base, base16-bytestring, bytestring
, containers, cryptohash-sha256, directory, filepath, lib
, optparse-applicative, QuickCheck, tasty, tasty-hunit
, tasty-quickcheck, temporary, text, time, zstd
}:
mkDerivation {
  pname = "engi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring containers
    cryptohash-sha256 directory filepath text time zstd
  ];
  executableHaskellDepends = [
    base bytestring optparse-applicative text time
  ];
  testHaskellDepends = [
    base base16-bytestring bytestring cryptohash-sha256 directory
    filepath QuickCheck tasty tasty-hunit tasty-quickcheck temporary
    text time zstd
  ];
  description = "Archive closed agent sessions: strip, compress, verify";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "engi";
}
