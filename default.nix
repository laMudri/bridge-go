{ mkDerivation, base, base-unicode-symbols, containers_0_5_6_3
, containers-unicode-symbols, stdenv
}:
mkDerivation {
  pname = "bridge-go";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base-unicode-symbols containers_0_5_6_3 containers-unicode-symbols
  ];
  homepage = "http://senseis.xmp.net/?BridgeGo";
  description = "Interactive implementation of Bridge Go";
  license = stdenv.lib.licenses.gpl2;
}
