{ mkDerivation, base, bindings-libusb, bytestring, containers
, ghc-prim, stdenv, text, vector
}:
mkDerivation {
  pname = "usb";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [
    base bindings-libusb bytestring containers ghc-prim text vector
  ];
  homepage = "http://basvandijk.github.com/usb";
  description = "Communicate with USB devices";
  license = stdenv.lib.licenses.bsd3;
}
