{ cabal, bindingsLibusb, text, vector }:

cabal.mkDerivation (self: {
  pname = "usb";
  version = "1.3.0.0";
  src = ./.;
  buildDepends = [ bindingsLibusb text vector ];
  meta = {
    homepage = "http://basvandijk.github.com/usb";
    description = "Communicate with USB devices";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
