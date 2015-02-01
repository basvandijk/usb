let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      usb = self.callPackage ./usb.nix {};
      bindingsLibusb = self.callPackage ../bindings-libusb {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.usb.name;
     buildInputs = [
       pkgs.pkgconfig
       pkgs.libusb1
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.usb.propagatedNativeBuildInputs)))
     ];
   }
