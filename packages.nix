{ ... }:
let 
  pkgs = import <nixpkgs> {};
  hl = pkgs.haskell.lib;
  hp = pkgs.haskell.packages.ghc8104.override {
    overrides = self: super: {
      metronome_cli = self.callCabal2nix "metronome-cli" ./. { };
    };
  };

in {
  shell = hp.shellFor{
    name = "dev";
    packages = p: [p.metronome_cli];
    buildInputs = [ pkgs.toilet pkgs.darwin.apple_sdk.frameworks.CoreAudio ];
  };
  metronome-cli = hp.metronome_cli;
}
