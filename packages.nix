{}:
let 
  pkgs = import <nixpkgs> {};
  hl = pkgs.haskell.lib;
  hp = pkgs.haskell.packages.ghc884.override {
    overrides = self: super: {
      metronome_cli = self.callCabal2nix "metronome-cli" ./. { };
    };
  };

in {
  shell = hp.shellFor{
    name = "dev";
    packages = p: [p.metronome_cli];
  };
  metronome-cli = hp.metronome_cli;
}
