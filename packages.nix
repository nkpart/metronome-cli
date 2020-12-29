{}:
let 
  pkgs = import <nixpkgs> {};
  hl = pkgs.haskell.lib;
  hp = pkgs.haskell.packages.ghc884.override {
    overrides = self: super: {
      # proteaaudio = (self.callHackage "proteaaudio" "0.8.0" {}).overrideAttrs(old: {
      #   buildInputs = old.buildInputs ++ [pkgs.darwin.apple_sdk.frameworks.CoreAudio];
      # });
      xxxmwc-random = self.callHackage "mwc-random" "0.15.0.1" {
        random = hl.dontCheck (self.callHackage "random" "1.2.0" {
          splitmix = hl.dontCheck (self.callHackage "splitmix" "0.1.0.3" {});

        });
      };
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
