metro - Metronome and Music Practice CLI
===

`metro` is a command line metronome and music practice tool. It is under active development, but probably has the following features:

 * Metronome, driven by either terminal or command line interfaces
 * Ring recorder, allowing playback of several most recent seconds or minutes during a practice session

Build Requirements
---

 * Install `nix`: https://nixos.org/download.html#nix-quick-install 
 * Execute: `nix-shell`

That will download the world according to `metro`, install all build tools and depedencies, and then execute a shell that has access to all required build tools and dependencies.

From within that `nix-shell` environment:

 * Build the project: `cabal build`

 * Run the built exe: `cabal run -- metro --help`

 * Execute tests: `cabal test`

