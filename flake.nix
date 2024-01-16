{
  description = "Nix devenv";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      {
        # `nix develop`
        devShells.default = pkgs.mkShell rec {
          buildInputs = with pkgs; [
            pkg-config
            rust-bin.nightly.latest.default
            cargo-udeps
            cargo-nextest
            cargo-watch
            cargo-insta
            cargo-expand
            bacon
            act

            stdenv.cc.cc.lib
            zlib
            ncurses
            libffi
            libxml2
            llvmPackages_16.llvm
          ];

          shellHook = ''
            #export LLVM_SYS_160_PREFIX=$(dirname $(dirname $(which llvm-config)));
            export LLVM_SYS_160_PREFIX=${pkgs.llvmPackages_16.llvm.dev};
          '';

          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";

        };
      }
    );
}
