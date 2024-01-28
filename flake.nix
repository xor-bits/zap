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
            # rust-bin.nightly.latest.default
            rustup
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
            llvmPackages_17.llvm
            llvmPackages_17.bintools
          ];

          shellHook = ''
            export LLVM_SYS_170_PREFIX=${pkgs.llvmPackages_17.llvm.dev};
          '';

          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";

        };
      }
    );
}
