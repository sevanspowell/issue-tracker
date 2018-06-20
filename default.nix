{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;

  pkgsTmp = if compiler == "default"
         then pkgs.haskellPackages
         else pkgs.haskell.packages.${compiler};
  haskellPackages = pkgsTmp.extend (self: super: {
    beam-core   = self.callHackage "beam-core" "0.7.2.0" {};
    beam-sqlite = self.callHackage "beam-sqlite" "0.3.2.0" {};
    beam-postgres = self.callHackage "beam-postgres" "0.3.2.0" {};
    beam-migrate = self.callHackage "beam-migrate" "0.3.2.0" {};
    haskell-src-exts = self.callHackage "haskell-src-exts" "1.20.0" {};
  });
in
  {
    issue-tracker = haskellPackages.callPackage ./issue-tracker.nix {};
  }
