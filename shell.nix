{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
(import ./default.nix { inherit nixpkgs compiler; }).issue-tracker.env
