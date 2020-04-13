{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }@args:

(import ./nix args).env
