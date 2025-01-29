# save this as shell.nix
{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  packages = [
    pkgs.clojure
    pkgs.clojure-lsp
    pkgs.clj-kondo
    pkgs.cljfmt
    pkgs.jdk23];
}
