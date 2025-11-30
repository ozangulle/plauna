# save this as shell.nix
{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  buildInputs = [
    pkgs.clojure
    pkgs.clojure-lsp
    pkgs.clj-kondo
    pkgs.cljfmt
    #(pkgs.jdk25.override { enableJavaFX = true; })
    #pkgs.javaPackages.openjfx25
    #pkgs.xorg.libXtst
    #pkgs.xorg.libXxf86vm
    #pkgs.libGL
    #pkgs.glib.out
    #pkgs.gtk3
    pkgs.git-cliff
    pkgs.nodejs_22
    pkgs.tailwindcss_4];

  shellHook = ''
    export JAVA_HOME="${pkgs.jdk25}/lib/openjdk"
  '';
}
# export JAVAFX_PATH="${pkgs.javaPackages.openjfx25}/lib"
# export LD_LIBRARY_PATH="${pkgs.libGL}/lib:${pkgs.gtk3}/lib:${pkgs.glib.out}/lib:${pkgs.xorg.libXtst}/lib:${pkgs.xorg.libXxf85vm.out}/lib";
