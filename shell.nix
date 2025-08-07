#with (import <nixpkgs> {});
with (import (builtins.fetchTarball {
  name = "nixos-25.05";
  url = "https://github.com/nixos/nixpkgs/archive/ce01daebf8489ba97bd1609d185ea276efdeb121.tar.gz";
  sha256 = "10cqhkqkifcgyibj9nwxrnq424crfl40kwr3daky83m2fisb4f6p";
}) {});
mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      bytestring
      unordered-containers
      statistics
      wide-word
      treefold
    ]))
    emacs
    gnuplot
  ];
}
