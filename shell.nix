with (import <nixpkgs> {});

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
  ];
}
