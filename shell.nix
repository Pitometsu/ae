with import <nixpkgs> {};

mkShell {
  buildInputs = [
    zlib.out zlib.dev ripgrep tree
    cacert wget gnupg coreutils
    which less curl tree gitMinimal
  ] ++ (with haskellPackages; [
    ghc cabal-install ghci
    hasktags hindent hlint
  ]);
}
