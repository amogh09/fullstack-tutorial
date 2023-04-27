{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    nodejs
    (nodePackages.create-react-app)
    (nodePackages.typescript)
    (nodePackages.typescript-language-server)
    (
      haskellPackages.ghcWithPackages
      (
        haskellPkgs: with haskellPkgs;
        [ cabal-install haskell-language-server ormolu cabal-fmt ]
      )
    )
    httpie
    sqlite
  ];
}
