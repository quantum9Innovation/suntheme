{
  description = "Suntheme's development and build environment";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);

      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          hsPkgs = pkgs.haskell.packages.ghc982.override {
            overrides =
              self: super:
              let
                inherit (pkgs.haskell.lib.compose) markUnbroken doJailbreak;
                inherit (pkgs.lib) pipe;
                superUnbreak =
                  x:
                  pipe x [
                    markUnbroken
                    doJailbreak
                  ];
              in
              {
                # solar gets very mad because every other package depends on
                # time 1.10+  while it wants 1.10 so we shut it up
                solar = superUnbreak super.solar;
                # request gets very mad because every other package depends on
                # bytestring 0.12.1.0  while it wants 0.11.5.3 so we shut it up
                request = superUnbreak super.request;
              };
          };
        in
        {
          default = pkgs.haskell.lib.doJailbreak (hsPkgs.callCabal2nix "suntheme" ./. { });
        }
      );
    };
}
