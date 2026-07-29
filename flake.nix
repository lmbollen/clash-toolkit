{
  description = "VS Code Extension Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { system = system; config.allowUnfree = true; };

        # GHC with Clash and required plugins
        ghcWithClash = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          clash-ghc
          clash-prelude
          clash-lib
          ghc-typelits-natnormalise
          ghc-typelits-extra
          ghc-typelits-knownnat
          doctest-parallel
          tasty
          tasty-hedgehog
          tasty-th
          QuickCheck
          hedgehog
          clash-prelude-hedgehog
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Node.js and npm for extension development
            nodejs_20

            # VS Code extension development tools
            nodePackages.npm
            nodePackages.typescript
            nodePackages.typescript-language-server

            # VS Code for testing
            # vscode-fhs

            # Haskell toolchain with Clash
            ghcWithClash
            cabal-install
            haskell-language-server

            # Hardware synthesis tools
            yosys

            # FPGA place-and-route tools
            nextpnr  # nextpnr-ice40, nextpnr-ecp5, etc.


            # Documentation — builds the book/ directory
            mdbook

            # Useful utilities
            git
          ];

          shellHook = ''
            echo "======================================"
            echo "Clash VS Code Extension Development"
            echo "======================================"
            echo ""
            echo "Node version: $(node --version)"
            echo "npm version: $(npm --version)"
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version | head -n1)"
            echo "Clash version: $(clash --version 2>/dev/null || echo 'Available via ghcWithClash')"
            echo "HLS: $(haskell-language-server --version 2>/dev/null | head -n1 || echo 'installed')"
            echo "Yosys version: $(yosys -V | head -n1)"
            echo "nextpnr-ecp5: $(nextpnr-ecp5 --version 2>/dev/null | head -n1 || echo 'installed')"
            echo "mdbook version: $(mdbook --version)"
            echo ""
            echo "Extension Development:"
            echo "  1. Run 'npm install' to install dependencies"
            echo "  2. Press F5 in VS Code to launch the extension"
            echo ""
            echo "Test Clash Project (in test-project/):"
            echo "  1. cd test-project"
            echo "  2. cabal build"
            echo "  3. cabal run clash -- Example.Project --verilog"
            echo ""
          '';
        };
      }
    );
}
