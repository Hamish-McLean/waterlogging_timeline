{
  description = "Waterlogging timeline R project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    # Use eachSystem to define outputs for multiple systems
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        # Define the development shell
        devShells.default = pkgs.mkShell {
          # buildInputs lists the packages that will be available in the shell.
          buildInputs = with pkgs; [
            # python3Minimal
            radian
            R
            rPackages.languageserver
            rPackages.renv
            rPackages.testthat
          ];
        };
      }
    );
}
