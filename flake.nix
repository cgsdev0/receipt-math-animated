{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { config, self', pkgs, lib, system, ... }: {
        # packages, devShells, checks, etc. go here
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            opam
            which
            bubblewrap
            autoconf
            gcc
          ];
          shellHook = ''
            export OPAMNOSANDBOX=1
            export OPAMDEPEXTYES=1
            export PATH="${pkgs.autoconf}/bin:${pkgs.gcc}/bin:${pkgs.which}/bin:$PATH"
            eval $(opam env --switch=./)
          '';
        };
      };

      flake = {
        # system-independent outputs go here (nixosModules, overlays, etc.)
      };
    };
}
