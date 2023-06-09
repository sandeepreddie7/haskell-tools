{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    systems.url = "github:nix-systems/default";
    flake-root.url = "github:srid/flake-root";
    nixpkgs-21_11.url = "github:nixos/nixpkgs/nixos-21.11"; # Used for ormolu
    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";
    treefmt-nix.url = "github:numtide/treefmt-nix";

    # Commonly useful flakes
    mission-control.url = "github:Platonic-Systems/mission-control";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    classyplate.flake = false;
    watch.url = "github:Chaitanya-nair/watch/cdeb9dd7c7e3e0aff11bb826c47ae8c782db27a2";
    watch.flake = false;
    references.url = "github:Chaitanya-nair/references/cdeb9dd7c7e3e0aff11bb826c47ae8c782db27a2";
    references.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          # basePackages = pkgs.haskellPackages;
          basePackages = pkgs.haskell.packages.ghc8107;

          # Extra package information. See https://haskell.flake.page/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          packages = {
            classyplate.source = inputs.classyplate;
            watch.source = inputs.watch;
            references.source = inputs.references;
            template-haskell.source = "2.16.0.0";
            th-abstraction.source = "0.4.5.0";
            text.source = "1.2.5.0";
            knob.source = "0.1.1";
            Diff.source = "0.3.4";
            Glob.source = "0.9.3";
            minisat-solver.source = "0.1";
            portable-lines.source = "0.1";
            text-format.source = "0.3.2";
            parsec.source = "3.1.14.0";
            warp.source = "3.3.19";
            http2.source = "3.0.0";
            recv.source = "0.0.0";
            network.source = "3.1.2.8";
            aeson.source = "1.5.4.1";
            strict.source = "0.4.0.1";
            dlist.source = "0.8.0.8";
            Cabal.source = "3.0.0.0";
            # shower.source = inputs.shower; 
          };
          # settings = { 
          #   aeson = {
          #     check = false;
          #   };
          #   relude = {
          #     haddock = false;
          #     broken = false;
          #   };
          # };
          settings = {
            classyplate = {
              jailbreak = true;
              broken = false;
            };
            references.broken = false;
            fswatch.broken = false;
            knob.jailbreak = true;
            template-haskell.jailbreak = true;
          };

          # devShell = {
          #  # Enabled by default
          #  enable = true;  
          #
          #  # Programs you want to make available in the shell.
          #  # Default programs can be disabled by setting to 'null'
          #  tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          #
          #  hlsCheck.enable = true;
          # };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.haskell-tools-ast;
      };
    };
}
