{
  inputs = {
    classyplate.flake = false;
    classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    direct-sqlite.flake = false;
    direct-sqlite.url = "github:IreneKnapp/direct-sqlite";
    flake-parts.url = "github:hercules-ci/flake-parts";
    fswatch.url = "github:eswar2001/watch/41839963961c6890c62a74d24056f4769fdf137b";
    haskell-flake.url = "github:srid/haskell-flake";
    inspection-testing.flake = false;
    inspection-testing.url = "github:nomeata/inspection-testing/18f40a0be7d78a23a344c1f94034bed645985915";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    references.url = "github:eswar2001/references/35912f3cc72b67fa63a8d59d634401b79796469e";
  };
  outputs =
    inputs @ { self
    , nixpkgs
    , flake-parts
    , ...
    }:
    let
      # Overriding GHC to use perf counters provided by linux perf tools
      ghc-overlay = self: super: {
        haskell = super.haskell // {
          compiler = super.haskell.compiler // {
            ghc8107-perf-events = (super.haskell.compiler.ghc8107.overrideAttrs (drv: {
              src = ./ghc-8.10.7-sdist.tar.xz;
              patches = drv.patches ++ [ ./ghc-patches/0001-Patch-primop-update.patch ./ghc-patches/0001-Add-a-perf-counters-RTS-flag-to-enable-linux-perf-co.patch ./ghc-patches/0001-Disable-LINUX_PERF_EVENTS-and-improve-the-compile-sp.patch ];
              preConfigure = ''
                echo ${drv.version} >VERSION
                patchShebangs boot
                ./boot
              '' + drv.preConfigure or "";
            })).override {
              bootPkgs = super.haskell.packages.ghc865Binary // {
                happy = super.haskell.packages.ghc865Binary.happy_1_19_12;
              };
            };
          };
          packages = super.haskell.packages // {
            ghc8107-perf-events = super.haskell.packages.ghc8107.override {
              buildHaskellPackages = self.buildPackages.haskell.packages.ghc8107-perf-events;
              ghc = self.buildPackages.haskell.compiler.ghc8107-perf-events;
            };
          };
        };
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { self'
        , pkgs
        , system
        , lib
        , config
        , ...
        }: {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              ghc-overlay
            ];
          };
          haskellProjects.default = {
            imports = [
              inputs.fswatch.haskellFlakeProjectModules.output
              inputs.references.haskellFlakeProjectModules.output
            ];
            basePackages = pkgs.haskell.packages.ghc8107-perf-events;
            packages = {
              classyplate.source = inputs.classyplate;
              Cabal.source = "3.2.1.0";
              Glob.source = "0.9.3";
              minisat-solver.source = "0.1";
              portable-lines.source = "0.1";
              text-format.source = "0.3.2";
              text.source = "1.2.5.0";
              parsec.source = "3.1.14.0";
              http2.source = "3.0.0";
              recv.source = "0.0.0";
              network.source = "3.1.2.8";
              aeson.source = "1.5.4.1";
              th-abstraction.source = "0.4.5.0";
              strict.source = "0.4.0.1";
              dlist.source = "0.8.0.8";
              knob.source = "0.1.1";
            };
            settings = {
              knob = {
                jailbreak = true;
              };
              aeson = {
                jailbreak = true;
                check = false;
              };
              fourmolu = {
                check = false;
                haddock = false;
                jailbreak = true;
              };
              Diff = {
                check = false;
                haddock = false;
              };
              Glob = {
                jailbreak = true;
              };
              parsec = {
                jailbreak = true;
              };
              Cabal = {
                jailbreak = true;
              };
              http2 = {
                check = false;
              };
              co-log-core = {
                jailbreak = true;
              };
              hls-plugin-api = {
                check = false;
                haddock = false;
                jailbreak = true;
              };
              haskell-tools-builtin-refactorings = {
                check = false;
              };
              haskell-tools-daemon = {
                check = false;
              };
              ormolu = {
                check = false;
                haddock = false;
                jailbreak = true;
              };
            };
          };
          packages = {
            default =
              let
                localCabalPackages =
                  builtins.map
                    (p:
                      if p.exes != { }
                      then lib.getBin p.package
                      else null)
                    (lib.attrValues config.haskellProjects.default.outputs.packages);
              in
              pkgs.symlinkJoin {
                name = "haskell-tools";
                paths = localCabalPackages;
              };
          };
        };
    };
}
