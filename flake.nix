{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    classyplate.flake = false;
    watch.url = "github:Chaitanya-nair/watch/cdeb9dd7c7e3e0aff11bb826c47ae8c782db27a2";
    watch.flake = false;
    references.url = "github:eswar2001/references/35912f3cc72b67fa63a8d59d634401b79796469e";
    references.flake = true;
    inspection-testing.url = "github:nomeata/inspection-testing/18f40a0be7d78a23a344c1f94034bed645985915";
    inspection-testing.flake = false;
    direct-sqlite.url = "github:IreneKnapp/direct-sqlite";
    direct-sqlite.flake = false;
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {
        self',
        pkgs,
        ...
      }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc8107;

          packages = {
            # ansi-terminal.source = "0.11.1";
            # array.source = "0.5.4.0";
            # async.source = "2.2.4";
            # base-orphans.source = "0.8.6";
            # base.source = "4.14.3.0";
            # binary.source = "0.8.8.0";
            # bytestring.source = "0.10.12.0";
            # call-stack.source = "0.3.0";
            # colour.source = "2.3.6";
            # containers.source = "0.6.5.1";
            # deepseq.source = "1.4.4.0";
            # directory.source = "1.3.6.0";
            # either.source = "5.0.1.1";
            # exceptions.source = "0.10.4";
            # filepath.source = "1.4.2.1";
            # ghc-boot-th.source = "8.10.7";
            # hashable.source = "1.3.0.0";
            # haskell-tools-ast.source = "1.1.1.0";
            # hfsevents.source = "0.1.6";
            # hie-compat.source = "0.3.1.1";
            # integer-gmp.source = "1.0.3.0";
            # mtl.source = "2.2.2";
            # Only.source = "0.1";
            # parsec.source = "3.1.14.0";
            # pretty.source = "1.1.3.6";
            # primitive.source = "0.7.3.0";
            # process.source = "1.6.13.2";
            # random.source = "1.2.1.1";
            # shower.source = inputs.shower;
            # splitmix.source = "0.1.0.4";
            # stm.source = "2.5.0.1";
            # template-haskell.source = "2.16.0.0";
            # text-short.source = "0.1.3";
            # text.source = "1.2.5.0";
            # time-compat.source = "1.9.5";
            # time.source = "1.9.3";
            # transformers.source = "0.5.6.2";
            # unix-compat.source = "0.5.3";
            # unix.source = "2.7.2.2";
            # x509.source = "1.7.5";
            # zlib.source = "0.6.2.3";
            abstract-deque.source = "0.3";
            abstract-par.source = "0.3.3";
            aeson.source = "1.5.4.1";
            algebraic-graphs.source = "0.6.1";
            ansi-wl-pprint.source = "0.6.9";
            appar.source = "0.1.8";
            asn1-encoding.source = "0.9.6";
            asn1-parse.source = "0.9.5";
            asn1-types.source = "0.3.4";
            assoc.source = "1.0.2";
            attoparsec.source = "0.13.2.5";
            auto-update.source = "0.1.6";
            base-compat-batteries.source = "0.11.2";
            base-compat.source = "0.11.2";
            basement.source = "0.0.12";
            bifunctors.source = "5.5.11";
            blaze-builder.source = "0.4.2.2";
            blaze-markup.source = "0.8.2.8";
            bsb-http-chunked.source = "0.0.0.4";
            byteorder.source = "1.0.4";
            Cabal.source = "3.2.1.0";
            case-insensitive.source = "1.2.1.0";
            cereal.source = "0.5.8.2";
            classyplate.source = inputs.classyplate;
            comonad.source = "5.0.8";
            contravariant.source = "1.5.5";
            cryptonite.source = "0.29";
            data-default-class.source = "0.1.2.0";
            data-fix.source = "0.3.2";
            direct-sqlite.source = inputs.direct-sqlite;
            distributive.source = "0.6.2.1";
            dlist.source = "0.8.0.8";
            easy-file.source = "0.2.2";
            filemanip.source = "0.3.6.3";
            fsnotify.source = "0.3.0.1";
            haskeline.source = "0.8.0.0";
            hlint.source = "3.4.1";
            hourglass.source = "0.2.12";
            http-date.source = "0.0.11";
            http-types.source = "0.12.3";
            http2.source = "3.0.0";
            indexed-traversable.source = "0.1.2";
            inspection-testing.source = inputs.inspection-testing;
            instance-control.source = "0.1.2.0";
            integer-logarithms.source = "1.0.3.1";
            iproute.source = "1.7.12";
            math-functions.source = "0.3.4.2";
            memory.source = "0.15.0";
            monad-par-extras.source = "0.3.3";
            mwc-random.source = "0.15.0.2";
            network-byte-order.source = "0.1.6";
            network.source = "3.1.2.8";
            old-locale.source = "1.0.0.7";
            old-time.source = "1.1.0.3";
            OneTuple.source = "0.3.1";
            optparse-applicative.source = "0.16.1.0";
            parallel.source = "3.2.2.0";
            pem.source = "0.2.4";
            profunctors.source = "5.6.2";
            psqueues.source = "0.2.7.3";
            references.source = inputs.references;
            safe.source = "0.3.19";
            scientific.source = "0.3.7.0";
            semigroupoids.source = "5.3.6";
            simple-sendfile.source = "0.2.30";
            split.source = "0.2.3.4";
            StateVar.source = "1.2.2";
            streaming-commons.source = "0.2.2.3";
            strict.source = "0.4.0.1";
            syb.source = "0.7.2.1";
            tagged.source = "0.8.6.1";
            terminfo.source = "0.4.1.4";
            th-abstraction.source = "0.4.5.0";
            these.source = "1.1.1.1";
            time-manager.source = "0.0.0";
            transformers-compat.source = "0.6.6";
            uniplate.source = "1.6.13";
            unix-time.source = "0.4.7";
            unliftio-core.source = "0.2.0.1";
            unliftio.source = "0.2.20";
            unordered-containers.source = "0.2.16.0";
            uuid-types.source = "1.0.5";
            vault.source = "0.3.1.5";
            vector-algorithms.source = "0.8.0.4";
            vector-binary-instances.source = "0.2.5.2";
            vector-th-unbox.source = "0.2.2";
            vector.source = "0.12.3.1";
            wai.source = "3.2.3";
            watch.source = inputs.watch;
            word8.source = "0.1.3";
          };
          settings = {
            happy = {
              check = false;
            };
            classyplate = {
              broken = false;
              libraryProfiling = false;
              haddock = false;
            };
            Cabal = {
              jailbreak = true;
              check = false;
            };
            aeson = {
              jailbreak = true;
              check = false;
            };
            dlist = {
              jailbreak = true;
              check = false;
              haddock = false;
            };
            filemanip = {
              jailbreak = true;
              check = false;
              haddock = false;
            };
            OneTuple = {
              jailbreak = true;
              check = false;
              haddock = false;
            };
            fsnotify = {
              check = false;
              haddock = false;
            };
            algebraic-graphs = {
              jailbreak = true;
              check = false;
              haddock = false;
            };
            references = {
              check = false;
              haddock = false;
              libraryProfiling = false;
            };
            ghc-lib-parser = {
              check = false;
              haddock = false;
            };
            ghc-lib-parser-ex = {
              check = false;
              haddock = false;
            };
            stylish-haskell = {
              check = false;
              haddock = false;
            };
            hlint = {
              check = false;
              haddock = false;
              jailbreak = true;
            };
            fourmolu = {
              check = false;
              haddock = false;
              jailbreak = true;
            };
            ghcide = {
              check = false;
              haddock = false;
              jailbreak = true;
            };
          };
        };

        packages.default = self'.packages.haskell-tools-ast;
      };
    };
}