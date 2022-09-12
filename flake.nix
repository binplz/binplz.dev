{
  description = "binplz";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    programs-sqlite.url = "github:jonascarpay/programs.sqlite";
    programs-sqlite.flake = false;
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    let

      system = "x86_64-linux";

      pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };

      programs-db = "${inputs.programs-sqlite}/nixos-22.05/programs.sqlite";

      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              binplz-server = hfinal.callCabal2nix "binplz-server" ./server { };
            };
        };
        binplz-server = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.binplz-server;
        binplz-docs = final.callPackage ./docs { };

        # Small helper script for connecting to the nixbuild.net administration shell.
        connect-nixbuildnet = final.writeShellApplication {
          name = "connect-nixbuild.net";
          runtimeInputs = [ final.openssh final.rlwrap ];
          text = ''
            # The nixbuild.net shell doesn't yet have read-line capabilities,
            # so we use rlwrap here to give us read-line capabilities (like the
            # ability to press the up arrow to go back to the previous
            # command).
            rlwrap ssh -i ./secrets/plaintext/nixbuild.pem eu.nixbuild.net shell
          '';
        };
      };

      image-name = "binplz-ami-${system}";
      port = 80;

      config = {
        amazon = {
          amazonImage.sizeMB = 4096;
          amazonImage.name = image-name;
        };
        qemu = {
          services.qemuGuest.enable = true;
          services.getty.autologinUser = "root";
          virtualisation.memorySize = 4096;
          virtualisation.forwardPorts = [
            { from = "host"; host.port = 8081; guest.port = port; }
            { from = "host"; host.port = 2222; guest.port = 22; }
          ];
        };
        nixbuild = {
          # This module expects the nixbuild private key to be present at /root/nixbuild.pem.
          # For the amazon image, we use `user_data` to pass the key.
          # For QEMU, I haven't looked into how to distribute the key yet, so QEMU doesn't use nixbuild atm. TODO
          # TODO make a SSOT for the key path.
          programs.ssh = {
            extraConfig = ''
              Host eu.nixbuild.net
                PubkeyAcceptedKeyTypes ssh-ed25519
                IdentityFile /root/nixbuild.pem
            '';
            knownHosts.nixbuild = {
              hostNames = [ "eu.nixbuild.net" ];
              publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPIQCZc54poJ8vqawd8TraNryQeJnvH1eLpIDgbiqymM";
            };
          };
          nix = {
            distributedBuilds = true;
            buildMachines = builtins.map
              (system: {
                inherit system;
                hostName = "eu.nixbuild.net";
                maxJobs = 100;
                supportedFeatures = [ "benchmark" "big-parallel" ];
              })
              [ "x86_64-linux" "aarch64-linux" "armv7l-linux" ];
            extraOptions = ''
              experimental-features = nix-command flakes
            '';
          };
        };
        global = { pkgs, ... }: {
          documentation.man.enable = false;
          nixpkgs.overlays = [ overlay ];
          system.stateVersion = "22.05";
          networking.firewall.allowedTCPPorts = [ port ];
          systemd.services.binplz-server = {
            enable = true;
            description = "Binplz binary server";
            wantedBy = [ "multi-user.target" ];
            script = ''
              ${pkgs.binplz-server}/bin/binplz-server \
                --port ${builtins.toString port} \
                --program-db ${programs-db}
            '';
            serviceConfig = {
              Restart = "always";
              Type = "simple";
              Environment = "PATH=\"/run/current-system/sw/bin:$PATH\"";
            };
          };
          users = {
            mutableUsers = false;
            users.root = {
              password = "nixos";
              openssh.authorizedKeys.keys =
                builtins.concatMap
                  (x: if builtins.isList x then x else [ ])
                  (builtins.split
                    "(ssh-[^\n]*)\n"
                    (builtins.readFile ./secrets/trustees));
            };
          };
          services = {
            openssh.enable = true;
            openssh.permitRootLogin = "prohibit-password";
          };
        };
      };

      server-shell = let hspkgs = pkgs.haskellPackages; in
        hspkgs.shellFor {
          withHoogle = true;
          packages = p: [ p.binplz-server ];
          buildInputs = [
            hspkgs.cabal-install
            hspkgs.haskell-language-server
            hspkgs.hlint
            hspkgs.ormolu
            pkgs.bashInteractive
            pkgs.rlwrap
            pkgs.sqlite
            pkgs.age
          ];
          BINPLZ_NIX_PROGRAM_DB = programs-db;
        };

      deploy-shell = pkgs.mkShell rec {
        packages = [
          pkgs.terraform
          pkgs.awscli2
          pkgs.rlwrap
          pkgs.sqlite
          pkgs.age
        ];
        TF_VAR_binplz_ami_path = "${inputs.self.packages.${system}.server-ami}/${image-name}.vhd";
        # Image name on AWS. The purpose is to invalidate when the image contents change.
        # TODO cleaner way of getting the store path than `rec`
        TF_VAR_binplz_image_name_hash =
          let hash = builtins.substring 11 8 TF_VAR_binplz_ami_path;
          in "${image-name}-${hash}";
      };

    in
    {
      inherit overlay;

      devShell.${system} = server-shell;

      devShells.${system} = {
        server = server-shell;
        deploy = deploy-shell;
      };

      defaultPackage.${system} = pkgs.binplz-server;

      packages.${system} = {
        docs = pkgs.binplz-docs;
        server = pkgs.binplz-server;
        server-vm = inputs.nixos-generators.nixosGenerate {
          inherit pkgs;
          format = "vm";
          modules = [ config.global config.qemu config.nixbuild ];
        };
        server-ami = inputs.nixos-generators.nixosGenerate {
          inherit pkgs;
          format = "amazon";
          modules = [ config.global config.amazon config.nixbuild ];
        };
        connect-nixbuildnet = pkgs.connect-nixbuildnet;
      };
    };
}
