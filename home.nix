{ config, pkgs, ... }:

let
	master = import (builtins.fetchGit {
		url = "https://github.com/nixos/nixpkgs";
		rev = "35619ce1e513fa044b341f62a633deaa458eed06";
	}) {};
	pinned = import (builtins.fetchGit {
		url = "https://github.com/nixos/nixpkgs";
		rev = "eb5409461a41f5e3d78997d870f38a6329bb8044";
	}) {};
	gui = true;
in {
	home = {
		# Home Manager needs a bit of information about you and the
		# paths it should manage.
		username      = "u3836";
		homeDirectory = "/home/u3836";

		# This value determines the Home Manager release that your
		# configuration is compatible with. This helps avoid breakage
		# when a new Home Manager release introduces backwards
		# incompatible changes.
		#
		# You can update Home Manager without changing this value. See
		# the Home Manager release notes for a list of state version
		# changes in each release.
		stateVersion = "22.05";

		packages = let
			eterm8    = pkgs.callPackage ./packages/eterm8.nix {};
			digiflisp = pkgs.callPackage ./packages/digiflisp.nix {};
			doasedit  = pkgs.writeShellScriptBin "doasedit" (builtins.readFile scripts/doasedit);
			monitor   = pkgs.writeShellScriptBin "monitor" (builtins.readFile scripts/monitor);
		in with pkgs; [
			git
			wget
			exa
			fd
			tealdeer
			zoxide
			bat
			ripgrep
			bottom
			du-dust
			ranger
			neofetch
			trash-cli
			rsync
			xclip
			direnv
			doasedit
		] ++
		(if gui then [

			eterm8
			digiflisp
			monitor

			firefox-bin
			alacritty
			vscode
			pinta
			cura

			discord
			signal-desktop

			libreoffice
			ark
			pcmanfm
			vlc
			sgtpuzzles
			xournalpp
			dolphin-emu-primehack
			pinta
			mullvad-vpn
			fragments
			heroic

			virt-manager
			docker-compose

			gnome.gnome-tweaks
			gnome.baobab
			gnome.gnome-system-monitor
			gnome.gnome-logs
			gnome-text-editor
			gnome.gnome-calendar
			gnome.gnome-weather
			evince
			gnome.gnome-disk-utility
			gnome.eog
			gnome.aisleriot
			gnome.iagno

			gnomeExtensions.unite
			gnomeExtensions.just-perfection
			gnomeExtensions.add-username-to-top-panel
			gnomeExtensions.blur-my-shell
			gnomeExtensions.appindicator
			gnomeExtensions.hot-edge
			gnomeExtensions.caffeine
			gnomeExtensions.sound-output-device-chooser
			gnomeExtensions.fuzzy-app-search
			gnomeExtensions.pop-shell
			gnomeExtensions.burn-my-windows
		] else []);

		sessionVariables = {
			MOZ_ENABLE_WAYLAND = 1;
			EDITOR = "nvim";
			DIRENV_LOG_FORMAT = "";
		};
	};

	programs = {
		git = {
			enable    = true;
			userName  = "Samuel Kyletoft";
			userEmail = "samuel@kyletoft.se";
		};

		neovim = {
			enable     = true;
			withNodeJs = true;
			coc        = {
				enable   = true;
				settings = {
					languageserver =  {
						haskell = {
							args         = [ "--lsp" ];
							command      = "haskell-language-server";
							filetypes    = [ "haskell" "lhaskell" ];
							rootPatterns = [
								"*.cabal"
								"stack.yaml"
								"cabal.project"
								"package.yaml"
								"hie.yaml"
							];
						};
						clangd = {
							command   = "clangd";
							filetypes = [ "c" "cpp" "cc" "h" "hpp" ];
							args      = [
								"--background-index"
								"--clang-tidy"
								"--header-insertion=iwyu"
								"--header-insertion-decorators"
							];
						};
					};
				};
			};

			plugins = with pinned.vimPlugins; 
			let
				custom_monokai = pkgs.vimUtils.buildVimPlugin {
					name = "monokai_vim";
					src  = pkgs.fetchFromGitHub {
						owner  = "SKyletoft";
						repo   = "monokai.nvim";
						rev    = "604186067ab1782361d251945c524eb622beb499";
						sha256 = "048blqrnm7rr4a0p0ffahfjzqf62hrcvpza7gmkc5jx2p0ca1k9k";
					};
				};
				vimspector = pkgs.callPackage ./packages/vimspector.nix {};
			in
			[
				custom_monokai
				nvim-treesitter
				vim-table-mode
				zen-mode-nvim
				vim-slime
				coc-rust-analyzer
				coc-git
				copilot-vim
				vimspector
			];

			extraPackages = with pkgs; [ rust-analyzer haskell-language-server clang-tools_14 ];
			extraConfig   = builtins.readFile ./neovim_init.vim;
			viAlias       = true;
			vimdiffAlias  = true;
		};

		bash = {
			enable = true;

			shellAliases = {
				cat = "bat --paging=never --tabs=8";
				cd  = "z";
				ls  = "exa -a";
				ll  = "exa -la";
				lt  = "exa -a --tree";
				rm  = "trash-put";

				hackeholken = "ssh 3836@dtek.se -p222";
				eurydice    = "ssh kyletoft.se -p1234";
				hades       = "ssh kyletoft.se -p1235";
			};

			shellOptions = [
				"histappend"
				"checkwinsize"
				"globstar"
			];

			initExtra = builtins.readFile ./bashrc;
		};

		zoxide = {
			enable                = true;
			enableBashIntegration = true;
		};
	};

	services.lorri.enable = true;

	# gtk = {
		# enable         = true;
		# theme.name     = "Yaru";
		# iconTheme.name = "Yaru - Edit";
	# };

	xdg.configFile = {
		"alacritty/alacritty.yml".source = ./alacritty.yml;
		"rustfmt/rustfmt.toml".source    = ./rustfmt.toml;
		"kitty/kitty.conf".source        = ./kitty.conf;
	};

	home.file = {
		# Files
		".gdbinit".source           = ./gdbinit;
		".nanorc".source            = ./nanorc;
		".clang-format".source      = ./clang-format;
		".cargo/config.toml".source = ./cargo_config;
		".ghci".source              = ./ghci;
		".xonshrc".source           = ./xonshrc;

		# Folders
		".themes/yaru".source                         = ./.themes/yaru;
		".icons/yaru_edit".source                     = ./.icons/yaru_edit;
		".icons/severa_cursors_linux_expanded".source = ./.icons/severa_cursors_linux_expanded;
		"bin".source                                  = ./scripts;
	};

	# Let Home Manager install and manage itself.
	programs.home-manager.enable = true;
}

