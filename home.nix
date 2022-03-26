{ config, pkgs, ... }:

let
	master = import (builtins.fetchGit {
		url = "https://github.com/nixos/nixpkgs";
	}) {};
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
			protonup
			neofetch
			trash-cli
			rsync

			eterm8
			digiflisp

			firefox-bin
			alacritty
			vscode

			discord
			signal-desktop

			libreoffice
			ark
			pcmanfm
			vlc
			sgtpuzzles
			xournalpp

			virt-manager

			gnome.gnome-tweaks
			gnome.baobab
			gnome.gnome-system-monitor
			gnome.gnome-logs
			gnome-text-editor
			gnome.gnome-calendar
			gnome.gnome-weather
			evince
			gnome.gnome-disk-utility
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
			gnomeExtensions.vertical-overview
			gnomeExtensions.fuzzy-app-search
			gnomeExtensions.fuzzy-clock
			gnomeExtensions.duckduckgo-search-provider
			gnomeExtensions.pop-shell
			gnomeExtensions.burn-my-windows
		];

		sessionVariables = {
			MOZ_ENABLE_WAYLAND = 1;
			EDITOR = "nvim";
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
							args      = [ "--background-index" ];
						};
					};
				};

			};

			plugins = with pkgs.vimPlugins; 
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
			in
			[
				custom_monokai
				nvim-treesitter
				coc-nvim
				coc-rust-analyzer
				coc-git
				vim-sneak
				vim-repeat
			];

			extraPackages = with pkgs; [ rust-analyzer haskell-language-server clang-tools ];
			extraConfig   = builtins.readFile ./neovim_init.vim;
			viAlias       = true;
			vimAlias      = true;
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
				pi          = "ssh u3836@81.230.171.125 -p1234";
			};

			shellOptions = [
				"histappend"
				"checkwinsize"
				"globstar"
			];

			# Different PS1s for a plain tty, alacritty and the rest
			# Can't figure out where `complete` and `bind` are supposed to go
			# when done properly
			# sessionVariables for EDITOR doesn't seem to work?
			initExtra = ''
				complete -cf doas
				bind "set completion-ignore-case on"

				if [ "$TERM" == linux ]; then
					PS1='\[\033[01;32m\]\u \[\033[01;34m\]\w\[\033[00m\] \$ '
				elif [ "$ALACRITTY" == yes ]; then
					PS1='\e[1;97;42;24m \u \e[21;32;44;24m\e[1;97;44;24m \h \e[21;34;41;24m\e[1;97;41m \w \001\e[21;31;49;24m\002\n\001\e[97;1m\002↳\001\e[0m\002 '
				else
					PS1='\e[32;1m\u: \e[34m\w \[\033[00m\]\n↳ '
				fi
			'';
		};

		zoxide = {
			enable                = true;
			enableBashIntegration = true;
		};
	};

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

		# Folders
		".themes/yaru".source                         = ./.themes/yaru;
		".icons/yaru_edit".source                     = ./.icons/yaru_edit;
		".icons/severa_cursors_linux_expanded".source = ./.icons/severa_cursors_linux_expanded;
		"bin".source                                  = ./scripts;
	};
	# Let Home Manager install and manage itself.
	programs.home-manager.enable = true;
}

