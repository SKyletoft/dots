{ config, pkgs, inputs, homeConfig, ... }:

let
	stablePkgs = import inputs.stablePkgs {};
	citraPkgs  = import inputs.stablePkgs {};
	vimPin     = import inputs.vimPin {};
	emacsPin   = import inputs.emacsPkgs { overlays = [
		inputs.emacsOverlay.overlays.default
		(_: p: {
			emacs-git = p.emacs-git.overrideAttrs(_: { LSP_USE_PLISTS = true; });
			emacs-igc = p.emacs-igc.overrideAttrs(old: {
				LSP_USE_PLISTS = true;
				preConfigure = ''
					export CC=${pkgs.llvmPackages.clang}/bin/clang
					export CXX=${pkgs.llvmPackages.clang}/bin/clang++
					export AR=${pkgs.llvm}/bin/llvm-ar
					export NM=${pkgs.llvm}/bin/llvm-nm
					export LD=${pkgs.lld}/bin/ld.lld
					export RANLIB=${pkgs.llvm}/bin/llvm-ranlib
				'';
				NIX_CFLAGS_COMPILE = toString (
					["-O2" "-flto=full"]
					++ homeConfig.nativeFlags
					++ old.NIX_CFLAGS_COMPILE or []
				);
			});
			emacsPackages = p.emacsPackages.overrideScope'
				(_: p': { lsp-mode = p'.lsp-mode.overrideAttrs(_: { LSP_USE_PLISTS = true; }); });
		})
	]; };

	enableHyprland  = homeConfig.enableHyprland;
	enableGnome     = homeConfig.enableGnome;
	enableDebugging = homeConfig.enableDebugging;

	gui = enableGnome || wm;
	wm  = enableHyprland;
in {
	home = {
		username      = "u3836";
		homeDirectory = "/home/u3836";
		stateVersion  = "22.05";

		packages = let
			cppfront     = pkgs.callPackage ./packages/cppfront.nix {};
			hylo         = pkgs.callPackage ./packages/hylo.nix {};
			update-lorri = pkgs.writeShellScriptBin "update-lorri" (builtins.readFile scripts/update-lorri);
			hms          = pkgs.writeShellScriptBin "hms" (builtins.readFile scripts/hms);
			doasedit     = pkgs.writeShellScriptBin "doasedit" (builtins.readFile scripts/doasedit);
			monitor      = pkgs.writeShellScriptBin "monitor" (builtins.readFile scripts/monitor);
			mdpdf        = pkgs.writeShellScriptBin "mdpdf" ''
				export PATH=${pkgs.lib.strings.makeBinPath (with pkgs; [ tectonic pandoc ])}
				pandoc $1 -o $2 --pdf-engine=tectonic -s -V papersize:a4 --citeproc
			'';

		in with pkgs; [
			git
			wget
			eza
			fd
			tealdeer
			zoxide
			bat
			viu
			ripgrep
			fzf
			bottom
			htop
			du-dust
			ranger
			neofetch
			trash-cli
			rsync
			xclip
			doasedit
			# mdpdf
			hstr
			bluetui

			nil
			inputs.lsp-booster.packages.${system}.default
			nodePackages.bash-language-server
			shellcheck
			shfmt
			inputs.upwards.packages.${system}.default
			inputs.revpath.packages.${system}.default
			update-lorri
			hms
		] ++

		(if gui then [
			inputs.lem.packages.${system}.default

			# aider-chat-full

			bazecor

			monitor
			iptsd

			vdhcoapp
			google-chrome

			alacritty
			kitty
			vscode
			pinta
			gimp

			(import inputs.curaPkgs {}).cura
			# stablePkgs.blender # Crashes on launch, try steam version?

			vesktop
			# discord
			# dissent
			signal-desktop
			slack
			isync # Emacs email
			mu

			stablePkgs.libreoffice
			stablePkgs.hunspellDicts.sv_SE

			kdePackages.ark
			pcmanfm
			nautilus
			nautilus-open-any-terminal
			vlc
			mpv
			sgt-puzzles
			xournalpp
			prismlauncher
			dolphin-emu-primehack
			# citraPkgs.citra
			# citraPkgs.yuzu
			cemu
			fragments
			heroic
			lutris
			mangohud
			ruffle
			# gamescope
			mission-center
			scanmem # gameconqueror (cheatengine alternative)

			i2c-tools

			scrcpy

			virt-manager
			docker-compose
		] else [])

		++ (if enableGnome then [
			baobab
			totem
			gnome-system-monitor
			gnome-logs
			gnome-text-editor
			gnome-calendar
			gnome-weather
			evince
			# papers
			gnome-disk-utility
			eog
			aisleriot
			iagno
		] else [])

		++ (if enableHyprland then [
			hyprpaper
			hyprsunset
			hypridle
			playerctl
			quickshell
			grim
			wl-clipboard
			imagemagick
			swappy
			swaylock-effects
		] else []);

		sessionVariables = {
			EDITOR = "emacsclient -nw";
			XCURSOR_THEME = homeConfig.cursorTheme or "severa_cursors_linux_expanded";
		} // (if enableHyprland then {
			MOZ_ENABLE_WAYLAND = "1";
		} else {});
	};

	programs = {
		git = {
			enable    = true;
			userName  = "Samuel Kyletoft";
			userEmail = "samuel@kyletoft.se";

			# Reenable whenever I return to vim
			# difftastic.enable = true;
			extraConfig = {
				core.editor    = "emacsclient -r";
				pull.ff        = "only";
				github.user    = "SKyletoft";
				safe.directory = "/etc/nixos/dots";
			};
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

					# "rust-analyzer.checkOnSave.enable"  = true;
					"rust-analyzer.checkOnSave.command" = "clippy";
					# "rust-analyzer.checkOnSave.target" = "#rust-analyzer.cargo.target#";

					"rust-analyzer.hover.documentation.enable" = false;
					"rust-analyzer.hover.links.enable" = true;

					"rust-analyzer.inlayHints.closingBraceHints.enable" = false;
					"rust-analyzer.inlayHints.renderColons" = false;
					"rust-analyzer.inlayHints.reborrowHints.enable" = true;
					"rust-analyzer.inlayHints.typeHints.hideNamedConstructor" = true;

					"java.initializationOptions" = {
						bundles = [
							"${vimPin.vscode-extensions.vscjava.vscode-java-debug}/share/vscode/extensions/vscjava.vscode-java-debug/server/com.microsoft.java.debug.plugin-0.40.0.jar"
						];
					};
				};
			};

			plugins = let
				custom_monokai = vimPin.vimUtils.buildVimPlugin {
					pname = "monokai_vim";
					version = "0.0.1";
					src  = vimPin.fetchFromGitHub {
						owner  = "SKyletoft";
						repo   = "monokai.nvim";
						rev    = "604186067ab1782361d251945c524eb622beb499";
						sha256 = "048blqrnm7rr4a0p0ffahfjzqf62hrcvpza7gmkc5jx2p0ca1k9k";
					};
				};
				vimspector = vimPin.callPackage ./packages/vimspector.nix {};
				treesitter = (vimPin.vimPlugins.nvim-treesitter.withPlugins (_: vimPin.tree-sitter.allGrammars));
			in
			with vimPin.vimPlugins; [
				custom_monokai
				# treesitter
				vim-table-mode
				zen-mode-nvim
				vim-slime
				firenvim
				coc-rust-analyzer
				coc-git
				nvim-dap
				nvim-dap-ui
			];

			# package = vimPin.neovim;
			extraConfig = (builtins.readFile ./neovim_init.vim)
						+ (if enableDebugging then builtins.readFile ./debug.vim else "");
			vimAlias = true;
		};

		emacs = {
			enable = true;
			package = emacsPin.emacs-igc.override {
				withGTK3 = false;
				withX    = gui;
				withWebP = gui;
			};
			extraPackages =
				let
					my-dafny-mode = emacsPin.stdenv.mkDerivation {
						pname = "dafny-mode";
						version = "241001";
						src = emacsPin.fetchFromGitHub {
							owner = "SKyletoft";
							repo = "boogie-friends";
							rev = "ca902b37e0756c4d69c7dd28afe3070c62821807";
							hash = "sha256-1YlOeinld7LSlQC/OwhR0mzKqqB2NriuulUoHh4KDpk=";
						};
						installPhase = ''
							mkdir -p $out/share/emacs/site-lisp
							cp -r * $out/share/emacs/site-lisp/
						'';
					};
					# typst-ts-mode = emacsPin.stdenv.mkDerivation {
					#	pname = "typst-ts-mode";
					#	version = "240317";
					#	src = emacsPin.fetchFromGitHub {
					#		owner = "kaction-emacs";
					#		repo = "typst-ts-mode";
					#		rev = "a5f094dd1d1ce992e9ad290e4838779b2dd5809a";
					#		hash = "sha256-HUOsb5aUj2Kb5E0HaZENp9pqQIAOF4t2SEIKH7cFspo=";
					#	};
					#	installPhase = ''
					#		mkdir -p $out/share/emacs/site-lisp
					#		cp -r * $out/share/emacs/site-lisp/
					#	'';
					# };
				in epkgs: (with emacsPin.emacsPackages; [
					epkgs.treesit-grammars.with-all-grammars
					evil-textobj-tree-sitter

					emacsPin.python313Packages.python
					emacsPin.nodejs
					emacsPin.ispell

					direnv
					magit
					forge
					diff-hl
					evil
					doom-modeline
					ligature
					transpose-frame
					editorconfig
					ripgrep
					yasnippet
					yasnippet-snippets
					mixed-pitch
					company
					company-posframe
					ivy
					ivy-posframe

					gptel

					pdf-tools
					multi-vterm
					vterm

					lsp-mode
					lsp-ui
					lsp-treemacs

					# lsp-java
					nix-ts-mode
					haskell-ts-mode
					idris-mode
					merlin
					tuareg
					ocamlformat
					erlang
					lsp-haskell
					elsa
					flycheck # Todo: Replace flycheck-elsa with lsp when supported
					flycheck-elsa
					racket-mode
					xonsh-mode
					wgsl-mode
					glsl-mode
					bnfc
					futhark-mode
					kotlin-mode
					pest-mode
					typescript-mode
					my-dafny-mode
					ein
					dyalog-mode
					roc-ts-mode
					typst-ts-mode
					swift-ts-mode
					# lsp-sourcekit # Swift LSP support
					qml-mode
				]);
		};

		bash = {
			enable = true;

			shellAliases = {
				cat = "bat --paging=never --tabs=8";
				cd  = "z";
				ls  = "eza -a --icons --colour=always";
				ll  = "eza -la";
				lt  = "eza -a --tree";
				rm  = "trash-put";
				em  = "emacs -nw";
				vi  = "emacsclient -nw";
				restart-emacs = "pkill -9 emacs && emacs --daemon";
				".." = "cd ..";
				chat = "ollama run codellama:13b-instruct";
			};

			shellOptions = [
				"histappend"
				"checkwinsize"
				"globstar"
			];

			initExtra = builtins.readFile ./bashrc;
		};


		direnv = {
			enable = true;
			silent = true;
		};

		zoxide = {
			enable                = true;
			enableBashIntegration = true;
			enableFishIntegration = true;
		};

		firefox = {
			enable = true;
			languagePacks = [ "en-GB" "sv" ];
			# package = pkgs.firefox-devedition-bin;
			profiles.default = {
				id = 0;
				name = "default";
				isDefault = true;
				settings = {
					browser = {
						newtabpage.enabled = false;
						startup.homepage = "chrome://browser/content/blanktab.html";
						toolbars.bookmarks.visibility = "always";
						translations.neverTranslateLanguages = "sv";
						extensions = {
							autoDisableScopes = 0;
							enabledScopes = 15;
							update = {
								autoUpdateDefault = false;
								enabled = false;
							};
						};
					};
					general.autoScroll = true;
				};
				search = {
					force = true;
					default = "ddg";
					order = [ "ddg" "wikipedia" ];
					engines = {
						"wikipedia".metaData.alias = "@wiki";
						"google".metaData.hidden = true;
						"amazondotcom-us".metaData.hidden = true;
						"bing".metaData.hidden = true;
						"ebay".metaData.hidden = true;
					};
				};

				extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
					bitwarden
					ublock-origin
					privacy-badger
					vimium
					sponsorblock
					# enhancer-for-youtube
					consent-o-matic
					enhanced-h264ify
					# refined-hackernews
					# user-agent-switcher-and-manager
					# css-override
					video-downloadhelper
					old-reddit-redirect
					reddit-enhancement-suite
					british-english-dictionary-2
					swedish-dictionary
				];
			};
		};
	};

	services = {
		emacs = {
			enable = false;
			defaultEditor = true;
			startWithUserSession = false;
		};

		lorri = {
			enable = true;
			enableNotifications = true;
		};

		mako = {
			enable = false;
			settings.icons = true;
		};
	};

	# wayland.windowManager.hyprland.enable = hyprland;

	# gtk = {
		# enable         = true;
		# theme.name     = "Yaru";
		# iconTheme.name = "Yaru - Edit";
	# };

	xdg.configFile = {
		"alacritty/alacritty.toml".source = ./alacritty.toml;
		"rustfmt/rustfmt.toml".source     = ./rustfmt.toml;
		"kitty/kitty.conf".source         = ./kitty.conf;
		"hypr/hyprpaper.conf".source      = ./hyprpaper.conf;
		"swaylock/config".source          = ./swaylock;
		# "waybar/config".source            = ./waybar;
		# "niri/config.kdl".source          = ./niri.kdl;
		"fourmolu.yaml".source            = ./fourmolu.yaml;
		# "mpv/mpv.conf".source             = ./mpv.conf; ; It seems like this is what's causing random stuttering on medusa?
		# "emacs".source                    = mkOutOfStoreSymlink "${config.home.homeDirectory}/git/dotfiles/emacs";
		# "hypr/hyprland.conf".source       = mkOutOfStoreSymlink "${config.home.homeDirectory}/git/dotfiles/hyprland.conf";
		# "quickshell".source               = mkOutOfStoreSymlink "${config.home.homeDirectory}/git/dotfiles/quickshell";
	};

	# home.pointerCursor = {
		# name = "Severa_cursors_linux_expanded";
		# size = 32;
	# };
	# dconf.settings.gnome.configuration = {
	#	cursor-theme = config.home.pointerCursor.name;
	#	cursor-size = config.home.pointerCursor.size;
	# };

	home.file = {
		# Files
		".gdbinit".source              = ./gdbinit;
		".nanorc".source               = ./nanorc;
		".clang-format".source         = ./clang-format;
		".compile_commands.txt".source = ./clangd_config;
		".ghci".source                 = ./ghci;
		".xonshrc".source              = ./xonshrc;
		".vimrc".source                = ./vimrc;
		".cargo/config.toml".text      =
''[build]
rustflags = "-Ctarget-cpu=native"

[net]
git-fetch-with-cli = true

[target.x86_64-unknown-linux-gnu]
linker = "${pkgs.clang}/bin/clang"
rustflags = ["-C", "link-arg=-fuse-ld=${pkgs.mold}/bin/mold"]
'';

		# Treesitter grammars
		".config/emacs/tree-sitter/libtree-sitter-roc.so".source =
			"${with emacsPin; inputs.roc-ts.packages.${system}.default}/parser";
		".config/emacs/tree-sitter/libtree-sitter-swift.so".source =
			"${with emacsPin; inputs.swift-ts.defaultPackage.${system}}/parser";

		# Extra desktop files
		".local/share/applications/signal-background.desktop".source = ./signal-background.desktop;
		".local/share/applications/easterislandtd.desktop".source    = ./easterislandtd.desktop;
		".local/share/applications/emacs.desktop".source             = ./emacsclient.desktop;
		".local/share/applications/discord.desktop".source           = ./discord.desktop;
		".local/share/applications/dissent.desktop".source           = ./dissent.desktop;

		# Folders
		# ".themes/yaru".source                         = ./.themes/yaru;
		# ".icons/yaru_edit".source                     = ./.icons/yaru_edit;
		# ".icons/severa_cursors_linux_expanded".source = ./.icons/severa_cursors_linux_expanded;

		# Emacs
		# ".config/emacs".source                    = ./emacs;
		".config/emacs/configs/node-path.el".text = ''
;; -*- lexical-binding: t -*-

(setq copilot-node-executable "${emacsPin.nodejs}/bin/node")
(provide 'node-path)
'';
		".config/emacs/configs/dictionary-path.el".text = ''
;; -*- lexical-binding: t -*-

(setq ispell-program-name "${emacsPin.hunspell}/bin/hunspell")
(setenv "DICPATH" (concat
	"${emacsPin.hunspellDicts.en_GB-ise}/share/hunspell"
	":"
	"${emacsPin.hunspellDicts.sv_SE}/share/hunspell"))
(provide 'dictionary-path)
'';
	};

	# Let Home Manager install and manage itself.
	programs.home-manager.enable = true;
}
