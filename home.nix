{ config, pkgs, inputs, ... }:

let
	stable = import (builtins.fetchGit {
		url = "https://github.com/nixos/nixpkgs";
		ref = "nixos-22.05";
	}) {};

	vimPin = import (builtins.fetchGit {
		url = "https://github.com/nixos/nixpkgs";
		ref = "master";
		rev = "9813adc7f7c0edd738c6bdd8431439688bb0cb3d";
	}) {};

	emacsOverlayPin = import (builtins.fetchGit {
		url = "https://github.com/nix-community/emacs-overlay.git";
		ref = "master";
		rev = "791acfa700b9f96c35635fde2a17a66b4ed88c9e"; # change the revision
	});
	emacsPin = import (builtins.fetchGit {
		url = "https://github.com/nixos/nixpkgs";
		ref = "nixos-23.05";
		rev = "f3fbbc36b4e179a5985b9ab12624e9dfe7989341";
	}) { overlays = [
		emacsOverlayPin
		(final: prev: {
			tree-sitter-grammars = prev.tree-sitter-grammars // {
				tree-sitter-cpp = prev.tree-sitter-grammars.tree-sitter-cpp.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-c = prev.tree-sitter-grammars.tree-sitter-c.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-java = prev.tree-sitter-grammars.tree-sitter-java.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-rust = prev.tree-sitter-grammars.tree-sitter-rust.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-haskell = prev.tree-sitter-grammars.tree-sitter-haskell.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-python = prev.tree-sitter-grammars.tree-sitter-python.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-elisp = prev.tree-sitter-grammars.tree-sitter-elisp.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-make = prev.tree-sitter-grammars.tree-sitter-make.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-latex = prev.tree-sitter-grammars.tree-sitter-latex.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-javascript = prev.tree-sitter-grammars.tree-sitter-javascript.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
				tree-sitter-bash = prev.tree-sitter-grammars.tree-sitter-bash.overrideAttrs (_: {
					nativeBuildInputs = [ final.nodejs final.tree-sitter ];
					configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
				});
			};
		})
	]; };

	enableHyprland = false;
	enableGnome = true;
	enableDebugging = false;
	gui = enableGnome || enableHyprland;
in {
	home = {
		username      = "u3836";
		homeDirectory = "/home/u3836";
		stateVersion  = "22.05";

		packages = let
			eterm8    = pkgs.callPackage ./packages/eterm8.nix {};
			digiflisp = pkgs.callPackage ./packages/digiflisp.nix {};
			doasedit  = pkgs.writeShellScriptBin "doasedit" (builtins.readFile scripts/doasedit);
			monitor   = pkgs.writeShellScriptBin "monitor" (builtins.readFile scripts/monitor);
			mdpdf     = pkgs.writeShellScript "mdpdf" ''
				export PATH=${pkgs.lib.strings.makeBinPath (with pkgs; [ tectonic pandoc ])}
				pandoc $1 -o $2 --pdf-engine=tectonic -s -V papersize:a4
			'';
		in with pkgs; [
			git
			wget
			exa
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
			direnv
			doasedit
			# code-server
			# mdpdf
			hstr
		] ++
		(if gui then [
			monitor
			iptsd

			firefox-devedition-bin
			alacritty
			vscode
			pinta
			cura
			blender

			discord
			signal-desktop
			slack

			stable.libreoffice
			stable.hunspellDicts.sv_SE

			ark
			pcmanfm
			vlc
			sgtpuzzles
			xournalpp
			prismlauncher
			dolphin-emu-primehack
			pinta
			fragments
			heroic

			i2c-tools

			scrcpy

			virt-manager
			docker-compose
		] else [])
		++ (if enableGnome then [
			gnome.gnome-tweaks
			gnome.baobab
			gnome.totem
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

			gnomeExtensions.brightness-control-using-ddcutil
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
			gnomeExtensions.dotspaces
			gnomeExtensions.hibernate-status-button
		] else [])
		++ (if enableHyprland then [
			hyprland
			hyprpaper
			wofi
			waybar
		] else []);

		sessionVariables = {
			EDITOR = "emacsclient -nw";
			DIRENV_LOG_FORMAT = "";
		} // (if enableHyprland then {
			MOZ_ENABLE_WAYLAND = "1";
		} else {});
	};

	programs = {
		git = {
			enable    = true;
			userName  = "Samuel Kyletoft";
			userEmail = "samuel@kyletoft.se";

			difftastic.enable = true;
			extraConfig = {
				core.editor = "emacsclient -r";
				pull = {
					ff = "only";
				};
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
				treesitter
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
			package = emacsPin.emacs-git.override {
				withGTK2       = false;
				withGTK3       = false;
				withX          = gui;
				withWebP       = gui;
			};
			extraPackages = epkgs: (with emacsPin.emacsPackages; [
				emacsPin.python311Packages.python
				nodejs
				
				direnv
				use-package
				magit
				evil
				treemacs
				telephone-line
				ligature
				dashboard
				gcmh
				crdt
				transpose-frame
				# tramp
				editorconfig
				ripgrep
				yasnippet
				yasnippet-snippets
				mixed-pitch
				company-posframe

				org-appear
				org-superstar
				olivetti

				lsp-mode
				lsp-ui
				dap-mode

				# tree-sitter
				tree-sitter-langs

				flycheck
				eldoc-box
				company

				nix-mode
				haskell-mode
				# idris2-mode
				idris-mode
				merlin
				tuareg
				ocamlformat
				erlang
				lsp-haskell
				rustic
				elsa
				flycheck-elsa
				racket-mode
				pdf-tools
				multi-vterm
				vterm
			]);
		};

		bash = {
			enable = true;

			shellAliases = {
				cat = "bat --paging=never --tabs=8";
				cd  = "z";
				ls  = "exa -a --icons --colour=always";
				ll  = "exa -la";
				lt  = "exa -a --tree";
				rm  = "trash-put";
				em  = "emacs -nw";
				vi  = "emacsclient -nw";
				hms = "home-manager switch --impure";
				restart-emacs = "pkill emacs && emacs --daemon";
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
	services.emacs = {
		enable = false;
		defaultEditor = true;
		startWithUserSession = false;
	};

	# wayland.windowManager.hyprland.enable = hyprland;

	# gtk = {
		# enable         = true;
		# theme.name     = "Yaru";
		# iconTheme.name = "Yaru - Edit";
	# };

	xdg.configFile = {
		"alacritty/alacritty.yml".source  = ./alacritty.yml;
		"rustfmt/rustfmt.toml".source     = ./rustfmt.toml;
		"kitty/kitty.conf".source         = ./kitty.conf;
		"hypr/hyprland.conf".source       = ./hyprland.conf;
		"hypr/hyprpaper.conf".source      = ./hyprpaper.conf;
		"waybar/config".source            = ./waybar;
	};

	home.file = {
		# Files
		".gdbinit".source           = ./gdbinit;
		".nanorc".source            = ./nanorc;
		".clang-format".source      = ./clang-format;
		".ghci".source              = ./ghci;
		".xonshrc".source           = ./xonshrc;
		".vimrc".source             = ./vimrc;
		".cargo/config.toml".text   =
''[build]
rustflags = "-Ctarget-cpu=native"

[net]
git-fetch-with-cli = true

[target.x86_64-unknown-linux-gnu]
linker = "${pkgs.clang_16}/bin/clang"
rustflags = ["-C", "link-arg=-fuse-ld=${pkgs.mold}/bin/mold"]
'';

		# Treesitter grammars
		".emacs.d/tree-sitter/libtree-sitter-cpp.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-cpp}/parser";
		".emacs.d/tree-sitter/libtree-sitter-c.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-c}/parser";
		".emacs.d/tree-sitter/libtree-sitter-java.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-java}/parser";
		".emacs.d/tree-sitter/libtree-sitter-rust.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-rust}/parser";
		".emacs.d/tree-sitter/libtree-sitter-haskell.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-haskell}/parser";
		".emacs.d/tree-sitter/libtree-sitter-python.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-python}/parser";
		".emacs.d/tree-sitter/libtree-sitter-elisp.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-elisp}/parser";
		".emacs.d/tree-sitter/libtree-sitter-make.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-make}/parser";
		".emacs.d/tree-sitter/libtree-sitter-bash.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-bash}/parser";
		".emacs.d/tree-sitter/libtree-sitter-latex.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-latex}/parser";
		".emacs.d/tree-sitter/libtree-sitter-javascript.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-javascript}/parser";

		# Extra desktop files
		".local/share/applications/signal-background.desktop".source = ./signal-background.desktop;
		".local/share/applications/emacs.desktop".source             = ./emacsclient.desktop;
		".local/share/applications/discord.desktop".source           = ./discord.desktop;

		# Folders
		# ".themes/yaru".source                         = ./.themes/yaru;
		# ".icons/yaru_edit".source                     = ./.icons/yaru_edit;
		# ".icons/severa_cursors_linux_expanded".source = ./.icons/severa_cursors_linux_expanded;

		# Emacs
		# ".emacs.d/configs".source       = ./emacs.d/configs;
		# ".emacs.d/custom-themes".source = ./emacs.d/custom-themes;
		# ".emacs".source                 = ./emacs;
	};

	# Let Home Manager install and manage itself.
	programs.home-manager.enable = true;
}
