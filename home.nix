{ config, pkgs, inputs, ... }:

let
	stablePkgs = import inputs.stablePkgs {};
	vimPin     = import inputs.vimPin {};
	emacsPin   = import inputs.emacsPkgs { overlays = [
		inputs.emacsOverlay.overlays.default
		(final: prev: {
			tree-sitter-grammars =
				let useAbi13 = lang: {
						name = "tree-sitter-${lang}";
						value = prev.tree-sitter-grammars."tree-sitter-${lang}".overrideAttrs (_: {
							nativeBuildInputs = [ final.nodejs final.tree-sitter ];
							configurePhase = "tree-sitter generate --abi 13 src/grammar.json";
						});
					};
				in prev.tree-sitter-grammars // (builtins.listToAttrs (builtins.map useAbi13 [
					"cpp"
					"c"
					"cmake"
					"java"
					# "kotlin"
					"rust"
					"haskell"
					"python"
					"elisp"
					"make"
					"latex"
					"javascript"
					"html"
					"css"
					"bash"
					"nix"
					"hlsl"
					"glsl"
				]));
		})
	]; };

	enableHyprland = false;
	enableGnome = true;
	enableRiver = false;
	enableSway = false;
	enableDebugging = false;
	gui = enableGnome || enableHyprland || enableRiver || enableSway;
	wm = enableHyprland || enableRiver || enableSway;
in {
	home = {
		username      = "u3836";
		homeDirectory = "/home/u3836";
		stateVersion  = "22.05";

		packages = let
			ruffle    = pkgs.callPackage ./packages/ruffle.nix {};
			eterm8    = pkgs.callPackage ./packages/eterm8.nix {};
			digiflisp = pkgs.callPackage ./packages/digiflisp.nix {};
			cppfront  = pkgs.callPackage ./packages/cppfront.nix {};
			hylo      = pkgs.callPackage ./packages/hylo.nix {};
			lsp-booster = emacsPin.callPackage ./packages/lsp-booster.nix {};
			doasedit  = pkgs.writeShellScriptBin "doasedit" (builtins.readFile scripts/doasedit);
			monitor   = pkgs.writeShellScriptBin "monitor" (builtins.readFile scripts/monitor);
			mdpdf     = pkgs.writeShellScript "mdpdf" ''
				export PATH=${pkgs.lib.strings.makeBinPath (with pkgs; [ tectonic pandoc ])}
				pandoc $1 -o $2 --pdf-engine=tectonic -s -V papersize:a4
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
			direnv
			doasedit
			# code-server
			# mdpdf
			hstr

			rnix-lsp
			lsp-booster
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

			stablePkgs.libreoffice
			stablePkgs.hunspellDicts.sv_SE

			ark
			pcmanfm
			vlc
			sgt-puzzles
			xournalpp
			prismlauncher
			dolphin-emu-primehack
			pinta
			fragments
			# heroic
			ruffle

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
		] else [])
		++ (if enableRiver then [
			river

			rivercarro
			kile-wl
			stacktile

			wofi

			ristate
			swaylock
			waybar
			swaybg
			swayosd
			swayidle
			swaynag-battery
			wayshot
			wlr-randr
			xonsh
		] else [])
		++ (if enableSway then [
			sway
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

			# Reenable whenever I return to vim
			# difftastic.enable = true;
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
			package = emacsPin.emacs29.override {
				withGTK2       = false;
				withGTK3       = false;
				withX          = gui;
				withWebP       = gui;
			};
			extraPackages = epkgs: (with emacsPin.emacsPackages; [
				emacsPin.python311Packages.python
				emacsPin.nodejs

				direnv
				use-package
				magit
				diff-hl
				evil
				symex
				treemacs
				telephone-line
				ligature
				dashboard
				gcmh
				crdt
				transpose-frame
				tramp
				editorconfig
				ripgrep
				yasnippet
				yasnippet-snippets
				mixed-pitch
				company-posframe
				topsy
				ivy
				projectile
				fzf

				pdf-tools
				multi-vterm
				vterm

				org-appear
				org-superstar
				olivetti

				lsp-mode
				lsp-ui
				dap-mode
				lsp-java

				realgud
				realgud-jdb

				# tree-sitter
				tree-sitter-langs
				evil-textobj-tree-sitter
				paredit

				flycheck
				eldoc-box
				company
				which-key

				nix-mode
				haskell-mode
				idris-mode
				agda2-mode
				merlin
				tuareg
				ocamlformat
				erlang
				lsp-haskell
				rustic
				elsa
				flycheck-elsa
				racket-mode
				xonsh-mode
				wgsl-mode
				glsl-mode
				bnfc
				futhark-mode
				kotlin-mode
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
				# hms = "home-manager switch --impure";
				hms = "echo 'cd ~/dots/home && home-manager switch --flake . --impure -L' | bash";
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
linker = "${pkgs.clang_16}/bin/clang"
rustflags = ["-C", "link-arg=-fuse-ld=${pkgs.mold}/bin/mold"]
'';

		# Treesitter grammars
		".emacs.d/tree-sitter/libtree-sitter-cpp.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-cpp}/parser";
		".emacs.d/tree-sitter/libtree-sitter-c.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-c}/parser";
		".emacs.d/tree-sitter/libtree-sitter-cmake.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-cmake}/parser";
		".emacs.d/tree-sitter/libtree-sitter-java.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-java}/parser";
		# ".emacs.d/tree-sitter/libtree-sitter-kotlin.so".source =
			# "${emacsPin.tree-sitter-grammars.tree-sitter-kotlin}/parser";
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
		".emacs.d/tree-sitter/libtree-sitter-c-sharp.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-c-sharp}/parser";

		# Extra desktop files
		".local/share/applications/signal-background.desktop".source = ./signal-background.desktop;
		".local/share/applications/easterislandtd.desktop".source    = ./easterislandtd.desktop;
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
		".emacs.d/configs/node-path.el".text =
''(provide 'node-path)
(setq copilot-node-executable "${emacsPin.nodejs}/bin/node")
'';
	};

	# Let Home Manager install and manage itself.
	programs.home-manager.enable = true;
}
