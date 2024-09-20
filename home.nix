{ config, pkgs, inputs, ... }:

let
	stablePkgs = import inputs.stablePkgs {};
	citraPkgs  = import inputs.stablePkgs {};
	vimPin     = import inputs.vimPin {};
	emacsPin   = import inputs.emacsPkgs { overlays = [
		inputs.emacsOverlay.overlays.default
		(_: p: {
			emacs-git = p.emacs-git.overrideAttrs(_: { LSP_USE_PLISTS = true; });
			emacsPackages = p.emacsPackages.overrideScope'
				(_: p': { lsp-mode = p'.lsp-mode.overrideAttrs(_: { LSP_USE_PLISTS = true; }); });
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
			# ruffle    = pkgs.callPackage ./packages/ruffle.nix {};
			eterm8    = pkgs.callPackage ./packages/eterm8.nix {};
			digiflisp = pkgs.callPackage ./packages/digiflisp.nix {};
			cppfront  = pkgs.callPackage ./packages/cppfront.nix {};
			hylo      = pkgs.callPackage ./packages/hylo.nix {};
			lsp-booster = emacsPin.callPackage ./packages/lsp-booster.nix {};
			doasedit  = pkgs.writeShellScriptBin "doasedit" (builtins.readFile scripts/doasedit);
			monitor   = pkgs.writeShellScriptBin "monitor" (builtins.readFile scripts/monitor);
			mdpdf     = pkgs.writeShellScriptBin "mdpdf" ''
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
			mdpdf
			hstr

			nil
			lsp-booster
			nodePackages.bash-language-server
			shellcheck
			shfmt
			inputs.upwards.packages.${system}.default
		] ++
		(if gui then [
			inputs.lem.packages.${system}.default

			monitor
			iptsd

			firefox-devedition-bin
			vdhcoapp

			alacritty
			kitty
			vscode
			pinta
			gimp
			stablePkgs.cura
			blender

			discord
			dissent
			signal-desktop
			slack

			stablePkgs.libreoffice
			stablePkgs.hunspellDicts.sv_SE

			ark
			pcmanfm
			vlc
			mpv
			sgt-puzzles
			xournalpp
			prismlauncher
			dolphin-emu-primehack
			citraPkgs.citra
			citraPkgs.yuzu
			cemu
			fragments
			heroic
			lutris
			mangohud
			ruffle
			gamescope
			mission-center

			i2c-tools

			scrcpy

			virt-manager
			docker-compose
		] else [])
		++ (if enableGnome then [
			baobab
			totem
			gnome-system-monitor
			gnome.gnome-logs
			gnome-text-editor
			gnome-calendar
			gnome.gnome-weather
			evince
			gnome-disk-utility
			eog
			gnome.aisleriot
			gnome.iagno
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
				pull.ff     = "only";
				github.user = "SKyletoft";
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
			package = emacsPin.emacs-git.override {
				withGTK2       = false;
				withGTK3       = false;
				withX          = gui;
				withWebP       = gui;
			};
			extraPackages = epkgs: (with emacsPin.emacsPackages; [
				emacsPin.python313Packages.python
				emacsPin.nodejs

				direnv
				magit
				forge
				diff-hl
				evil
				treemacs
				telephone-line
				ligature
				gcmh
				transpose-frame
				editorconfig
				ripgrep
				yasnippet
				yasnippet-snippets
				mixed-pitch
				company-posframe
				ivy
				ivy-posframe
				projectile
				casual-calc

				pdf-tools
				multi-vterm
				vterm

				lsp-mode
				lsp-ui
				dap-mode

				realgud
				realgud-jdb

				# tree-sitter-langs
				evil-textobj-tree-sitter

				company

				lsp-java
				nix-ts-mode
				haskell-mode
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
				boogie-friends # dafny-mode
				ada-mode
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
				".." = "cd ..";
				update-lorri = "nix develop -L < /dev/null && nix-shell < /dev/null && lorri init && direnv allow && lorri watch";
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

	services.emacs = {
		enable = false;
		defaultEditor = true;
		startWithUserSession = false;
	};
	services.lorri = {
		enable = true;
		enableNotifications = true;
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
		".emacs.d/tree-sitter/libtree-sitter-kotlin.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-kotlin}/parser";
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
		".emacs.d/tree-sitter/libtree-sitter-json.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-json}/parser";
		".emacs.d/tree-sitter/libtree-sitter-jsdoc.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-jsdoc}/parser";
		".emacs.d/tree-sitter/libtree-sitter-javascript.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-javascript}/parser";
		".emacs.d/tree-sitter/libtree-sitter-typescript.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-typescript}/parser";
		".emacs.d/tree-sitter/libtree-sitter-tsx.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-tsx}/parser";
		".emacs.d/tree-sitter/libtree-sitter-c-sharp.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-c-sharp}/parser";
		".emacs.d/tree-sitter/libtree-sitter-dockerfile.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-dockerfile}/parser";
		".emacs.d/tree-sitter/libtree-sitter-nix.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-nix}/parser";
		".emacs.d/tree-sitter/libtree-sitter-html.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-html}/parser";
		".emacs.d/tree-sitter/libtree-sitter-css.so".source =
			"${emacsPin.tree-sitter-grammars.tree-sitter-css}/parser";

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
