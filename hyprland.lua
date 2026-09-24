-- Medusa
hl.monitor({ output = "DP-1", mode = "2560x1440@144", position = "0x0", scale = 1 })
hl.monitor({ output = "DP-2", mode = "2560x1440@144", position = "2560x0", scale = 1 })

-- Medea
hl.monitor({ output = "HDMI-A-1", mode = "1920x1080@60", position = "0x0", scale = 1 })
hl.monitor({ output = "DP-3",     mode = "1920x1080@60", position = "0x0", scale = 1 })
hl.monitor({ output = "eDP-1",    mode = "3840x2400@60", position = "0x1080", scale = 2 })

hl.config({
	misc = {
		vrr = 3,
		disable_hyprland_logo = true,
		disable_splash_rendering = true,
	},
	input = {
		kb_layout = "se-good",
		kb_variant = "",
		kb_model = "",
		kb_options = "caps:swapescape",
		kb_rules = "",
		numlock_by_default = true,

		follow_mouse = 1,
		sensitivity = 0.275,
		accel_profile = "adaptive",

		repeat_delay = 300,
		repeat_rate = 40,

		touchpad = {
			natural_scroll = true,
			clickfinger_behavior = true,
		},
	},
	general = {
		gaps_in = 5,
		gaps_out = 20,
		border_size = 2,
		-- Note: Same colour but different alphas
		col = {
			active_border = 0xFFFBB86C,
			inactive_border = 0x00FBB86C,
		},
	},
	decoration = {
		rounding = 4,
		shadow = {
			enabled = false,
		},
	},
	animations = {
		enabled = true,
	},
	dwindle = {
		preserve_split = true,
		smart_split = false,
		smart_resizing = false,
	},
	cursor = {
		hide_on_touch = true,
		no_break_fs_vrr = true,
		no_hardware_cursors = false,
	},
	xwayland = {
		force_zero_scaling = true,
	},
})

hl.animation({
	leaf = "workspaces",
	enabled = true,
	speed = 6,
	bezier = "default",
})

hl.gesture({
	fingers = 3,
	direction = "horizontal",
	action = "workspace",
})
hl.config({
	gestures = {
		workspace_swipe_direction_lock = false,
		workspace_swipe_forever = false,
		workspace_swipe_use_r = true,
		workspace_swipe_distance = 600,
	},
})

-- Autostart
hl.on("hyprland.start", function()
	hl.exec_cmd("quickshell")
	hl.exec_cmd("hyprpaper")
	hl.exec_cmd("systemctl --user start hyprpolkitagent")
	hl.exec_cmd("bash -c 'while true; do xwayland-satellite :10; notify-send \"Xwayland crashed, restarting\"; sleep 2; done'")
	hl.exec_cmd("emacs --daemon")
	hl.exec_cmd('signal-desktop --start-in-tray --password-store="gnome-libsecret"')
	hl.exec_cmd("vesktop")
end)

-- Environment
hl.env("DISPLAY", ":10")
-- hl.env("XCURSOR_THEME", "severa_cursors_linux_expanded")
hl.env("XCURSOR_SIZE", "32")

-- Smart gaps
hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "f[1]", gaps_out = 0, gaps_in = 0 })
hl.window_rule({
	match = { float = false, workspace = "w[tv1]" },
	border_size = 0,
	rounding = 0,
})
hl.window_rule({
	match = { float = false, workspace = "f[1]" },
	border_size = 0,
	rounding = 0,
})

-- Toggle overview/launcher
hl.bind("SUPER + Space", hl.dsp.global("quickshell:overviewToggleRelease"))
hl.bind("SUPER + Tab", hl.dsp.global("quickshell:overviewToggleRelease"))

hl.bind("SUPER + T",         hl.dsp.exec_cmd("ghostty"))
hl.bind("SUPER + Q",         hl.dsp.window.close())
hl.bind("SUPER + O",         hl.dsp.exit())
hl.bind("SUPER + SHIFT + O", hl.dsp.exec_cmd("pkill -9 quickshell; quickshell"))
hl.bind("SUPER + E",         hl.dsp.exec_cmd("nautilus"))
hl.bind("SUPER + D",         hl.dsp.exec_cmd("hyprctl dispatch focuswindow class:vesktop ; vesktop"))
hl.bind("SUPER + SHIFT + D", hl.dsp.exec_cmd("pkill -9 vesktop; vesktop"))
hl.bind("SUPER + G",         hl.dsp.window.float({ action = "toggle" }))
hl.bind("SUPER + R",         hl.dsp.exec_cmd('emacsclient -a nvim --create-frame -e "(about-emacs)"'))
hl.bind("SUPER + SHIFT + R", hl.dsp.exec_cmd("emacs --debug-init"))
hl.bind("SUPER + F",         hl.dsp.exec_cmd("firefox"))
hl.bind("SUPER + SHIFT + P", hl.dsp.exec_cmd("firefox --private-window"))
hl.bind("SUPER + SHIFT + S", hl.dsp.exec_cmd("grimshot copy area"))

hl.bind("SUPER + F11", hl.dsp.window.fullscreen({ mode = "fullscreen" }))
hl.bind("SUPER + M",   hl.dsp.window.fullscreen_state({ internal = 2, client = 2 }))

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+ -l 1.0"))
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1- -l 1.0"))
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
hl.bind("XF86AudioMicMute",     hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"))
hl.bind("XF86AudioPlay",        hl.dsp.exec_cmd("playerctl play-pause"))
hl.bind("XF86AudioNext",        hl.dsp.exec_cmd("playerctl next"))
hl.bind("XF86AudioPrev",        hl.dsp.exec_cmd("playerctl previous"))

hl.bind("SUPER + left",  hl.dsp.focus({ direction = "left" }))
hl.bind("SUPER + right", hl.dsp.focus({ direction = "right" }))
hl.bind("SUPER + up",    hl.dsp.focus({ direction = "up" }))
hl.bind("SUPER + down",  hl.dsp.focus({ direction = "down" }))

hl.bind("SUPER + mouse:272",     hl.dsp.window.drag(), { mouse = true })
hl.bind("SUPER + mouse:273",     hl.dsp.window.resize(), { mouse = true })
hl.bind("SUPER + SHIFT + left",  hl.dsp.window.move({ direction = "left" }))
hl.bind("SUPER + SHIFT + right", hl.dsp.window.move({ direction = "right" }))
hl.bind("SUPER + SHIFT + up",    hl.dsp.window.move({ direction = "up" }))
hl.bind("SUPER + SHIFT + down",  hl.dsp.window.move({ direction = "down" }))

for i = 1, 10 do
	local key = i % 10
	hl.bind("SUPER + " .. key, hl.dsp.focus({ workspace = i }))
end

for i = 1, 10 do
	local key = i % 10
	hl.bind("ALT + " .. key, hl.dsp.window.move({ workspace = i }))
end

hl.bind("switch:Lid Switch", hl.dsp.exec_cmd("swaylock"), { locked = true })

-- Monitor layouts
hl.bind("SUPER + L",         hl.dsp.exec_cmd("/home/u3836/git/dots/scripts/monitor left"))
hl.bind("SUPER + U",         hl.dsp.exec_cmd("/home/u3836/git/dots/scripts/monitor default"))
hl.bind("SUPER + Y",         hl.dsp.exec_cmd("/home/u3836/git/dots/scripts/monitor right"))
hl.bind("SUPER + SHIFT + L", hl.dsp.exec_cmd("/home/u3836/git/dots/scripts/monitor left portrait"))
