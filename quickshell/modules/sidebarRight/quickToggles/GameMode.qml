import qs.modules.common
import qs.modules.common.widgets
import qs
import Quickshell
import Quickshell.Io

QuickToggleButton {
	id: root
	buttonIcon: "gamepad"
	toggled: toggled

	onClicked: {
		root.toggled = !root.toggled
		if (root.toggled) {
			Quickshell.execDetached(["hyprctl", "eval", "hl.config({animations={enabled=false}, decoration={shadow={enabled=false}, blur={enabled=false}, rounding=0}, general={gaps_in=0, gaps_out=0, border_size=1, allow_tearing=true}})"])
		} else {
			Quickshell.execDetached(["hyprctl", "reload"])
		}
	}
	Process {
		id: fetchActiveState
		running: false
		command: ["hyprctl", "getoption", "animations:enabled", "-j"]
		stdout: StdioCollector {
			id: gameModeStateCollector
			onStreamFinished: {
				try {
					const opt = JSON.parse(gameModeStateCollector.text);
					root.toggled = !(opt.bool ?? true); // Inverted because enabled = game mode off
				} catch (e) {
				}
			}
		}
	}
	StyledToolTip {
		content: Translation.tr("Game mode")
	}
}
