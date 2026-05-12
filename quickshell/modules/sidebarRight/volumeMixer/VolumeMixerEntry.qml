import qs.modules.common
import qs.modules.common.widgets
import qs.services
import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Pipewire

Item {
	id: root
	required property PwNode node
	property real preMuteVolume: -1

	function isOverSlider(x, y) {
		var sliderPos = slider.mapFromItem(root, x, y)
		return sliderPos.x >= 0 && sliderPos.x <= slider.width
			&& sliderPos.y >= 0 && sliderPos.y <= slider.height
	}

	function handleVolumeWheel(delta) {
		const step = 0.05
		const volDelta = delta > 0 ? step : -step
		slider.value = Math.max(0, Math.min(1, slider.value + volDelta))
	}

	PwObjectTracker {
		objects: [node]
	}

	implicitHeight: rowLayout.implicitHeight

	RowLayout {
		id: rowLayout
		anchors.fill: parent
		spacing: 6

		Image {
			Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
			property real size: slider.height * 0.9
			Layout.preferredWidth: size
			Layout.preferredHeight: size
			width: size
			height: size
			visible: source != ""
			sourceSize.width: size
			sourceSize.height: size
			source: {
				let icon;
				icon = AppSearch.guessIcon(root.node.properties["application.icon-name"]);
				if (AppSearch.iconExists(icon)) {
					return Quickshell.iconPath(icon, "image-missing");
				}
				icon = AppSearch.guessIcon(root.node.properties["node.name"]);
				return Quickshell.iconPath(icon, "image-missing");
			}
			opacity: root.node.audio.muted ? 0.4 : 1.0
			Behavior on opacity { NumberAnimation { duration: 150 } }

			MaterialSymbol {
				anchors.right: parent.right
				anchors.bottom: parent.bottom
				iconSize: parent.size * 0.4
				text: "volume_off"
				color: Appearance.m3colors.m3error
				opacity: root.node.audio.muted ? 1 : 0
				Behavior on opacity { NumberAnimation { duration: 150 } }
			}

			MouseArea {
				anchors.fill: parent
				cursorShape: Qt.PointingHandCursor
				onClicked: {
					if (root.node.audio.muted) {
						if (root.preMuteVolume >= 0) {
							root.node.audio.volume = root.preMuteVolume
						}
						root.preMuteVolume = -1
						root.node.audio.muted = false
					} else {
						root.preMuteVolume = root.node.audio.volume
						root.node.audio.muted = true
					}
				}
			}
		}

		ColumnLayout {
			Layout.fillWidth: true
			spacing: -4

			StyledText {
				Layout.fillWidth: true
				font.pixelSize: Appearance.font.pixelSize.small
				color: Appearance.colors.colSubtext
				elide: Text.ElideRight
				text: {
					// application.name -> description -> name
					const app = root.node.properties["application.name"] ?? (root.node.description != "" ? root.node.description : root.node.name);
					const media = root.node.properties["media.name"];
					return media != undefined ? `${app} • ${media}` : app;
				}
			}

			StyledSlider {
				id: slider
				value: root.node.audio.volume
				onValueChanged: root.node.audio.volume = value
			}
		}
	}
}
