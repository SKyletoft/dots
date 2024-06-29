complete -cf doas
bind "set completion-ignore-case on"
eval "$(direnv hook bash)"
export DIRENV_LOG_FORMAT=""

if [[ $- =~ .*i.* ]]; then
	bind '"\C-r": "\C-a hstr -- \C-j"'
	export HSTR_CONFIG=hicolor
fi

if [[ "$DISPLAY" ]]; then
	gsettings set org.gnome.desktop.input-sources xkb-options \[\'caps:swapescape\'\]
	xset r rate 300 40 # Hz
	gsettings set org.gnome.desktop.peripherals.keyboard delay 300
	gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 25 # ms
fi

if ! pidof emacs -q ; then
	clear
	emacs --daemon 2> /dev/null
	echo "Started Emacs daemon"
fi

if ! pidof ssh-agent -q ; then
	eval "$(ssh-agent -s)" > /dev/null 2> /dev/null
	echo "Started ssh agent"
fi
ssh-add ~/.ssh/* > /dev/null 2> /dev/null

if [ "$TERM" == linux ]; then
	PS1='\[\033[01;32m\]\u \[\033[01;34m\]\w\[\033[00m\] \$ '
elif [ "$TERM" == xterm-256color ] || [ "$TERM" == eterm-color ]; then
	PS1='\e[1;97;42;24m \u \e[21;32;44;24m\e[1;97;44;24m \h \e[21;34;41;24m\e[1;97;41m \w \001\e[21;31;49;24m\002\n\001\e[97;1m\002↳\001\e[0m\002 '
else
	PS1='\u@\h $ '
fi

run () {
	if [ -n "$2" ]; then
		nix-shell -p $1 --run $2
	else
		nix-shell -p $1 --run $1
	fi
}
