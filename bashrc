complete -cf doas
bind "set completion-ignore-case on"
eval "$(direnv hook bash)"
export DIRENV_LOG_FORMAT=""
export EDITOR="vi"

if [ "$TERM" == linux ]; then
	PS1='\[\033[01;32m\]\u \[\033[01;34m\]\w\[\033[00m\] \$ '
elif [ "$__vsc_initialized" == 1 ]; then
	PS1='\e[32;1m\u: \e[34m\w \[\033[00m\] [bash]\n↳ '
else
	PS1='\e[1;97;42;24m \u \e[21;32;44;24m\e[1;97;44;24m \h \e[21;34;41;24m\e[1;97;41m \w \001\e[21;31;49;24m\002\n\001\e[97;1m\002↳\001\e[0m\002 '
fi

iptsd () {
	if [ $1 == "on" ]; then
		doas systemctl reload-or-restart iptsd
	elif [ $1 == "off" ]; then
		doas systemctl kill iptsd
	elif [ $1 == "status" ]; then
		systemctl is-enabled iptsd
	else
		echo "Invalid command"
	fi
}

run () {
	if [ -n "$2" ]; then
		nix-shell -p $1 --run $2
	else
		nix-shell -p $1 --run $1
	fi
}

lorri_init() {
	lorri init
	direnv allow
}
