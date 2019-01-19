r:
	@stack exec nonosolver-exe
b:
	@stack build
logs:
	mkdir log
	touch ./log/.keep
	touch ./log/access.log
	touch ./log/error.log
daemon:
	setsid "$(shell find ./.stack-work/install -name nonosolver-exe)" >/dev/null 2>&1 < /dev/null &
listps:
	ps -eo 'tty,pid,comm' | grep nonosolver-exe