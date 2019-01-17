r:
	@stack exec nonosolver-exe
b:
	@stack build
logs:
	mkdir log
	touch ./log/.keep
	touch ./log/access.log
	touch ./log/error.log