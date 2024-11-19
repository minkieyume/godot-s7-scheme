.PHONY: test

test: bin/s7 demo/.godot
	scons && bin/s7 test/test-main.scm

demo/.godot: $(wildcard demo/addons/**) $(wildcard demo/bin/**)
	godot --path demo --headless --import

bin/s7: s7/s7.c
	mkdir -p bin
	gcc s7/s7.c -o bin/s7 -DWITH_MAIN -DWITH_SYSTEM_EXTRAS -DWITH_C_LOADER=0 -I. -O2 -g -ldl -lm

s7: bin/s7

.PHONY: run

run:
	scons && godot -e --path demo main.tscn

.PHONY: android

android:
	scons platform=android target=template_debug

.PHONY: test-watch

test-watch:
	find demo/addons/s7 test | entr make test
