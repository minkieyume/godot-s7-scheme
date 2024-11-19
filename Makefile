.PHONY: test

test: bin/s7 demo/.godot
	@echo 🧪 Running tests
	scons && bin/s7 test/test-main.scm

bin/s7: s7/s7.c
	@echo ⚙️ Building scheme interpreter
	gcc s7/s7.c -o bin/s7 -DWITH_MAIN -DWITH_SYSTEM_EXTRAS -DWITH_C_LOADER=0 -I. -O2 -g -ldl -lm

demo/.godot: $(wildcard demo/addons/**) $(wildcard demo/bin/**)
	@echo 📦 Importing test scene
	godot --path demo --headless --import

.PHONY: run

run:
	scons && godot -e --path demo main.tscn

.PHONY: android

android:
	scons platform=android target=template_debug

.PHONY: test-watch

test-watch:
	find demo/addons/s7 test | entr make test
