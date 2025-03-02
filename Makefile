ifeq ($(OS),Windows_NT)
		SHELL := powershell.exe
		.SHELLFLAGS := -NoProfile -Command
endif

.PHONY: all clean run

all: hobors.dll

RUST_SOURCES := $(shell Get-ChildItem -Recurse -Filter *.rs -File src | Select-Object -ExpandProperty FullName)

hobors.dll: target/debug/hobors.dll
	Copy-Item target/debug/hobors.dll hobors.dll

target/debug/hobors.dll: $(RUST_SOURCES) Cargo.toml
	cargo build

clean:
	Remove-Item -ErrorAction SilentlyContinue hobors.dll
	cargo clean

run: all
	emacs --eval "(hobo-start)"
