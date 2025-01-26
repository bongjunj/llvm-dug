DUNE = dune

.PHONY: all
all:
	$(DUNE) build
	ln -sf _build/default/bin/main.exe llvm-dug
