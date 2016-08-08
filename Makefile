CURRY := $(shell command -v curry 2> /dev/null)
CPM := $(shell command -v cpm 2> /dev/null)

.PHONY: test
test:
ifndef CURRY
	$(error "curry is not available, please add it to your PATH")
endif
ifndef CPM
	$(error "cpm is not available, please add it to your PATH")
endif
	cpm install
	cd src; \
	cpm exec "currycheck JSON.Parser"; \
	cd ..
