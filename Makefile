EMACS   := emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile

EL  = yandex-weather.el
EL += org-yandex-weather.el

ELC = $(EL:.el=.elc)

EXTRA_DIST = README.md LICENSE

.PHONY : all compile clean test

all : compile

test:
	$(BATCH) -L tests -l tests/yandex-weather-tests.el -f ert-run-tests-batch

compile: $(ELC)

clean:
	$(RM) *.elc

%.elc: %.el
	@$(COMPILE) $<
