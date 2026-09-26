MYPY ?= mypy
PYTHON ?= python3

all: tcheck

.PHONY: all tcheck

PYFILES=FWATS3_2basics.py FWATS3_2interp.py FWATS3_2staexp.py FWATS3_2template.py

tcheck:
	$(MYPY) --strict ${PYFILES}


