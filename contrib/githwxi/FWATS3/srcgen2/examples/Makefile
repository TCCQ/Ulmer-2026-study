PYTHON ?= python3

.PHONY: test show_gfact_tmpres show_template_dynamic

test:: test_gfact
test:: test_queens

test_gfact:
	PYTHONPATH=.. $(PYTHON) test_gfact.py

show_gfact_tmpres:
	PYTHONPATH=.. $(PYTHON) show_gfact_tmpres.py

show_template_dynamic:
	PYTHONPATH=.. $(PYTHON) show_template_dynamic.py

test_queens:
	PYTHONPATH=.. $(PYTHON) test_queens.py
