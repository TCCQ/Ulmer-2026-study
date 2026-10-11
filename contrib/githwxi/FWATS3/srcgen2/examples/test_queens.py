"""Type-check and run the 8-queens FWATS3 example."""
from pathlib import Path

from FWATS3_2interp import libfile_evaluate, program_evaluate
from FWATS3_2parser import libfile_parse, program_parse
from FWATS3_2tcheck import libfile_tcheck, program_tcheck


ROOT = Path(__file__).resolve().parents[1]


def main() -> None:
    type_environment = None
    variable_environment = {}
    template_environment = ()

    for relative_path in ("mylib/1.fwats3", "mylib/list.fwats3"):
        source_path = ROOT / relative_path
        declarations = libfile_parse(
            source_path.read_text(), filename=str(source_path))
        type_environment = libfile_tcheck(declarations, type_environment)
        variable_environment, template_environment = libfile_evaluate(
            declarations, variable_environment, template_environment)

    source_path = ROOT / "examples/queens.fwats3"
    program = program_parse(source_path.read_text(), filename=str(source_path))
    program_tcheck(program, type_environment)
    result = program_evaluate(program, variable_environment, template_environment)
    if result != 92:
        raise AssertionError(f"Expected 92 solutions, got {result!r}")
    print("queens.fwats3: passed (92 solutions)")


if __name__ == "__main__":
    main()
