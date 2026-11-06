"""Type-check and run the generic gfact example."""
from pathlib import Path

from FWATS3_2interp import libfile_evaluate, program_evaluate
from FWATS3_2parser import libfile_parse, program_parse
from FWATS3_2tcheck import libfile_tcheck, program_tcheck


ROOT = Path(__file__).resolve().parents[1]


def evaluate(source: str) -> int:
    type_environment = None
    variable_environment = {}
    template_environment = ()

    library_path = ROOT / "mylib/1.fwats3"
    declarations = libfile_parse(
        library_path.read_text(), filename=str(library_path))
    type_environment = libfile_tcheck(declarations, type_environment)
    variable_environment, template_environment = libfile_evaluate(
        declarations, variable_environment, template_environment)

    program = program_parse(source, filename="examples/gfact.fwats3")
    program_tcheck(program, type_environment)
    return program_evaluate(program, variable_environment, template_environment)


def main() -> None:
    source_path = ROOT / "examples/gfact.fwats3"
    generic_source = source_path.read_text()
    implementations = (
        "imp fact_mult[sint] = lam(xy: (sint, sint)) => xy.0 * xy.1;\n"
        "imp fact_mult_unit[sint] = lam(u: ()) => 1;\n"
    )
    factorial = evaluate(generic_source + implementations + "@gfact[sint](5)")
    if factorial != 120:
        raise AssertionError(f"Expected 5! = 120, got {factorial!r}")

    sum_implementations = implementations.replace(
        "xy.0 * xy.1", "xy.0 + xy.1"
    ).replace(
        "=> 1;", "=> 0;"
    )
    sum_source = generic_source + sum_implementations + "@gfact[sint](5)"
    total = evaluate(sum_source)
    if total != 15:
        raise AssertionError(f"Expected 1 + ... + 5 = 15, got {total!r}")

    print("gfact.fwats3: passed (5! = 120; sum 1..5 = 15)")


if __name__ == "__main__":
    main()
