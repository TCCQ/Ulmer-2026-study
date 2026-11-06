"""Print the AST produced by resolving templates in the gfact example."""
from pathlib import Path
from FWATS3_2parser import program_parse
from FWATS3_2pretty import program_pretty
from FWATS3_2tmpres import program_tmpres


ROOT = Path(__file__).resolve().parents[1]


def main() -> None:
    source = (ROOT / "mylib/1.fwats3").read_text()
    source += (ROOT / "examples/gfact.fwats3").read_text()
    source += """
imp fact_mult[sint] = lam(xy: (sint, sint)) => xy.0 * xy.1;
imp fact_mult_unit[sint] = lam(u: ()) => 1;
@gfact[sint](5)
"""
    program = program_parse(source, filename="gfact.fwats3")
    resolved = program_tmpres(program)
    print(program_pretty(resolved))
    source = (ROOT / "mylib/1.fwats3").read_text()
    source += (ROOT / "examples/gfact.fwats3").read_text()
    source += """
imp fact_mult[sint] = lam(xy: (sint, sint)) => xy.0 + xy.1;
imp fact_mult_unit[sint] = lam(u: ()) => 0;
@gfact[sint](5)
"""
    program = program_parse(source, filename="gfact.fwats3")
    resolved = program_tmpres(program)
    print(program_pretty(resolved))


if __name__ == "__main__":
    main()
