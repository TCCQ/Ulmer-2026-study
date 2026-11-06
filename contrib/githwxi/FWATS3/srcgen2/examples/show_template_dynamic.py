"""Print the template-resolved template-dynamic example as FWATS3 source."""
from pathlib import Path

from FWATS3_2parser import program_parse
from FWATS3_2pretty import program_pretty
from FWATS3_2tmpres import program_tmpres


ROOT = Path(__file__).resolve().parents[1]


def main() -> None:
    source_path = ROOT / "examples/template_dynamic.fwats3"
    program = program_parse(source_path.read_text(), filename=str(source_path))
    resolved = program_tmpres(program)
    print(program_pretty(resolved))


if __name__ == "__main__":
    main()
