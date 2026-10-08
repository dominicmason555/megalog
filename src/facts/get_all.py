import sys
from pathlib import Path

from .fact import (
    CSV_HEADER,
    Fact,
    write_csv,
    write_json,
    write_ntriples,
    write_prolog,
    write_sqlite,
)
from .from_sqlite import get_facts as get_sqlite
from .megalog import get_facts as get_megalog
from .directories import get_facts_path

try:
    from .from_aw import get_facts as get_aw
except ImportError:

    def get_aw() -> list[Fact]:
        print("ActivityWatch Disabled")
        return []


FILETYPES = {
    "csv": write_csv,
    "json": write_json,
    "nt": write_ntriples,
    "pl": write_prolog,
}


def get_facts() -> list[Fact]:
    facts = get_megalog()
    facts += get_sqlite()
    facts += get_aw()
    return facts


def partition_facts(facts: list[Fact]) -> dict[str, list[Fact]]:
    fact_dict: dict[str, list[Fact]] = {}
    for fact in facts:
        if fact.source not in fact_dict:
            fact_dict[fact.source] = []
        fact_dict[fact.source].append(fact)
    return fact_dict


def main():
    facts = get_facts()

    partitioned = partition_facts(facts)
    for key in partitioned.keys():
        print(f"From {key}: {len(partitioned[key])} facts")

    csv_header = ",".join(CSV_HEADER) + "\n"

    if facts_dir := get_facts_path():
        for filetype, func in FILETYPES.items():
            # Write each source to its own file, overwrite only what we parsed
            for name, part in partitioned.items():
                filename = name.replace("/", "_") + "." + filetype
                func(str((facts_dir / filename).absolute()), part)

            # Read all files back and combine, including what we didn't parse this time
            all_file = Path(facts_dir / f"all.{filetype}")
            all_file.unlink()
            files_found = facts_dir.glob(f"*.{filetype}")
            contents = csv_header if filetype == "csv" else ""
            for file in files_found:
                contents += file.read_text()
            all_file.write_text(contents)
            print(f"Writing to {all_file}")
        write_sqlite(str(facts_dir / "all.db"), facts)
    else:
        print("ERROR: Cannot find facts path", file=sys.stderr)
