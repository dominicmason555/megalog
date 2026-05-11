import os
from pathlib import Path

from .fact import (
    CSV_HEADER,
    Fact,
    write_csv,
    write_json,
    write_ntriples,
    write_prolog,
)
from .from_sqlite import get_facts as get_sqlite
from .from_aw import get_facts as get_aw
from .megalog import get_facts as get_megalog


FILETYPES = {
    "csv": write_csv,
    "json": write_json,
    "nt": write_ntriples,
    "pl": write_prolog,
}

FACTS_PATH_ENV = "WELLKNOWN_SYNC_PERSONAL"
FACTS_PATH = "facts"


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

    if parent := os.getenv(FACTS_PATH_ENV):
        facts_dir = Path(parent) / FACTS_PATH
        if facts_dir.is_dir():
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

        else:
            print(f"ERROR: Cannot find {facts_dir}")
    else:
        print(f"ERROR: Cannot find {FACTS_PATH_ENV}")
