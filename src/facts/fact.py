import csv
import json
import sqlite3
from dataclasses import asdict, dataclass
from pathlib import Path

CSV_HEADER = ("source", "id", "rel", "type", "value")


@dataclass
class Fact:
    source: str
    id: str
    rel: str
    value_type: str
    value: str

    def to_ntriple(self):
        return (
            f"<https://rdf.domson.dev/sources/{self.source}/{self.id}> "
            + f"<https://rdf.domson.dev/predicates/{self.rel}> "
            + f'"{self.value}"^^<https://rdf.domson.dev/types/{self.value_type}> .'
        )

    def to_prolog(self):
        return f'fact("{self.source}", "{self.id}", "{self.rel}", "{self.value_type}", "{self.value}").'

    def to_csv_row(self):
        return f'"{self.source}","{self.id}","{self.rel}","{self.value_type}","{self.value}"'

    def to_json(self):
        return json.dumps(asdict(self))


def write_csv(filepath: str, facts: list[Fact], write_header=False) -> None:
    print(f"Writing {len(facts)} facts as CSV")
    with open(filepath, "w") as file:
        writer = csv.writer(file, csv.unix_dialect)
        if write_header:
            writer.writerow(CSV_HEADER)
        writer.writerows((f.source, f.id, f.rel, f.value_type, f.value) for f in facts)


def write_prolog(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as Prolog")
    contents = (f.to_prolog() for f in facts)
    Path(filepath).write_text("\n".join(contents) + "\n")


def write_ntriples(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as N-Triples")
    contents = [f.to_ntriple() for f in facts]
    Path(filepath).write_text("\n".join(contents) + "\n")


def write_json(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as JSON")
    with open(filepath, "w") as file:
        json.dump(facts, file, default=asdict)


def write_sqlite(filepath: str, facts: list[Fact]) -> None:
    CREATE_FACTS = """
    CREATE TABLE facts (
        source VARCHAR,
        id VARCHAR,
        rel VARCHAR,
        type VARCAR,
        value VARCHAR
    )
    """
    INSERT_FACTS = """
    INSERT INTO facts VALUES (:source, :id, :rel, :value_type, :value)
    """
    print(f"Writing {len(facts)} facts as SQLite")
    with sqlite3.connect(filepath) as conn:
        conn.execute("DROP TABLE IF EXISTS facts")
        conn.execute(CREATE_FACTS)
        conn.executemany(INSERT_FACTS, map(asdict, facts))
