import dataclasses
import os
import sqlite3
import tomllib
from pathlib import Path
from typing import Self

from .fact import Fact

CONFIG_FILE = "config.toml"


@dataclasses.dataclass
class Field:
    name: str
    type: str
    source_suffix: str
    query: str


@dataclasses.dataclass
class Database:
    path_prefix_env: str
    path: str
    source: str
    fields: list[Field]

    @classmethod
    def from_dict(cls, **data) -> Self:
        fields: list[Field] = []
        for field in data["fields"].values():
            fields.append(Field(**field))
        rest = {k: v for k, v in data.items() if k != "fields"}
        return cls(fields=fields, **rest)


@dataclasses.dataclass
class Fact:
    source: str
    id: str
    rel: str
    value_type: str
    value: str


def process_fields(db: Database) -> list[Fact]:
    facts = []
    filepath = os.getenv(db.path_prefix_env, "") + "/" + db.path
    path = Path(filepath)
    if not path.is_file():
        print(f"ERROR: cannot open {filepath}, skipping")
        return []
    conn = sqlite3.connect(filepath)

    for field in db.fields:
        print(
            f"Processing {db.source}/{field.source_suffix}/{field.name} ({field.type})"
        )
        cur = conn.cursor()
        cur.execute(field.query)
        rows = cur.fetchall()
        for row in rows:
            source = f"db/{db.source}/{field.source_suffix}"
            facts.append(Fact(source, row[0], field.name, field.type, row[1]))

    return facts


def main():
    with open(CONFIG_FILE, "rb") as tomlfile:
        config = tomllib.load(tomlfile)

    facts: list[Fact] = []

    for db_vals in config["databases"].values():
        db = Database.from_dict(**db_vals)
        print(f"Parsed database config: {db.source}")
        facts += process_fields(db)

    print(facts)


if __name__ == "__main__":
    main()
