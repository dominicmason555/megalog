import dataclasses
import os
import sqlite3
import tomllib
from pathlib import Path
from typing import Self

from .fact import Fact

CONFIG_FILE = "config_from_sqlite.toml"


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


def process_fields(db: Database) -> list[Fact]:
    facts = []
    filepath = os.getenv(db.path_prefix_env, "") + "/" + db.path
    path = Path(filepath)
    if not path.is_file():
        print(f"ERROR: cannot open {filepath}, skipping")
        return []
    conn = sqlite3.connect(filepath)

    for field in db.fields:
        cur = conn.cursor()
        cur.execute(field.query)
        rows = cur.fetchall()
        for row in rows:
            source = f"db/{db.source}/{field.source_suffix}"
            facts.append(Fact(source, row[0], field.name, field.type, row[1]))

    return facts


def get_facts() -> list[Fact]:
    with open(CONFIG_FILE, "rb") as tomlfile:
        config = tomllib.load(tomlfile)

    facts: list[Fact] = []

    for db_vals in config["databases"].values():
        db = Database.from_dict(**db_vals)
        print(f"Reading SQLite database: {db.source}")
        facts += process_fields(db)

    return facts


def main():
    facts: list[Fact] = get_facts()

    print("\n".join(f.to_json() for f in facts))


if __name__ == "__main__":
    main()
