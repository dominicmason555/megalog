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


SQLITE_STATEMENTS = [
    """DROP VIEW IF EXISTS "LastUnfinishedGames" """,
    """DROP VIEW IF EXISTS "LastPlayedGames" """,
    """DROP VIEW IF EXISTS "ToPlay" """,
    """DROP VIEW IF EXISTS "UnfinishedGames" """,
    """DROP VIEW IF EXISTS "UnfinishedBooks" """,
    """DROP TABLE IF EXISTS "facts" """,
    """
    CREATE TABLE facts (
        source VARCHAR,
        id VARCHAR,
        rel VARCHAR,
        type VARCAR,
        value VARCHAR
    )
    """,
    """
CREATE VIEW LastPlayedGames AS
WITH
    TitleID AS (
        SELECT "value" as "LastPlayedTitle", "id" as "LastPlayedID"
	    FROM facts
        WHERE "type" = 'Game'
        AND ("rel" = 'Started' OR "rel" = 'Played' OR "rel" = 'Finished')
    ),
    DateID AS (
        SELECT "value" as "PlayedDate", "id" as "PlayedDateID"
        from facts
        WHERE "type" = 'Day'
	)
SELECT
    "LastPlayedTitle" AS Title,
	max("PlayedDate") AS "LastPlayedDate"
FROM
TitleID INNER JOIN DateID
ON "LastPlayedID" = "PlayedDateID"
GROUP BY "LastPlayedTitle"
ORDER BY "LastPlayedDate"
    """,
    """
CREATE VIEW LastReadBooks AS
WITH
    TitleID AS (
        SELECT "value" as "LastReadTitle", "id" as "LastReadID"
	    FROM facts
        WHERE "type" = 'Book'
        AND ("rel" = 'Started' OR "rel" = 'Read' OR "rel" = 'Finished' OR "rel" = 'Listened')
    ),
    DateID AS (
        SELECT "value" as "ReadDate", "id" as "ReadDateID"
        from facts
        WHERE "type" = 'Day'
	)
SELECT
    "LastReadTitle" AS Title,
	max("ReadDate") AS "LastReadDate"
FROM
TitleID INNER JOIN DateID
ON "LastReadID" = "ReadDateID"
GROUP BY "LastReadTitle"
ORDER BY "LastReadDate"
    """,
    """
CREATE VIEW "ToPlay" AS
WITH Games
    AS (SELECT "id",
               "value" AS "Title"
        FROM facts
        WHERE type = 'Game'
        AND "rel" = 'ToPlay'),
    Durations
    AS (SELECT "id",
               Cast(Rtrim("value", 'h') AS INTEGER) AS "Takes Hours"
        FROM facts
        WHERE type = 'Duration'
        AND "rel" = 'Takes')
SELECT "Title",
       "Takes Hours"
FROM Games
       LEFT JOIN Durations
              ON Games.id = Durations.id
ORDER BY "Takes Hours"
    """,
    """
CREATE VIEW UnfinishedGames AS
WITH StartedGames
    AS (SELECT DISTINCT "value" AS "Title"
        FROM facts
        WHERE type = 'Game'
        AND ("rel" = 'Started' OR "rel" = 'Played')),
	FinishedGames
    AS (SELECT DISTINCT "value" AS "Title"
        FROM facts
        WHERE type = 'Game'
        AND "rel" = 'Finished')
SELECT "Title"
FROM StartedGames
EXCEPT
SELECT "Title"
FROM FinishedGames
ORDER BY "Title"
    """,
    """
CREATE VIEW LastUnfinishedGames AS
SELECT
    UnfinishedGames.Title,
    LastPlayedDate
FROM UnfinishedGames
LEFT JOIN LastPlayedGames
ON UnfinishedGames.Title = LastPlayedGames.Title
ORDER BY LastPlayedDate, UnfinishedGames.Title
    """,
    """
CREATE VIEW UnfinishedBooks AS
WITH StartedBooks
    AS (SELECT DISTINCT "value" AS "Title"
        FROM facts
        WHERE type = 'Book'
        AND ("rel" = 'Started' OR "rel" = 'Read' OR "rel" = 'Listened')),
	FinishedBooks
    AS (SELECT DISTINCT "value" AS "Title"
        FROM facts
        WHERE type = 'Book'
        AND "rel" = 'Finished')
SELECT "Title"
FROM StartedBooks
EXCEPT
SELECT "Title"
FROM FinishedBooks
ORDER BY "Title"
    """,
    """
CREATE VIEW LastUnfinishedBooks AS
SELECT
    UnfinishedBooks.Title,
    LastReadDate
FROM UnfinishedBooks
LEFT JOIN LastReadBooks
ON UnfinishedBooks.Title = LastReadBooks.Title
ORDER BY LastReadDate, UnfinishedBooks.Title
    """,
]


def write_sqlite(filepath: str, facts: list[Fact]) -> None:
    INSERT_FACTS = """
    INSERT INTO facts VALUES (:source, :id, :rel, :value_type, :value)
    """
    print(f"Writing {len(facts)} facts as SQLite")
    with sqlite3.connect(filepath) as conn:
        for statement in SQLITE_STATEMENTS:
            conn.execute(statement)
        conn.executemany(INSERT_FACTS, map(asdict, facts))
