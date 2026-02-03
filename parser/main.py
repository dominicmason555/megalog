#!/usr/bin/env python3

import csv
import json
import sqlite3
import sys
import tomllib
from dataclasses import asdict, dataclass
from datetime import datetime
from pathlib import Path
from typing import Callable, Optional

CONF_FILE = "config.toml"


@dataclass
class Fact:
    subject_t: str
    subject: str
    verb: str
    object_t: str
    object: str


def match_date(text: str) -> Optional[str]:
    try:
        datetime.strptime(text, "%Y-%M-%d")
        return text[:10]
    except ValueError:
        return None


@dataclass
class HeaderLine:
    level: int
    start_date: Optional[str]
    end_date: Optional[str]

    @classmethod
    def parse(cls, line: str) -> Optional["HeaderLine"]:
        pos = 0
        level = 0
        while pos < len(line) and line[pos] == "#":
            level += 1
            pos += 1
        pos += 1
        if level > 0:
            if len(line) >= pos + 10:
                if start_date := match_date(line[pos : pos + 10]):
                    pos += 10
                    if len(line) >= pos + 13:
                        if end_date := match_date(line[pos + 3 : pos + 13]):
                            return cls(level, start_date, end_date)
                    return cls(level, start_date, None)
                return cls(level, None, None)
            else:
                return cls(level, None, None)


def get_to_char(line: str, end: str, banned: str) -> tuple[int, Optional[str]]:
    pos = 0
    value = ""
    while pos < len(line):
        if line[pos] in banned:
            pos += 1
            return pos, None
        elif line[pos] == end:
            pos += 1
            if value != "":
                return pos, value
            return pos, None
        else:
            if not line[pos].isspace():
                value += line[pos]
            pos += 1
    return pos, None


@dataclass
class ParsedAttr:
    key: str
    value: str
    subject_found: bool


@dataclass
class NormalParserState:
    pos: int
    key: str
    attrs: list[ParsedAttr]
    subject_found: bool


NormalParserReturn = tuple[Optional["NormalParserFun"], NormalParserState]
NormalParserFun = Callable[[NormalParserState], NormalParserReturn]


# Datemachine
def parse_key_val(line: str) -> list[ParsedAttr]:
    # TODO: Multiple values separated by space
    # TODO Split object type and object on the . character
    def parse_value(state: NormalParserState) -> NormalParserReturn:
        pos_change, value = get_to_char(line[state.pos :], "]", "[:")
        state.pos += pos_change
        if value is not None:
            state.attrs.append(
                ParsedAttr(state.key.lower(), value.lower(), state.subject_found)
            )
        return parse_outside, state

    def parse_key(state: NormalParserState) -> NormalParserReturn:
        pos_change, key = get_to_char(line[state.pos :], ":", "[]")
        state.pos += pos_change
        if key is not None:
            if len(line) >= state.pos and line[state.pos] == ":":
                state.pos += 1
                state.subject_found = True
            state.key = key
            return parse_value, state
        return parse_outside, state

    def parse_outside(state: NormalParserState) -> NormalParserReturn:
        while state.pos < len(line):
            if line[state.pos] == "[":
                state.pos += 1
                state.subject_found = False
                return parse_key, state
            state.pos += 1
        return None, state

    state = NormalParserState(0, "", [], False)
    parser: Optional[NormalParserFun] = parse_outside
    while parser is not None:
        parser, state = parser(state)
    return state.attrs


@dataclass
class NormalLine:
    attrs: list[ParsedAttr]

    @classmethod
    def parse(cls, line: str) -> Optional["NormalLine"]:
        if attrs := parse_key_val(line):
            return cls(attrs)


Line = NormalLine | HeaderLine


def parse_line(line: str) -> Optional[Line]:
    if header := HeaderLine.parse(line):
        return header
    elif normal := NormalLine.parse(line):
        return normal


def parse_file(facts: list[Fact], filename: str, contents: str) -> list[Fact]:
    def process_header(
        headers: list[HeaderLine], header: HeaderLine
    ) -> list[HeaderLine]:
        while len(headers):
            if header.level > headers[-1].level:
                break
            else:
                headers.pop()
        headers.append(header)
        return headers

    def get_day(headers: list[HeaderLine]) -> Optional[str]:
        for header in reversed(headers):
            if header.start_date:
                return header.start_date

    def record_normal(
        facts: list[Fact], day: Optional[str], attrs: list[ParsedAttr]
    ) -> list[Fact]:
        subject: Optional[ParsedAttr] = None
        for attr in attrs:
            if not attr.subject_found:
                subject = attr
                facts.append(
                    Fact(
                        "line",
                        loc,
                        attr.key,
                        attr.value.split(".")[0],
                        attr.value.split(".")[1],
                    )
                )
            if attr.subject_found and subject:
                # TODO: No split
                facts.append(
                    Fact(
                        subject.value.split(".")[0],
                        subject.value.split(".")[1],
                        attr.key,
                        attr.value.split(".")[0],
                        attr.value.split(".")[1],
                    )
                )
            elif day:
                facts.append(
                    Fact(
                        "day",
                        day,
                        attr.key,
                        ("PROP" if attr.subject_found else "")
                        + attr.value.split(".")[0],
                        attr.value.split(".")[1],
                    )
                )
        return facts

    headers: list[HeaderLine] = []
    for line_num, raw_line in enumerate(contents.splitlines()):
        loc = f"{filename}:{line_num + 1}"
        if line := parse_line(raw_line):
            # facts.append(Fact("line", loc, "raw_line", raw_line))
            match line:
                case HeaderLine() as header:
                    headers = process_header(headers, header)
                    if header.start_date:
                        facts.append(
                            Fact("line", loc, "LOGGED", "day", header.start_date)
                        )

                case NormalLine(attrs):
                    facts = record_normal(facts, get_day(headers), attrs)

    return facts


def write_csv(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as CSV")
    with open(filepath, "w") as file:
        writer = csv.writer(file, csv.unix_dialect)
        writer.writerow(("subject_type", "subject", "verbobject_type", "object"))
        writer.writerows(
            (f.subject_t, f.subject, f.verb, f.object_t, f.object) for f in facts
        )


def write_prolog(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as Prolog")
    contents = (
        f'fact("{f.subject_t}", "{f.subject}", "{f.verb}", "{f.object_t}", "{f.object}").'
        for f in facts
    )
    Path(filepath).write_text("\n".join(contents))


def write_json(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as JSON")
    with open(filepath, "w") as file:
        json.dump(facts, file, default=asdict)


def write_sqlite(filepath: str, facts: list[Fact]) -> None:
    CREATE_FACTS = """
    CREATE TABLE facts (
        subject_t VARCHAR, subject VARCHAR,
        veb VARCHAR,
        object_t VARCAR, object VARCHAR
    )
    """
    INSERT_FACTS = """
    INSERT INTO facts VALUES (:subject_t, :subject, :verb, :object_t, :object)
    """
    print(f"Writing {len(facts)} facts as SQLite")
    with sqlite3.connect(filepath) as conn:
        conn.execute("DROP TABLE IF EXISTS facts")
        conn.execute(CREATE_FACTS)
        conn.executemany(INSERT_FACTS, map(asdict, facts))


def main():
    with open(CONF_FILE, "rb") as tomlfile:
        config = tomllib.load(tomlfile)

    paths: dict[str, str] = config["paths"]
    facts: list[Fact] = []

    for filename, filepath in paths.items():
        contents = Path(filepath).expanduser().read_text()
        facts = parse_file(facts, filename, contents)

    if len(sys.argv) > 1:
        if sys.argv[1].endswith(".csv"):
            write_csv(sys.argv[1], facts)

        elif sys.argv[1].endswith(".pl"):
            write_prolog(sys.argv[1], facts)

        elif sys.argv[1].endswith(".json"):
            write_json(sys.argv[1], facts)

        elif sys.argv[1].endswith(".db"):
            write_sqlite(sys.argv[1], facts)

    else:
        days = set()
        printable_facts = ""
        for fact in facts:
            if fact.subject_t != "line":
                days.add(fact.subject)
                printable_facts += f"{fact.subject_t + ': ' + fact.subject:22}"
                printable_facts += f"{fact.verb:14}"
                printable_facts += f"{fact.object_t}: {fact.object}\n"
        print(f"Mega Log: {len(facts)} facts from {len(days)} days\n")
        print(printable_facts)


if __name__ == "__main__":
    main()
