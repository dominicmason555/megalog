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
    source: str
    id: str
    rel: str
    value_type: str
    value: str


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
    title: Optional[str]

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
                            title = line[pos + 13 :] if len(line) > pos + 13 else None
                            return cls(level, start_date, end_date, title)
                    title = line[pos:] if len(line) > pos else None
                    return cls(level, start_date, None, title)
                return cls(level, None, None, None)
            else:
                return cls(level, None, None, None)


def get_to_char(
    line: str, end: str, banned: str
) -> tuple[int, Optional[str], Optional[str]]:
    pos = 0
    value = ""
    while pos < len(line):
        if line[pos] in banned:
            pos += 1
            return pos, None, None
        elif line[pos] in end:
            pos += 1
            if value != "":
                return pos, value, line[pos - 1]
            return pos, None, None
        else:
            if (value != "") or (not line[pos].isspace()):
                value += line[pos]
            pos += 1
    return pos, None, None


@dataclass
class ParsedAttr:
    rel: str
    obj_t: str
    obj: str
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
    def parse_value(state: NormalParserState) -> NormalParserReturn:
        pos_change, value, ender = get_to_char(line[state.pos :], ";]", "[")
        state.pos += pos_change
        if value is not None:
            split = value.split(".")
            obj_t = split[0]
            obj = obj_t
            if len(split) > 1:
                obj = ".".join(s for s in split[1:])
            state.attrs.append(ParsedAttr(state.key, obj_t, obj, state.subject_found))
        if ender == ";":
            return parse_value, state
        return parse_outside, state

    def parse_key(state: NormalParserState) -> NormalParserReturn:
        pos_change, key, _ = get_to_char(line[state.pos :], ":", "[]")
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

    def get_day(headers: list[HeaderLine]) -> Optional[HeaderLine]:
        for header in reversed(headers):
            if header.start_date:
                return header

    def record_normal(
        facts: list[Fact],
        head: Optional[HeaderLine],
        attrs: list[ParsedAttr],
        file_name: str,
        line_num: int,
    ) -> list[Fact]:
        subject_counter = 0
        source = f"megalog/{file_name}"
        for attr in attrs:
            subject_counter += 0 if attr.subject_found else 1
            loc = f"{line_num}:{subject_counter}"
            if subject_counter > 0:
                if (
                    (not attr.subject_found)
                    and (head is not None)
                    and (head.start_date is not None)
                ):
                    facts.append(Fact(source, loc, "Date", "Day", head.start_date))
                    if head.end_date is not None:
                        facts.append(Fact(source, loc, "EndDate", "Day", head.end_date))
                facts.append(Fact(source, loc, attr.rel, attr.obj_t, attr.obj))
        return facts

    headers: list[HeaderLine] = []
    source = f"megalog/{filename}"
    for line_num, raw_line in enumerate(contents.splitlines()):
        if line := parse_line(raw_line):
            match line:
                case HeaderLine() as header:
                    headers = process_header(headers, header)
                    if header.start_date:
                        loc = f"{line_num}:0"
                        facts.append(
                            Fact(source, loc, "Date", "Day", header.start_date)
                        )
                        if header.title:
                            facts.append(
                                Fact(source, loc, "Title", "Title", header.title)
                            )

                case NormalLine(attrs):
                    facts = record_normal(
                        facts, get_day(headers), attrs, filename, line_num
                    )

    return facts


def write_csv(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as CSV")
    with open(filepath, "w") as file:
        writer = csv.writer(file, csv.unix_dialect)
        writer.writerow(("source", "id", "rel", "type", "value"))
        writer.writerows((f.source, f.id, f.rel, f.value_type, f.value) for f in facts)


def write_prolog(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as Prolog")
    contents = (
        f'fact("{f.source}", "{f.id}", "{f.rel}", "{f.value_type}", "{f.value}").'
        for f in facts
    )
    Path(filepath).write_text("\n".join(contents) + "\n")


def write_ntriples(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as N-Triples")
    contents = [
        f"<https://rdf.domson.dev/sources/megalog/{f.source}/{f.id.replace(':', '/')}> "
        f"<https://rdf.domson.dev/predicates/{f.rel}> "
        f'"{f.value}"^^<https://rdf.domson.dev/types/{f.value_type}> .'
        for f in facts
    ]
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

        elif sys.argv[1].endswith(".nt"):
            write_ntriples(sys.argv[1], facts)

        elif sys.argv[1].endswith(".json"):
            write_json(sys.argv[1], facts)

        elif sys.argv[1].endswith(".db"):
            write_sqlite(sys.argv[1], facts)

    else:
        days = set()
        printable_facts = ""
        for fact in facts:
            if fact.id.endswith(":0") and fact.rel == "Date":
                days.add(fact.id)
                printable_facts += f"\n### {fact.value}\n\n"
            if fact.rel != "Date":
                printable_facts += f"{fact.rel:14}"
                printable_facts += f"{fact.value_type}: {fact.value}\n"
        print(f"Mega Log: {len(facts)} facts from {len(days)} days\n")
        print(printable_facts)


if __name__ == "__main__":
    main()
