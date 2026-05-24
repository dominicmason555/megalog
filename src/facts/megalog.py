#!/usr/bin/env python3

import sys
import tomllib
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Callable, Optional

from .fact import (
    Fact,
    write_csv,
    write_json,
    write_ntriples,
    write_prolog,
    write_sqlite,
)

CONF_FILE = "config_megalog.toml"


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
                            title = (
                                line[pos + 13 :].strip()
                                if len(line) > pos + 13
                                else None
                            )
                            return cls(level, start_date, end_date, title)
                    title = line[pos:].strip() if len(line) > pos else None
                    return cls(level, start_date, None, title)
                return cls(level, None, None, None)
            else:
                return cls(level, None, None, None)


def get_to_char(
    line: str, end: str, banned: str
) -> tuple[int, Optional[str], Optional[str], str]:
    pos = 0
    value = ""
    while pos < len(line):
        if line[pos] in banned:
            pos += 1
            return pos, None, None, value + line[pos - 1]
        elif line[pos] in end:
            pos += 1
            if value != "":
                return pos, value, line[pos - 1], value + line[pos - 1]
            return pos, None, None, value + line[pos - 1]
        else:
            if (value != "") or (not line[pos].isspace()):
                value += line[pos]
            pos += 1
    return pos, None, None, value


@dataclass
class ParsedAttr:
    rel: str
    obj_t: str
    obj: str
    subject_found: bool


Chunk = ParsedAttr | str


@dataclass
class NormalParserState:
    pos: int
    key: str
    text: str
    attrs: list[ParsedAttr]
    chunks: list[Chunk]
    subject_found: bool


NormalParserReturn = tuple[Optional["NormalParserFun"], NormalParserState]
NormalParserFun = Callable[[NormalParserState], NormalParserReturn]


# Datemachine
def parse_key_val(line: str) -> tuple[list[ParsedAttr], list[Chunk]]:
    def parse_value(state: NormalParserState) -> NormalParserReturn:
        pos_change, value, ender, text = get_to_char(line[state.pos :], ";]", "[")
        state.pos += pos_change
        if value is not None:
            split = value.split(".")
            obj_t = split[0]
            obj = obj_t
            if len(split) > 1:
                obj = ".".join(s for s in split[1:])
            attr = ParsedAttr(state.key, obj_t, obj, state.subject_found)
            state.attrs.append(attr)
            state.chunks.append(attr)
        # else:
        #     state.text += text
        if ender == ";":
            return parse_value, state
        return parse_outside, state

    def parse_key(state: NormalParserState) -> NormalParserReturn:
        pos_change, key, _, text = get_to_char(line[state.pos :], ":", "[]")
        state.pos += pos_change
        if key is not None:
            if state.text:
                state.chunks.append(state.text)
                state.text = ""
            if len(line) >= state.pos and line[state.pos] == ":":
                state.pos += 1
                state.subject_found = True
            state.key = key
            return parse_value, state
        else:
            state.text += "[" + text
        return parse_outside, state

    def parse_outside(state: NormalParserState) -> NormalParserReturn:
        while state.pos < len(line):
            if line[state.pos] == "[":
                state.pos += 1
                state.subject_found = False
                return parse_key, state
            else:
                state.text += line[state.pos]
            state.pos += 1
        if state.text:
            state.chunks.append(state.text)
        return None, state

    state = NormalParserState(0, "", "", [], [], False)
    parser: Optional[NormalParserFun] = parse_outside
    while parser is not None:
        parser, state = parser(state)
    return state.attrs, state.chunks


@dataclass
class NormalLine:
    attrs: list[ParsedAttr]
    chunks: list[Chunk]

    @classmethod
    def parse(cls, line: str) -> Optional["NormalLine"]:
        if attrs := parse_key_val(line):
            return cls(*attrs)


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
            loc = f"{line_num}/{subject_counter}"
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
                        loc = f"{line_num}/0"
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


def get_facts() -> list[Fact]:
    with open(CONF_FILE, "rb") as tomlfile:
        config = tomllib.load(tomlfile)

    paths: dict[str, str] = config["paths"]
    facts: list[Fact] = []

    for filename, filepath in paths.items():
        contents = Path(filepath).expanduser().read_text()
        facts = parse_file(facts, filename, contents)

    return facts


def main():

    facts = get_facts()

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
            if fact.id.endswith("/0") and fact.rel == "Date":
                days.add(fact.id)
                printable_facts += f"\n### {fact.value}\n\n"
            if fact.rel != "Date":
                printable_facts += f"{fact.rel:14}"
                printable_facts += f"{fact.value_type}: {fact.value}\n"
        print(f"Mega Log: {len(facts)} facts from {len(days)} days\n")
        print(printable_facts)


if __name__ == "__main__":
    main()
