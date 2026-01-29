#!/usr/bin/env python3

import csv
import dataclasses
import enum
import json
import sys
import tomllib
from datetime import datetime
from pathlib import Path
from typing import Callable, Optional

CONF_FILE = "config.toml"


@dataclasses.dataclass
class Fact:
    subject_type: str
    subject: str
    object_type: str
    object: str


def match_date(text: str) -> Optional[str]:
    try:
        datetime.strptime(text, "%Y-%M-%d")
        return text[:10]
    except ValueError:
        return None


@dataclasses.dataclass
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


@dataclasses.dataclass
class NormalParserState:
    pos: int
    key: str
    attrs: list[tuple[str, str]]


NormalParserReturn = tuple[Optional["NormalParserFun"], NormalParserState]
NormalParserFun = Callable[[NormalParserState], NormalParserReturn]


# datemachine
def parse_key_val(line: str) -> list[tuple[str, str]]:
    def parse_value(state: NormalParserState) -> NormalParserReturn:
        pos_change, value = get_to_char(line[state.pos :], "]", "[:")
        state.pos += pos_change
        if value is not None:
            state.attrs.append((state.key.lower(), value.lower()))
        return parse_outside, state

    def parse_key(state: NormalParserState) -> NormalParserReturn:
        pos_change, key = get_to_char(line[state.pos :], ":", "[]")
        state.pos += pos_change
        if key is not None:
            state.key = key
            return parse_value, state
        return parse_outside, state

    def parse_outside(state: NormalParserState) -> NormalParserReturn:
        while state.pos < len(line):
            if line[state.pos] == "[":
                state.pos += 1
                return parse_key, state
            state.pos += 1
        return None, state

    state = NormalParserState(0, "", [])
    parser: Optional[NormalParserFun] = parse_outside
    while parser is not None:
        parser, state = parser(state)
    return state.attrs


@dataclasses.dataclass
class NormalLine:
    attrs: list[tuple[str, str]]

    @classmethod
    def parse(cls, line: str) -> Optional["NormalLine"]:
        if attrs := parse_key_val(line):
            return cls(attrs)


class TaskType(enum.StrEnum):
    TODO = "todo"
    TASK = "task"
    GOAL = "goal"


@dataclasses.dataclass
class TaskLine:
    task_type: TaskType
    attrs: list[tuple[str, str]]


Line = NormalLine | HeaderLine | TaskLine


def parse_line(line: str) -> Optional[Line]:
    if len(line) > 3 and line[:3] == "- [":
        attrs = parse_key_val(line)
        if len(attrs) > 0:
            if attrs[0][0] == "todo":
                return TaskLine(TaskType.TODO, attrs)
            elif attrs[0][0] == "task":
                return TaskLine(TaskType.TASK, attrs)
            elif attrs[0][0] == "goal":
                return TaskLine(TaskType.GOAL, attrs)
        return NormalLine(attrs)
    elif header := HeaderLine.parse(line):
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

    headers: list[HeaderLine] = []
    for line_num, raw_line in enumerate(contents.splitlines()):
        loc = f"{filename}:{line_num + 1}"
        if line := parse_line(raw_line):
            match line:
                case HeaderLine() as header:
                    headers = process_header(headers, header)
                    if header.start_date:
                        facts.append(Fact("line", loc, "day", header.start_date))

                case TaskLine(t_type, attrs):
                    if attrs:
                        name = attrs[0][1]
                        facts.append(Fact("line", loc, f"{t_type.value}created", name))
                        if day := get_day(headers):
                            facts.append(
                                Fact("day", day, f"{t_type.value}created", name)
                            )
                        for attr in attrs[1:]:
                            facts.append(Fact(t_type.value, name, attr[0], attr[1]))

                case NormalLine(attrs):
                    for attr in attrs:
                        facts.append(Fact("line", loc, attr[0], attr[1]))
                        if day := get_day(headers):
                            facts.append(Fact("day", day, attr[0], attr[1]))

    return facts


def write_csv(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as CSV")
    with open(filepath, "w") as file:
        writer = csv.writer(file, csv.unix_dialect)
        writer.writerow(("subject_type", "subject", "object_type", "object"))
        writer.writerows(
            (f.subject_type, f.subject, f.object_type, f.object) for f in facts
        )


def write_prolog(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as Prolog")
    contents = (
        f'fact("{f.subject_type}", "{f.subject}", "{f.object_type}", "{f.object}").'
        for f in facts
    )
    Path(filepath).write_text("\n".join(contents))


def write_json(filepath: str, facts: list[Fact]) -> None:
    print(f"Writing {len(facts)} facts as JSON")
    with open(filepath, "w") as file:
        json.dump(facts, file, default=dataclasses.asdict)


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

    else:
        for fact in facts:
            if fact.subject_type != "line":
                print(
                    f"{fact.subject_type:20}{fact.subject:20}{fact.object_type:20}{fact.object:20}"
                )


if __name__ == "__main__":
    main()
