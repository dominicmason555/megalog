from typing import Optional, Callable
from datetime import datetime
import enum
import dataclasses
import json


@dataclasses.dataclass
class Fact:
    subject: str
    verb: str
    object: str


class FactHandlerJson:
    def __init__(self) -> None:
        self.facts: list[tuple[str, str, str]] = []

    def add_fact(self, fact: Fact) -> None:
        self.facts.append((fact.subject, fact.verb, fact.object))

    def serialise(self) -> str:
        return json.dumps(self.facts)


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


def main():
    print("Hello from megalog!")


if __name__ == "__main__":
    main()
