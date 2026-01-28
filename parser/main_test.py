from main import HeaderLine, NormalLine, parse_line, TaskLine, TaskType

TEST_DOC = """

# Big header

## Smaller header

### 2026-01-01 The first day

- [ ] A nameless todo [ShouldDo: no]
- [x] A nameless but done todo [ShouldDo: yes]
- [Todo: NameTodos] Add names to todos [Goal: MegaLog] [Due: 2026-01-02]
- On this ] day, [ I did ] [ things [] with [brackets].

I ate [Lunch: Brackets], delicious.

## 2026-01-01 - 2026-12-31 Year Goals

- [Goal: LogEveryDay] Log every day [Done: 0] [Target: 365]
- [Goal: MegaLog] Create the MegaLog

## All the time

- [Task: WriteTests] Write tests [Recur: 1-day] for software.

2026-01-01 C'est ne pas un date

"""


def test_HeaderLine():
    assert HeaderLine.parse("# 2026-01-01") == HeaderLine(1, "2026-01-01", None)
    assert HeaderLine.parse("# Nonsense") == HeaderLine(1, None, None)
    assert HeaderLine.parse("## 2026-01-01 - 2026-nope") == HeaderLine(
        2, "2026-01-01", None
    )
    assert HeaderLine.parse("# 2026-01-01 - 2026-01-02") == HeaderLine(
        1, "2026-01-01", "2026-01-02"
    )
    assert HeaderLine.parse("# 2026-01-32") == HeaderLine(1, None, None)
    assert HeaderLine.parse(" 2026-01-01") is None
    assert HeaderLine.parse("2026-01-01") is None


def test_NormalLine():
    assert NormalLine.parse("Text [attr: maybe] more text") == NormalLine(
        [("attr", "maybe")]
    )
    assert NormalLine.parse("Text [attr: maybe] more text [attr: maybe]") == NormalLine(
        [("attr", "maybe"), ("attr", "maybe")]
    )
    assert NormalLine.parse(
        "Text [attr: maybe] more [] [attr:] [attr] [: maybe] [attr: may:be] text [attr:   maybe  ]"
    ) == NormalLine([("attr", "maybe"), ("attr", "maybe")])


def test_parse_line():
    lines = TEST_DOC.splitlines()
    assert parse_line(lines[2]) == HeaderLine(1, None, None)
    assert parse_line(lines[6]) == HeaderLine(3, "2026-01-01", None)
    assert parse_line(lines[8]) == NormalLine([("shoulddo", "no")])
    assert parse_line(lines[9]) == NormalLine([("shoulddo", "yes")])
    assert parse_line(lines[10]) == TaskLine(
        TaskType.TODO,
        [("todo", "nametodos"), ("goal", "megalog"), ("due", "2026-01-02")],
    )
    assert parse_line(lines[13]) == NormalLine([("lunch", "brackets")])
    assert parse_line(lines[24]) is None
