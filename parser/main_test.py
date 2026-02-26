from main import HeaderLine, NormalLine, ParsedAttr, parse_line

TEST_DOC = """

# Big header

## Smaller header

### 2026-01-01 The first day

- [ ] A nameless checkbox [ShouldDo: no]
- [x] A nameless but done todo [ShouldDo:: yes]
- [Typo:: Works.Still] [Attrs:: Follow.Tags] [Tags:: Have.Attrs] [Tags: Restart.Attrs] [Attrs:: Follow.LastTag]
- [New: Todo.NameTodos] Add names to todos [Towards:: Goal.MegaLog] [Due:: day.2026-01-02]
- On this ] day, [ I did ] [ things [] [:] with [brackets:].

I [Ate: Lunch.Brackets], delicious.

## 2026-01-01 - 2026-12-31 Year Goals

- [New: Goal.LogEveryDay] Log every day [Done:: Times.0] [Target:: Times.365]
- [New: Goal.MegaLog] Create the MegaLog

## All the time

- [New: Task.WriteTests] Write tests [Recur:: Days.1] for software.

2026-01-01 C'est ne pas un date

"""


def test_HeaderLine():
    assert HeaderLine.parse("# 2026-01-01") == HeaderLine(1, "2026-01-01", None, None)
    assert HeaderLine.parse("# Nonsense") == HeaderLine(1, None, None, None)
    assert HeaderLine.parse("## 2026-01-01 - 2026-nope") == HeaderLine(
        2, "2026-01-01", None, None
    )
    assert HeaderLine.parse("# 2026-01-01 - 2026-01-02") == HeaderLine(
        1, "2026-01-01", "2026-01-02", None
    )
    assert HeaderLine.parse("# 2026-01-32") == HeaderLine(1, None, None, None)
    assert HeaderLine.parse(" 2026-01-01") is None
    assert HeaderLine.parse("2026-01-01") is None


def test_NormalLine():
    assert NormalLine.parse("Text [attr: maybe] more text") == NormalLine(
        [
            ParsedAttr(
                "attr",
                "maybe",
                False,
            )
        ]
    )
    assert NormalLine.parse("Text [tag: maybe] more text [attr:: maybe]") == NormalLine(
        [ParsedAttr("tag", "maybe", False), ParsedAttr("attr", "maybe", True)]
    )
    assert NormalLine.parse(
        "Text [attr: maybe] more [] [attr:] [attr] [: maybe] [attr: may:be] text [attr:   maybe  ]"
    ) == NormalLine(
        [ParsedAttr("attr", "maybe", False), ParsedAttr("attr", "maybe", False)]
    )


def test_parse_line():
    lines = TEST_DOC.splitlines()
    assert parse_line(lines[2]) == HeaderLine(1, None, None, None)
    assert parse_line(lines[6]) == HeaderLine(3, "2026-01-01", None, None)
    assert parse_line(lines[8]) == NormalLine([ParsedAttr("shoulddo", "no", False)])
    assert parse_line(lines[9]) == NormalLine([ParsedAttr("shoulddo", "yes", True)])
    assert parse_line(lines[14]) == NormalLine(
        [ParsedAttr("ate", "lunch.brackets", False)]
    )
    assert parse_line(lines[24]) is None
