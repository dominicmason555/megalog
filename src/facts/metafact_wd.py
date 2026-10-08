from datetime import datetime, time, timedelta, timezone

from wikidata.client import Client

from .fact import Fact


def get_facts() -> list[Fact]:
    return []


def main():
    facts = get_facts()
    print(facts)


if __name__ == "__main__":
    main()
