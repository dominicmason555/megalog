from dataclasses import dataclass
from datetime import datetime, time, timedelta, timezone
import sqlite3
import socket

import aw_client

DAY_START = 4  # 4 AM
NUM_DAYS = 30


CREATE_TABLE = """CREATE TABLE IF NOT EXISTS """


@dataclass()
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


def main():
    bucket_id = f"aw-watcher-afk_{socket.gethostname()}"

    daystart = datetime.combine(datetime.now().date(), time(DAY_START)).astimezone(
        timezone.utc
    )
    dayend = daystart + timedelta(days=1)

    awc = aw_client.ActivityWatchClient("aw_facts")
    events = awc.get_events(bucket_id, start=daystart, end=dayend)
    events = [e for e in events if e.data["status"] == "not-afk"]
    total_duration = sum((e.duration for e in events), timedelta())
    print(f"Total time spent on computer today: {total_duration}")


if __name__ == "__main__":
    main()
