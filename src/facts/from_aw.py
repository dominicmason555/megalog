from requests.exceptions import ConnectionError
from urllib3.exceptions import MaxRetryError, NewConnectionError
from datetime import datetime, time, timedelta, timezone
import socket

import aw_client

from .fact import Fact

DAY_START = 4  # 4 AM
FIRST_DATE = datetime(2026, 1, 1)


def get_facts() -> list[Fact]:
    facts = []
    hostname = socket.gethostname()
    bucket_id = f"aw-watcher-afk_{hostname}"

    today = datetime.combine(datetime.now(), time(DAY_START)).astimezone(timezone.utc)
    date = datetime.combine(FIRST_DATE, time(DAY_START)).astimezone(timezone.utc)

    try:
        awc = aw_client.ActivityWatchClient("aw_facts")

        while date <= today:
            daystart = datetime.combine(date, time(DAY_START)).astimezone(timezone.utc)
            dayend = daystart + timedelta(days=1)

            events = awc.get_events(bucket_id, start=daystart, end=dayend)
            events = [e for e in events if e.data["status"] == "not-afk"]
            total_duration = sum((e.duration for e in events), timedelta())
            if total_duration.total_seconds() > 0:
                id = daystart.isoformat()[:10]
                split = str(total_duration).split(":")
                facts.append(
                    Fact(
                        f"aw/{hostname}",
                        id,
                        "NotAFK",
                        "Duration",
                        f"{split[0]}h{split[1]}m",
                    )
                )
                facts.append(
                    Fact(
                        f"aw/{hostname}",
                        id,
                        "Date",
                        "Day",
                        f"{daystart.isoformat()[:10]}",
                    )
                )
            date += timedelta(days=1)
    except (NewConnectionError, ConnectionError, MaxRetryError, ConnectionRefusedError):
        print("ERROR: failed to connect to ActivityWatch, skipping")

    return facts


def main():
    facts = get_facts()

    print("\n".join([f.to_prolog() for f in facts]))


if __name__ == "__main__":
    main()
