import os
import sys
from pathlib import Path
from typing import Optional

FACTS_PATH_ENV = "WELLKNOWN_SYNC_PERSONAL"
FACTS_PATH = "facts"


def get_facts_path() -> Optional[Path]:
    if parent := os.getenv(FACTS_PATH_ENV):
        facts_dir = Path(parent) / FACTS_PATH
        if facts_dir.is_dir():
            return facts_dir
        else:
            print(f"ERROR: Cannot find {facts_dir}", file=sys.stderr)
    else:
        print(f"ERROR: Cannot find {FACTS_PATH_ENV}", file=sys.stderr)
