import json
from dataclasses import dataclass, asdict


@dataclass
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

    def to_json(self):
        return json.dumps(asdict(self))
