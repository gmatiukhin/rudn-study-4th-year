import json
from enum import StrEnum, auto
import dataclasses
from itertools import product


class EnhancedJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if dataclasses.is_dataclass(o):
            return dataclasses.asdict(o)  # pyright: ignore
        return super().default(o)


class Type(StrEnum):
    Type1 = auto()
    Type2 = auto()


class Role(StrEnum):
    Ddns = auto()
    Dns = auto()
    Gateway = auto()
    Router = auto()


@dataclasses.dataclass
class Pc:
    # Yeah, yeah
    type: Type
    role: Role


if __name__ == "__main__":
    for c in list(product(Type, Role)):
        with open(f"items/pc_{c[0]}_{c[1]}.json", "w") as f:
            json.dump(Pc(c[0], c[1]), f, indent=2, cls=EnhancedJSONEncoder)
