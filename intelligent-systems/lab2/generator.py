import json
from enum import StrEnum, auto
import dataclasses
from itertools import product
import os


class EnhancedJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if dataclasses.is_dataclass(o):
            return dataclasses.asdict(o)  # pyright: ignore
        return super().default(o)


class AutoNameEnum(StrEnum):
    @staticmethod
    def _generate_next_value_(name, start, count, last_values) -> str:
        return name


class Type(AutoNameEnum):
    Type1 = auto()
    Type2 = auto()


class Role(AutoNameEnum):
    Ddns = "Ddns"
    Dns = auto()
    Gateway = auto()
    Router = auto()


@dataclasses.dataclass
class Pc:
    # Yeah, yeah
    type: Type
    role: Role


if __name__ == "__main__":
    os.makedirs("items", exist_ok=True)
    for c in list(product(Type, Role)):
        with open(f"items/pc_{c[0]}_{c[1]}.json".lower(), "w") as f:
            pc = Pc(c[0], c[1])
            json.dump(pc, f, indent=2, cls=EnhancedJSONEncoder)
