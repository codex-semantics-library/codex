"""
Compute statistical indicator for lists of numbers
"""

from math import sqrt
from typing import Callable, Generic, TypeVar

from .display import Display

N = TypeVar("N", int, float)


class Stats(Generic[N]):
    """Compute various statistical indicators for a list of numbers"""

    size: int
    sum: N
    sum_squares: N
    non_zero: int

    # These are only defined if size > 0
    max: N | None
    min: N | None
    average: float | None
    median: N | float | None
    variance: float | None
    std_deviation: float | None
    deviation_percent: int | None  # standard deviation as a percent of the mean

    # These are only defined if size >= 3
    q1: N | float | None
    q3: N | float | None

    def __init__(self, items: list[N]) -> None:
        self.size = len(items)
        self.sum = sum(items)
        self.sum_squares = sum(x * x for x in items)
        self.non_zero = len(list(x for x in items if x != 0))
        if self.size > 0:
            self.average = self.sum / self.size
            self.variance = (self.sum_squares / self.size) - (
                self.average * self.average
            )
            self.std_deviation = sqrt(self.variance)
            if self.average != 0:
                self.deviation_percent = int(self.std_deviation * 100.0 / self.average)
            else:
                self.deviation_percent = None
            sorted_items = sorted(items)
            self.min = sorted_items[0]
            self.max = sorted_items[-1]
            self.median, median_indices = self.find_median(sorted_items)
            if self.size >= 3:
                self.q1, _ = self.find_median(sorted_items[: median_indices[0]])
                self.q3, _ = self.find_median(sorted_items[median_indices[-1] + 1 :])
            else:
                self.q1 = self.median
                self.q3 = self.median
        else:
            self.min = None
            self.max = None
            self.average = None
            self.variance = None
            self.std_deviation = None
            self.deviation_percent = None
            self.median = None
            self.q1 = None
            self.q3 = None

    def find_median(self, sorted_list: list[N]) -> tuple[N | float, list[int]]:
        indices = []
        list_size = len(sorted_list)
        half = list_size // 2
        if list_size % 2 == 0:
            indices.append(half - 1)  # -1 because index starts from 0
            indices.append(half)
            return (sorted_list[indices[0]] + sorted_list[indices[1]]) / 2, indices
        indices.append(half)
        return sorted_list[indices[0]], indices

    def pretty_average(
        self,
        formatter: Callable[[float], str] = str,
        color_percent: bool = True,
        plus_minus_symbol: str | None = None,
    ) -> str | None:
        if self.size <= 0:
            return None
        if self.average is None or self.deviation_percent is None:
            return None
        color = ""
        if color_percent:
            if self.deviation_percent < 1:
                color = Display.grayscale(5)
            elif self.deviation_percent < 5:
                color = Display.grayscale(6)
            elif self.deviation_percent < 10:
                color = Display.grayscale(7)
            elif self.deviation_percent < 15:
                color = Display.grayscale(9)
            elif self.deviation_percent < 20:
                color = Display.grayscale(11)
            elif self.deviation_percent < 25:
                color = Display.grayscale(13)
            elif self.deviation_percent < 50:
                color = Display.grayscale(15)
            elif self.deviation_percent < 75:
                color = ""
            elif self.deviation_percent < 100:
                color = "{FgYellow}"
            else:
                color = "{FgRed}"
        return "{}{}{}{}%{{Reset}}".format(
            formatter(self.average),
            color,
            "{UPlusMinus}" if plus_minus_symbol is None else plus_minus_symbol,
            self.deviation_percent,
        )

    def summary(self, formatter: Callable[[N], str] = str) -> str:
        """return a pretty summary of the stats information"""
        return """ """


class DiffStats(Generic[N]):
    """Various stats for diffs between two lists of numbers"""

    diff: Stats[N]  # Stats for the diffs 'b - a'
    diff_normalized: Stats[float]  # Stats for the normalized diff '(b - a) / a'
    positive: Stats[N]  # Stats for positive diffs 'b - a when > 0'
    negative: Stats[N]  # Stats for the negative diffs 'b - a when < 0'

    def __init__(self, a: list[N], b: list[N]) -> None:
        assert len(a) == len(b)
        diffs = [b[i] - a[i] for i in range(len(a))]
        self.diff = Stats(diffs)
        self.diff_normalized = Stats(
            [diffs[i] / a[i] for i in range(len(a)) if a[i] != 0]
        )
        self.positive = Stats([x for x in diffs if x > 0])
        self.negative = Stats([x for x in diffs if x < 0])
