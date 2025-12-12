"""
This module contains various pretty printer for size, diffs, dates, time, etc...
"""

from datetime import date, datetime, timedelta
from typing import Callable

from .display import Display


def pretty_size(
    num: int | float, suffix: str = "B", base: int | float = 1000, sep: str = " "
) -> str:
    """Returns a pretty string representation of size"""
    base = float(base)
    for unit in ("", "k", "M", "G", "T", "P", "E", "Z"):
        if abs(num) < base:
            return f"{num:3.1f}{sep}{unit}{suffix}"
        num /= base
    return f"{num:.1f}{sep}Y{suffix}"


percent_colors = Display.color_scale(
    [(0.0, "green"), (0.33, "white"), (0.66, "yellow"), (1.0, "red")]
)


def colored_percent_usage(num: int | float) -> str:
    """Returns a colored percent utilisation (for CPU and memory)
    0% green -- 33% white -- 66% yellow -- 100% red
    Percent should be a number between 1 and 100"""
    return Display.rgb(percent_colors(num / 100.0)) + str(round(num))


WEEKDAYS = [
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday",
]


def base_relative_date(dt: date) -> str | None:
    """Prints natural language descriptions of date close to today
    Returns None for dates more than a month away"""
    today = date.today()
    if dt == today:
        return "today"
    if dt < today:
        diff = today - dt
        if diff.days == 1:
            return "yesterday"
        elif diff.days < 7:
            return "last " + WEEKDAYS[dt.weekday()]
        elif diff.days < 31:
            return "{} days ago".format(diff.days)
    else:
        diff = dt - today
        if diff.days == 1:
            return "tomorrow"
        elif diff.days < 7:
            return "next " + WEEKDAYS[dt.weekday()]
        elif diff.days < 31:
            return "in {} days".format(diff.days)
    return None


def relative_day(date: date) -> str:
    """Print natural language descriptions of days for dates less than a month away
    Use YYYY-MM-DD format for other dates"""
    relative = base_relative_date(date)
    if relative is None:
        return str(date)
    return relative


def base_relative_time(time: datetime) -> str | None:
    """Prints natural language description of times less than a day away
    None otherwise"""
    now = datetime.now()
    if time <= now:
        diff = now - time
        if diff <= timedelta(seconds=60.0):
            return "just now"
        if diff <= timedelta(seconds=120.0):
            return "1 minute ago"
        if diff <= timedelta(minutes=60.0):
            return "{} minutes ago".format(int(diff.seconds / 60))
        if diff < timedelta(hours=2):
            return "1 hour ago"
        if diff < timedelta(days=1):
            return "{} hours ago".format(int(diff.seconds / 3600))
    else:
        diff = time - now
        if diff <= timedelta(seconds=60.0):
            return "in an instant"
        if diff <= timedelta(seconds=120.0):
            return "in 1 minute"
        if diff <= timedelta(minutes=60.0):
            return "in {} minutes".format(int(diff.seconds / 60))
        if diff < timedelta(hours=2):
            return "in 1 hour"
        if diff < timedelta(days=1):
            return "in {} hours".format(int(diff.seconds / 3600))
    return None


def relative_time(time: datetime, hours_on_relative_days: bool = True) -> str:
    """Print a natural language description of times less than a month away
    if hours_on_relative_days is False, don't print hour information for
    times more than a day away"""
    ft = base_relative_time(time)
    if ft is not None:
        return ft
    ft = relative_day(time.date())
    if hours_on_relative_days:
        return f"{ft} {time.hour:02}:{time.hour:02}"
    return ft


def color_sign(
    string: str, value: int | float, add_plus: bool = True, negative_green: bool = False
) -> str:
    """Color the string green if value > 0, red otherwise"""
    if value > 0 and add_plus:
        string = "+" + string
    if negative_green:
        value *= -1
    if value < 0:
        return "{FG:red}" + string + "{Reset}"
    if value > 0:
        return "{FG:green}" + string + "{Reset}"
    return string


def pretty_diff_base(
    diff: int | float,
    diff_percent: int | None,
    percent: bool = True,
    color: bool = True,
    color_smaller_is_better: bool = False,  # True to color green when smaller
    format: Callable[[float], str] | None = None,
) -> str:
    """Pretty-print a diff (with color and optional percent string)"""
    sign = "+" if diff > 0 else ""
    if format is not None:
        diff_str = format(diff)
    else:
        diff_str = sign + str(diff)
    if percent:
        if diff_percent is not None:
            value = sign + str(diff_percent)
            if value == "0" and diff < 0:
                value = "-" + value
            diff_str += " ({}%)".format(value.rjust(3))
        else:
            diff_str += " (---%)"
    if color:
        return color_sign(
            diff_str, diff, add_plus=False, negative_green=color_smaller_is_better
        )
    return diff_str


def pretty_diff(
    a: int | float,
    b: int | float,
    percent: bool = True,
    color: bool = True,
    color_smaller_is_better: bool = False,  # True to color green when smaller
    format: Callable[[float], str] | None = None,
) -> str:
    """Pretty-print a diff (with color and optional percent string)"""
    diff = b - a
    return pretty_diff_base(
        diff,
        round((diff * 100) / a) if a != 0 else None,
        percent=percent,
        color=color,
        color_smaller_is_better=color_smaller_is_better,
        format=format,
    )
