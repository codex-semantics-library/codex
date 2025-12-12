#!/usr/bin/env python3
# PYTHON_ARGCOMPLETE_OK
# Note: CLI autocomplete requires either running:
# - eval "$(register-python-argcomplete manage.py)" (Just for this script)
# - activate-global-python-argcomplete (for all python)

# This script is designed to help manage running/saving the codex analysis
# and processing its results.
# See "./manage.py --help" for an overview of what it can do. Short summary below
#
# RUNNING ANALYSES
# - ./manage.py list-available -> list all detected analysis
# - ./manage.py run [-j N] [--flags EXTRA_FLAGS] -> run all analyses, on N cores
#            passing EXTRA_FLAGS to codex (be sure to escape them: ' -my-flag')
#
# SAVING RESULTS
# - ./manage.py save -> create a named saved of a run result
# - ./manage.pu list-saves -> show all named saves
# - ./manage.py rm-save SAVE -> delete a named save
#
# INFO ABOUT ALL ANALYSES
# - ./manage.py [-s SAVE] list-ok [-c COLUMNS] -> various stats about the runs that succeeded
#             (in working folder by default, in the given save if provided)
#             Set displayed columns with -c: default is "-c loc -c time -c mem -c alarms -c assets"
# - ./manage.py [-s SAVE] list-error -> show the runs that failed, along with a reason
# - ./manage.py [-fp] compare SAVE1 SAVE2 -> compare both run results
#             with -f/--filter: only show files whose dumps have are not equal
#             with -p/--print: show the dump diff
#
# INFO ABOUT A SINGLE ANALYSIS
# - ./manage.py [-s SAVE] output FILE_FILTER -> show dump file for all analysis matching
#             FILE_FILTER (ex 'foo' matches any filename containing foo)
# - ./manage.py [-s SAVE] log FILE_FILTER -> show log file for all analysis matching
#             FILE_FILTER (ex 'foo' matches any filename containing foo)
# - ./manage.py [-s SAVE] stats FILE_FILTER -> show analysis stats for given file
#             see './manage.py list-stats' for description of stats


from argparse import ArgumentParser
from datetime import datetime
from json import dump, load, loads
from os import chdir
from pathlib import Path
from textwrap import wrap
from typing import Any, Callable
from shutil import get_terminal_size
from subprocess import check_output, Popen, STDOUT, PIPE
from shutil import move, rmtree
from re import match, search, IGNORECASE

from pycli.formatters import pretty_size, pretty_diff, color_sign
from pycli.display import Display, pprint
from pycli.table import Table, TableValue
from pycli.parser import make_parser, ALL, DIFF
from pycli.stats import DiffStats, Stats

cli_autocomplete: Callable[[ArgumentParser], Any] | None = None
try:
    from argcomplete import autocomplete

    cli_autocomplete = autocomplete
except ImportError:
    pass

EXTRA_FLAGS = " -codex-no-try-hard -codex-gc-stats -codex-extra-metrics"

DOCKER = Path("/.dockerenv").exists()

if DOCKER:  # Running inside the docker container
    FRAMA_C_CODEX = "~/frama_c_codex"
    SVCOMP_PATH = "~/sv-comp"
else:
    FRAMA_C_CODEX = "frama-c -codex"
    SVCOMP_PATH = "/opt/git/sv-benchmarks"

root_dir = Path(__file__).parent.resolve()
backups_dir = root_dir / ".saves"  # Folder where previous runs are saved
backups_dir.mkdir(exist_ok=True)
config_file = root_dir / ".manage-py-config.json"  # Save script data between calls

SVCOMP_BENCHMARKS = "sv-benchmarks4codex"
WHOLE_PROGRAM_BENCHMARKS = "whole-program"
OPEN_SOURCE_CASE_STUDIES = "open-source-case-studies"
CSMITH_BENCHMARKS = "csmith"

svcomp_dir = root_dir / SVCOMP_BENCHMARKS

# Try to read JSON config file, else load empty config
if config_file.is_file():
    try:
        with open(config_file, "r") as file:
            config = load(file)
    except Exception:
        config = dict()
else:
    config = dict()

CONFIG_SAVES = "saves"
CONFIG_SELECTION = "current_save"
CONFIG_CLOC = "cloc"  ## Dict filename -> {"loc": cloc, "mtime": f) to avoid having to recompute them

# See ./manage.py list-stats of the STATS variable for possible values
DEFAULT_COLUMNS = ["loc", "mem", "time", "alarms", "asserts"]

# Same as for DEFAULT_COLUMNS, but also accepts stats prefixed by "1." or "2.":
# no prefix -> diff between both stats
# with prefix -> only show the stats for the given save
# Also accepts a special "diff" stat, number of lines that differ in the dump file (excluding summary)
DEFAULT_DIFF_COLUMNS = ["1.loc", "mem", "time", "alarms", "asserts", DIFF, "2.cp_ratio", "gc_major_words"]


class Stat[T]:
    """A single stat about a codex run, along with associated pretty printers"""

    name: str  # The name/id of the stat
    help: str  # a short documentation
    value: T | None
    negative_green: bool = True
    pretty_print: bool = True # enable/disable pretty printing

    def __init__(self, tgt: "Target | None" = None) -> None:
        if tgt is None:
            self.value = None
        else:
            self.value = tgt.raw_stats().get(self.name)

    def __str__(self) -> str:
        return str(self.value)

    def pretty(self) -> str:
        if not self.pretty_print:
            return str(self.value)
        if self.value is None:
            return "--"
        if isinstance(self.value, float):
            return str(round(self.value, 2))
        return str(self.value)

    def pretty_diff(self, other: "Stat[T]") -> str:
        if self.value is None or other.value is None:
            if not self.pretty_print:
                return "None"
            return "--"
        if (isinstance(self.value, float) or isinstance(self.value, int)) and (
            isinstance(other.value, float) or isinstance(other.value, int)
        ):
            if not self.pretty_print:
                return str(other.value - self.value)
            return color_sign(
                str(other.value - self.value), other.value - self.value, negative_green=self.negative_green
            )
        return str(self.value)

    @classmethod
    def from_value(cls, v: T | None) -> "Stat[T]":
        self = cls()
        self.value = v
        return self


def mk_stat[T](
    sname: str,
    shelp: str,
    init: Callable[["Target"], T | None] | None = None,
    pretty: Callable[[T], str] | None = None,
    pretty_diff: Callable[[T, T], str] | None = None,
    neg_green: bool = True,
) -> type[Stat[T]]:
    class S(Stat[T]):
        name = sname
        help = shelp
        negative_green = neg_green

        def __init__(self, tgt: "Target|None" = None) -> None:
            if init is not None and tgt is not None:
                self.value = init(tgt)
            else:
                super().__init__(tgt)

        def pretty(self) -> str:
            if self.pretty_print and pretty is not None and self.value is not None:
                return pretty(self.value)
            return super().pretty()

        def pretty_diff(self, other: Stat[T]) -> str:
            if self.pretty_print and pretty_diff is not None and self.value is not None and other.value is not None:
                return pretty_diff(self.value, other.value)
            return super().pretty_diff(other)

    return S


def combine_stats(stat_a: str, stat_b: str, combine: Callable[[Any, Any], Any]):
    def ret(tgt: "Target"):
        sa = tgt.get_stat(stat_a).value
        sb = tgt.get_stat(stat_b).value
        if sa is None or sb is None:
            return None
        return combine(sa, sb)

    return ret


def pretty_tuple(x: tuple[int, int] | int) -> str:
    if isinstance(x, int):
        return str(x)
    if isinstance(x, float):
        return str(round(x, 2))
    return (f"{{FG:Red}}{x[0]}{{Reset}}" if x[0] != 0 else str(x[0])) + "/" + str(x[1])


# Each stat has a unique name, so it can be selected in "list-ok" and "compare"
# It also comes with its own pretty printers
STATS: dict[str, type[Stat[Any]]] = {
    # Read from stats.txt file
    "time": mk_stat(
        "user_time",
        "from 'time %U', total number of CPU-seconds that the process used directly (in user mode), in seconds.",
        pretty_diff=lambda x, y: pretty_diff(
            x,
            y,
            format=lambda x: str(round(x, 2)),
            color_smaller_is_better=True,
        ),
    ),
    "mem": mk_stat(
        "memory",
        "from 'time %M', maximum resident set size of the process during its lifetime, in Kilobytes.",
        pretty=pretty_size,
        pretty_diff=lambda x, y: pretty_diff(
            x,
            y,
            format=pretty_size,
            color_smaller_is_better=True,
        ),
    ),
    "cmd": mk_stat("cmd_args", "The command ran for this analysis, with all arguments"),
    "timestamp": mk_stat("timestamp", "A timestamp of when the analysis was run, YYYY-MM-DD HH:mm:ss"),
    # Alarms
    "alarms_proved": mk_stat(
        "alarms_proved",
        "Number of regular alarms proved by Codex (Proved regular alarms)",
    ),
    "alarms_additional": mk_stat("alarms_additional", "Number of additional (unproved) alarms by Codex"),
    "alarms_total_regular": mk_stat(
        "alarms_total_regular",
        "Total number (proved+unproved) of alarms (excluding additional alarms)",
    ),
    "alarms_total": mk_stat(
        "total_alarms",
        "Total number (proved+unproved) of alarms",
        init=combine_stats("alarms_total_regular", "alarms_additional", lambda x, y: x + y),
    ),
    "alarms_unproved": mk_stat(
        "alarms_unproved",
        "Number of unproved alarms (regular+additional)",
        init=combine_stats("alarms_total", "alarms_proved", lambda x, y: x - y),
    ),
    "alarms": mk_stat(
        "alarms",
        "Ratio unproved/total alarms",
        init=combine_stats("alarms_unproved", "alarms_total", lambda x, y: (x, y)),
        pretty=pretty_tuple,
        pretty_diff=lambda a, b: pretty_diff(
            a[0],
            b[0],
            percent=False,
            color_smaller_is_better=True,
        )
        + "/"
        + str(a[1]),
    ),
    # Asserts
    "asserts_proved": mk_stat(
        "asserts_proved",
        "Number of assertions proved to be true",
    ),
    "asserts_solved": mk_stat(
        "asserts_solved",
        "Number of assertions solved (proved either true or false)",
    ),
    "asserts_total": mk_stat(
        "asserts_total",
        "Total number of assertion (solved + unsolved).",
    ),
    "asserts_unsolved": mk_stat(
        "asserts_unsolved",
        "Number of unsolved assertions (value is not known)",
        init=combine_stats("asserts_solved", "asserts_total", lambda x, y: y - x),
    ),
    "asserts": mk_stat(
        "asserts",
        "Ratio unsolved/total assertion",
        init=combine_stats("asserts_solved", "asserts_total", lambda x, y: (y - x, y)),
        pretty=pretty_tuple,
        pretty_diff=lambda a, b: pretty_diff(
            a[0],
            b[0],
            percent=False,
            color_smaller_is_better=True,
        )
        + "/"
        + str(a[1]),
    ),
    # Lines with values
    "dump_loc": mk_stat(
        "dump_loc",
        "Number of unique lines appearing in Codex Dump files with a value. "
        "Eg. number of unique 'i' such that 'filename.c:i: ...' appears in dump file",
    ),
    "dump_loc_range": mk_stat("dump_loc_range", "Range (Max - Min) of loc appearing in dump file"),
    "dump_coverage": mk_stat(
        "dump_coverage",
        "Ratio dump_loc*100 / loc",
        pretty=lambda x: f"{round(x, 2)}%",
        init=combine_stats("dump_loc", "loc", lambda x, y: (x * 100 / y)),
    ),
    # GC Stats
    "gc_minor_collections": mk_stat("gc_minor_collections", "Ocaml GC stat, number of minor collections performed"),
    "gc_major_collections": mk_stat("gc_major_collections", "Ocaml GC stat, number of major collections performed"),
    "gc_compactions": mk_stat("gc_compactions", "Ocaml GC stat, number of heap compaction performed"),
    "gc_forced_major_collections": mk_stat(
        "gc_forced_major_collections",
        "Ocaml GC stat, number of forced full major collections",
    ),
    "gc_minor_words": mk_stat("gc_minor_words", "Ocaml GC stat, number of words allocated to minor heap"),
    "gc_promoted_words": mk_stat(
        "gc_promoted_words",
        "Ocaml GC stat, Number of words allocated in the minor heap that survived a minor collection and were moved to the major heap",
    ),
    "gc_major_words": mk_stat(
        "gc_major_words",
        "Ocaml GC stat, number of words allocated to major heap",
        pretty=lambda x: pretty_size(x, suffix="W"),
        pretty_diff=lambda x, y: pretty_diff(
            x,
            y,
            format=lambda y: pretty_size(y, suffix="W"),
            color_smaller_is_better=True,
        ),
    ),
    "gc_top_heap_words": mk_stat(
        "gc_top_heap_words",
        "Ocaml GC stat, maximum size reached by major heap, in words",
    ),
    "gc_heap_words": mk_stat("gc_heap_words", "Ocaml GC stat, size of major heap, in words"),
    # "gc_live_words": mk_stat("gc_live_words", "Ocaml GC stat, see https://ocaml.org/manual/5.2/api/Gc.html"),
    # "gc_free_words": mk_stat("gc_free_words", "Ocaml GC stat, see https://ocaml.org/manual/5.2/api/Gc.html"),
    # "gc_largest_free": mk_stat("gc_largest_free", "Ocaml GC stat, see https://ocaml.org/manual/5.2/api/Gc.html"),
    # "gc_fragments": mk_stat("gc_fragments", "Ocaml GC stat, see https://ocaml.org/manual/5.2/api/Gc.html"),
    # "gc_live_blocks": mk_stat("gc_live_blocks", "Ocaml GC stat, see https://ocaml.org/manual/5.2/api/Gc.html"),
    # "gc_free_blocks": mk_stat("gc_free_blocks", "Ocaml GC stat, see https://ocaml.org/manual/5.2/api/Gc.html"),
    # "gc_heap_chunks": mk_stat("gc_heap_chunks", "Ocaml GC stat, see https://ocaml.org/manual/5.2/api/Gc.html"),
    # Union-find stats
    "uf_unions": mk_stat("uf_unions", "Number of labeled union-find unions performed by codex"),
    "binary_terms": mk_stat("binary_terms", "Number of binary terms created"),
    "uf_max_cluster_size": mk_stat("uf_max_cluster_size", "Number of elements in largest union-find class"),
    "uf_ratio": mk_stat(
        "uf_ratio",
        "Ratio uf_unions * 100 / binary_terms",
        pretty=lambda x: f"{round(x, 2)}%",
        init=combine_stats("uf_unions", "binary_terms", lambda x, y: (x * 100 / y)),
    ),
    # Compacting stats
    "cp_size": mk_stat("cp_size", "Compressed size of numeric values, in number of reachable words"),
    "cp_usize": mk_stat("cp_usize", "Uncompressed size of numeric values, in number of reachable words"),
    "cp_nb": mk_stat("cp_nb", "Number of calls to compress"),
    "cp_ratio": mk_stat(
        "cp_ratio",
        "Ratio cp_size / cp_usize",
        pretty=lambda x: f"{round(x, 2)}%",
        init=combine_stats("cp_size", "cp_usize", lambda x, y: ((y - x) * 100 / y if y != 0 else None)),
    ),
    # Other
    "loc": mk_stat(
        "loc",
        "Lines of code in the source file, excluding comments and blanks lines, as reported by cloc.",
        init=lambda x: x.cloc(),
    ),
    "path": mk_stat("path", "Path of the C file", init=lambda x: x.source()),
    "path_dump": mk_stat("path_dump", "Path of the dump file", init=lambda x: x.dump_file()),
    "path_stats": mk_stat("path_stats", "Path of the stats file", init=lambda x: x.stats_file()),
    "path_log": mk_stat("path_log", "Path of the log file", init=lambda x: x.log_file()),
}

STAT_KEYS = [k for k in STATS]
STAT_KEYS.sort()
GC_STATS = [x[3:] for x in STAT_KEYS if x.startswith("gc_")]


# Regexp detecting a line of the dump file that prints a value
DUMP_LINE_REGEXP = r"^\S+\.(c|i)\:(\d+)\.\d+\-(\d+\.)?\d+: `"


class Target:
    """A target is a single analysis result"""

    path: Path
    root: Path

    def __init__(self, path: Path, root: Path) -> None:
        posix = path.as_posix()
        self.root = root
        if posix.endswith(".log"):
            # strip .log from self.path, creating a fake path containing just the prefix
            self.path = Path(posix[:-4])
        else:
            self.path = path

    def stats_file(self) -> Path:
        """Get the path to the stats file"""
        # For some bizzare reason, python doesn't easily let you append to paths
        return Path(f"{self.path}.stats.txt")

    def dump_file(self) -> Path:
        """Get the path to the dump file"""
        return Path(f"{self.path}.main.cdump")

    def log_file(self) -> Path:
        """Get the path to the log file"""
        return Path(f"{self.path}.log")

    def succeeded(self) -> bool:
        """Check if the analysis was successful
        (i.e. that codex didn't crash, not that all goals where proved)"""
        # The stats file is removed if codex does not return 0
        return self.stats_file().exists()

    def __str__(self) -> str:
        return str(self.path.relative_to(root_dir))

    _raw_stats: dict[str, Any] | None = None

    def raw_stats(self) -> dict[str, Any]:
        """Parse the stats.txt files"""
        if self._raw_stats is not None:
            return self._raw_stats
        with open(self.stats_file(), "r") as file:
            content = file.read()
        stats: dict[str, Any] = dict()
        for line in content.split("\n"):
            parts = line.split("=")
            if len(parts) > 1:
                value: Any = "=".join(parts[1:])
                stats[parts[0]] = value
        if "memory" in stats:
            stats["memory"] = int(stats["memory"])
        if "user_time" in stats:
            stats["user_time"] = float(stats["user_time"])

        # stats.update(self.get_eva_metrics(target))
        # if "eva_analyzed_stmts" in stats and "dump_locs" in stats:
        #     stats["eva_codex_percent"] = round(
        #         stats["dump_locs"] * 100 / stats["eva_analyzed_stmts"], 2
        #     )
        stats.update(self.get_codex_metrics())
        self._raw_stats = stats
        return stats

    def get_codex_metrics(self) -> dict[str, Any]:
        """Reads extra metrics directly from the dump"""
        dump = self.dump_file()
        stats: dict[str, Any] = dict()
        if dump.is_file():
            with open(dump, "r") as file:
                contents = file.read()
            # To avoid regex parsing the full dump, split it into lines/summary
            contents_split = contents.split("Unproved regular alarms:\n")
            lines_info = contents_split[0]
            summary = "\n".join(contents_split[1:])
            found = search(r"Proved (\d+)/(\d+) regular alarms", summary)
            if found:
                stats["alarms_proved"] = int(found[1])
                stats["alarms_total_regular"] = int(found[2])
            found = search(r"Unproved (\d+) regular alarms and (\d+) additional alarms.", summary)
            if found:
                stats["alarms_additional"] = int(found[1])
            found = search(r"UF Unions performed: (\d+)", summary)
            if found:
                stats["uf_unions"] = int(found[1])
            found = search(r"UF Binary terms: (\d+)", summary)
            if found:
                stats["binary_terms"] = int(found[1])
            found = search(r"UF Max Clusters: (\d+)", summary)
            if found:
                stats["uf_max_cluster_size"] = int(found[1])
            found = search(r"Solved (\d+)/(\d+) user assertions, proved (\d+)", summary)
            if found:
                stats["asserts_solved"] = int(found[1])
                stats["asserts_proved"] = int(found[3])
                stats["asserts_total"] = int(found[2])
            found = search(r"Compressed size: (\d+)", summary)
            if found:
                stats["cp_size"] = int(found[1])
            found = search(r"Uncompressed size: (\d+)", summary)
            if found:
                stats["cp_usize"] = int(found[1])
            found = search(r"Compress calls: (\d+)", summary)
            if found:
                stats["cp_nb"] = int(found[1])
            lines = set()
            for line in lines_info.split("\n"):
                m = match(DUMP_LINE_REGEXP, line)
                if m is not None:
                    lines.add(int(m[2]))
            stats["dump_loc"] = len(lines)
            try:
                stats["dump_loc_range"] = max(lines) - min(lines)
            except ValueError:
                stats["dump_loc_range"] = 0
            for stat in GC_STATS:
                found = search(stat + r"\:\s+(\d+)", summary)
                if found:
                    stats["gc_" + stat] = int(found[1])
        return stats

    def fail_reason(self) -> str:
        # This doesn't work for svcomp...
        with open(self.log_file(), "r") as file:
            contents = file.read()
        for pattern in (
            "Uncaught exception: .*",
            "Fatal error: .*",
            "Error: .*",
        ):
            res = search(pattern, contents, flags=IGNORECASE)
            if res is not None:
                return res[0]
        return "unknown"

    def __repr__(self) -> str:
        return f"Target({str(self)})"

    def source(self) -> Path | None:
        """Return the path to the target's C source file"""
        partial_path = self.path.relative_to(self.root)
        for path in [
            root_dir / partial_path,
            Path(SVCOMP_PATH) / "c" / str(partial_path).replace(SVCOMP_BENCHMARKS + "/", ""),
        ]:
            if path.is_file():
                return path
        return None

    def cloc(self) -> int | None:
        """Return number of lines of code, as measured by cloc"""
        source_path = self.source()
        if source_path is None:
            return None
        mtime = source_path.stat().st_mtime
        source = str(source_path)
        if CONFIG_CLOC in config and source in config[CONFIG_CLOC]:
            memorized = config[CONFIG_CLOC][source]
            if isinstance(memorized, dict) and memorized["mtime"] >= mtime:
                return memorized["loc"]

        result = None
        output = check_output(["cloc", "--json", source]).decode().strip()
        joutput = loads(output)  # decode json
        if isinstance(joutput, dict):
            result = joutput.get("SUM", dict()).get("code", None)

        if CONFIG_CLOC not in config:
            config[CONFIG_CLOC] = dict()
        if result is not None:
            config[CONFIG_CLOC][source] = {"loc": result, "mtime": mtime}
        return result

    def get_stat(self, stat: str) -> Stat[Any]:
        """Return the given named stat"""
        return STATS[stat](self)


class AnalysisResults:
    """Collects all targets from a current run"""

    root: Path
    save: str
    results: list[Target]
    results_ok: list[Target]
    results_err: list[Target]

    def __init__(self, root=Path, save: str | None = None, read: bool = True):
        self.root = root
        if save is None:
            self.save = "."
        else:
            self.save = save
        if read:
            targets = []
            targets += list((root / SVCOMP_BENCHMARKS).glob("*/*.log"))
            targets += list((root / WHOLE_PROGRAM_BENCHMARKS).glob("*.log"))
            targets += list((root / CSMITH_BENCHMARKS).glob("*.log"))
            # targets += list(
            #     (root / OPEN_SOURCE_CASE_STUDIES).glob("*/.frama-c/*.codex/")
            # )
            targets.sort()
            self.results = [Target(x, self.root) for x in targets]
            self.results_ok = []
            self.results_err = []
            for res in self.results:
                if res.succeeded():
                    self.results_ok.append(res)
                else:
                    self.results_err.append(res)
        else:
            self.results = []
            self.results_err = []
            self.results_ok = []

    def pretty_target(self, target: Target, true_path: bool) -> str:
        """Prettify a target as "bench/cfile", removing the .frama-c and .codex"""
        if true_path:
            return str(target.path.relative_to(root_dir))
        return (
            str(target.path.relative_to(self.root))
            .replace(SVCOMP_BENCHMARKS + "/", "sv/")
            .replace(WHOLE_PROGRAM_BENCHMARKS + "/", "wp/")
            .replace(CSMITH_BENCHMARKS + "/", "cs/")
            # .replace("/", "{FG:blue}/{Reset}")
        )

    def move(self, save_dir: Path) -> None:
        """Move all analysis results to a new folder"""
        for target in self.results:
            newdir = save_dir / target.path.parent.relative_to(self.root)
            newdir.mkdir(parents=True, exist_ok=True)
            move(target.log_file(), newdir)
            if target.dump_file().exists():
                move(target.dump_file(), newdir)
            if target.stats_file().exists():
                move(target.stats_file(), newdir)

    # def get_eva_metrics(self, target: Path) -> dict[str, int]:
    #     """Get EVA's coverage, for comparison"""
    #     eva = (root_dir / target.relative_to(self.root)).with_suffix(
    #         ".eva"
    #     ) / "metrics.log"

    #     eva_stats: dict[str, int] = dict()
    #     if eva.is_file():
    #         with open(eva, "r") as file:
    #             contents = file.read()
    #         found = search(
    #             r"(\d+) stmts in analyzed functions, (\d+) stmts analyzed", contents
    #         )
    #         if found is not None:
    #             eva_stats["eva_total_stmts"] = int(found[1])
    #             eva_stats["eva_analyzed_stmts"] = int(found[2])
    #     else:
    #         print(
    #             f"Could not find file: {eva}. Run EVA analysis for coverage comparison"
    #         )
    #     return eva_stats


def get_available() -> list[Target]:
    chdir(root_dir)
    results = (
        check_output(
            [
                "make",
                "--no-print-directory",
                "list",
                "ISATTY=",
                "SVCOMPPATH=" + SVCOMP_PATH,
            ]
        )
        .decode()
        .strip()
        .split("\n")
    )
    results = [x.strip() for x in results if x.strip() != ""]
    results.sort()
    return [Target(root_dir / x, root_dir) for x in results]


def get_results(save: str | None) -> AnalysisResults:
    if save == ".":
        return AnalysisResults(root_dir, save=".")
    elif save is not None:
        return AnalysisResults(backups_dir / save, save=save)
    elif CONFIG_SELECTION in config:
        return AnalysisResults(backups_dir / config[CONFIG_SELECTION], save=config[CONFIG_SELECTION])
    return AnalysisResults(root_dir, save=".")


def count_diff(diff: str) -> int:
    count = 0
    for line in diff.split("\n"):
        if match(DUMP_LINE_REGEXP, line):
            count += 1
    return count


def append_if_numeric(stat: Stat[Any], lst: list[int | float]) -> None:
    """Add a stat to an accumulator only if it is an int or a float"""
    if isinstance(stat.value, int) or isinstance(stat.value, float):
        lst.append(stat.value)
    elif isinstance(stat.value, tuple) and (isinstance(stat.value[0], int) or isinstance(stat.value[0], float)):
        lst.append(stat.value[0])


def append_diff_if_numeric(
    stat1: Stat[Any], stat2: Stat[Any], lst: tuple[list[int | float], list[int | float]]
) -> None:
    """Add a stat to an accumulator only if it is an int or a float"""
    v1: int | float | None = None
    v2: int | float | None = None
    if isinstance(stat1.value, int) or isinstance(stat1.value, float):
        v1 = stat1.value
    elif isinstance(stat1.value, tuple) and (isinstance(stat1.value[0], int) or isinstance(stat1.value[0], float)):
        v1 = stat1.value[0]

    if isinstance(stat2.value, int) or isinstance(stat2.value, float):
        v2 = stat2.value
    elif isinstance(stat2.value, tuple) and (isinstance(stat2.value[0], int) or isinstance(stat2.value[0], float)):
        v2 = stat2.value[0]
    if v1 is not None and v2 is not None:
        lst[0].append(v1)
        lst[1].append(v2)


def main() -> int:
    saves = list(config[CONFIG_SAVES].keys() if CONFIG_SAVES in config else [])
    parser = make_parser(
        saves, config.get(CONFIG_SELECTION), STAT_KEYS, DEFAULT_COLUMNS, DEFAULT_DIFF_COLUMNS, FRAMA_C_CODEX
    )
    if cli_autocomplete is not None:
        cli_autocomplete(parser)
    args = parser.parse_args()

    Display.auto_colors(args.color)
    Stat.pretty_print = args.raw_values
    Table.format = args.table_format

    ############################################################################
    ## manage.py list-ok - shows the list of completed analysis
    ############################################################################
    if args.command == "list-ok":
        Results = get_results(args.save)
        if Results.results_ok == []:
            if Results.results_err == []:
                print("No analysis were run.")
            else:
                pprint("{FG:red}No analysis succeeded.{Reset}")
            return 0
        values: TableValue = []
        columns = args.columns
        if columns == []:
            columns = DEFAULT_COLUMNS
        if ALL in columns:
            columns = STAT_KEYS
        stats_v: list[list[int | float]] = [[] for _ in columns]
        for x in Results.results_ok:
            row: list[str | None] = [x.get_stat(column).pretty() for column in columns]
            row.insert(0, Results.pretty_target(x, args.true_path))
            values.append(row)
            for i, column in enumerate(columns):
                append_if_numeric(x.get_stat(column), stats_v[i])
        values.append(Table.SEPARATOR)
        values.append(Table.HEADERS)
        values.append(Table.SEPARATOR)
        stats = [Stats(x) for x in stats_v]
        values.append(
            list(
                ["{ST:bold}Sum{Reset}"]
                + [STATS[columns[i]].from_value(stats[i].sum).pretty() for i in range(len(columns))]
            )
        )
        values.append(
            list(
                ["{ST:bold}Max{Reset}"]
                + [STATS[columns[i]].from_value(stats[i].max).pretty() for i in range(len(columns))]
            )
        )
        values.append(
            list(
                ["{ST:bold}Min{Reset}"]
                + [STATS[columns[i]].from_value(stats[i].min).pretty() for i in range(len(columns))]
            )
        )
        values.append(list(["{ST:bold}Number of non-zero{Reset}"] + [str(s.non_zero) for s in stats]))
        values.append(
            list(
                ["{ST:bold}Average{Reset}"]
                + [STATS[columns[i]].from_value(stats[i].average).pretty() for i in range(len(columns))]
            )
        )
        values.append(
            list(
                ["{ST:bold}Standard deviation{Reset}"]
                + [STATS[columns[i]].from_value(stats[i].std_deviation).pretty() for i in range(len(columns))]
            )
        )
        values.append(list(["{ST:bold}Standard deviation{Reset}"] + [str(s.deviation_percent) + "%" for s in stats]))
        table = Table(
            headers=[
                "FILE",
            ]
            + [x.upper() for x in columns],
            values=values,
            column_align="l" + "r" * len(columns),
        )
        pprint(table.render())
        print(
            Display.format(
                "{FG:green}Completed {nb} / {total} analyses{Reset}",
                nb=len(Results.results_ok),
                total=len(Results.results),
            )
        )
    ############################################################################
    ## manage.py list-error - shows the list of failed analysis
    ############################################################################
    elif args.command == "list-error":
        Results = get_results(args.save)
        values = []
        for x in Results.results_err:
            values.append(
                [
                    Results.pretty_target(x, args.true_path),
                    "{FG:Red}"
                    + x.fail_reason().replace("{", "{{").replace("}", "}}").replace("\\n", " ").replace("  ", " ")
                    + "{Reset}",
                ]
            )
        table = Table(
            values=values,
            headers=["FILE", "ERROR"],
        )
        pprint(table.render())
        print(
            Display.format(
                "{FG:red}Failed {nb} / {total} analyses:{Reset}",
                nb=len(Results.results_err),
                total=len(Results.results),
            )
        )
    ############################################################################
    ## manage.py list-stats - shows available stats
    ############################################################################
    elif args.command == "list-stats":
        cols, _ = get_terminal_size()
        padding = max(len(k) for k in STAT_KEYS)
        pprint("{ST:Bold}Available stats:{Reset}")
        prefix = "\n" + (" " * (padding + 2))
        for k in STAT_KEYS:
            padded = k.ljust(padding)
            wrapped = prefix.join(wrap(STATS[k].help, width=cols - padding - 2))
            pprint(f" {{ST:Bold}}{padded}{{Reset}} {wrapped}")
        return 0
    ############################################################################
    ## manage.py stats file1 file2... - shows stats about the selected files
    ## using fuzzy select (so 'stats 20' will show stats for 2048
    ############################################################################
    elif args.command == "stats":
        Results = get_results(args.save)
        for x in Results.results_ok:
            if args.target == [] or any(tgt in Results.pretty_target(x, args.true_path) for tgt in args.target):
                pprint("{FG:green}Stats for " + Results.pretty_target(x, args.true_path) + ":{Reset}")
                for key in STAT_KEYS:
                    stat = x.get_stat(key)
                    if stat.value is not None:
                        pprint(f" {{ST:Bold}}{key}:{{Reset}} {stat.pretty()}")
        pprint("\nSee '{FG:green}./manage.py list-stats{Reset}' for a description of each stat.")
    ############################################################################
    ## manage.py output file1 file2... - shows dump of the selected files
    ## using fuzzy select (so 'output 20' will show stats for 2048
    ############################################################################
    elif args.command == "output":
        Results = get_results(args.save)
        for x in Results.results_ok:
            if args.target == [] or any(tgt in Results.pretty_target(x, args.true_path) for tgt in args.target):
                dump = x.dump_file()
                pprint("{FG:green}Codex output for " + Results.pretty_target(x, args.true_path) + ":{Reset}")
                with open(dump, "r") as file:
                    for line in file:
                        print("  " + line, end="")
    ############################################################################
    ## manage.py log file1 file2... - shows log of the selected files
    ## using fuzzy select (so 'output 20' will show stats for 2048
    ############################################################################
    elif args.command == "log":
        Results = get_results(args.save)
        for x in Results.results:
            if args.target == [] or any(tgt in Results.pretty_target(x, args.true_path) for tgt in args.target):
                log = x.log_file()
                pprint("{FG:green}Codex log for " + Results.pretty_target(x, args.true_path) + ":{Reset}")
                with open(log, "r") as file:
                    for line in file:
                        print("  " + line, end="")
    ############################################################################
    ## manage.py save - move all codex analysis result files to a new folder
    ## interactively asks for a save name and comment
    ############################################################################
    elif args.command == "save":
        # Only save the root, not other saves
        Results = AnalysisResults(root_dir, save=".")
        if len(Results.results) == 0:
            pprint("{FG:red}Error:{Reset} Nothing to save.")
            pprint(
                "Run the analysis {FG:blue}make <target>.codex{Reset} or {FG:blue}mmake codex -j <n>{Reset} to generate results"
            )
            return 1
        # Get a unique name
        try:
            if args.name is None:
                name = input("Folder name for save (leave blank to autogenerate): ").strip()
            else:
                name = args.name[0]
            if name == "":
                i = 1
                while (backups_dir / ("save-" + str(i))).exists():
                    i += 1
                name = "save-" + str(i)
            if (backups_dir / name).exists():
                pprint("{FG:red}Error:{Reset} a save named " + name + " already exists")
                return 1
            comment = ""
            if args.comment is None and args.name is None:
                comment = input("Comment (i.e. Codex version/config): ")
            if args.comment is not None:
                comment = args.comment[0]
        except KeyboardInterrupt:
            print("Aborted")
            return 1
        if CONFIG_SAVES not in config:
            config[CONFIG_SAVES] = dict()
        save_dir = backups_dir / name
        save_dir.mkdir()
        Results.move(save_dir)
        size = sum(f.stat().st_size for f in save_dir.glob("**/*") if f.is_file())
        print(
            Display.format(
                "Saved {nb} analysis results to {path}.\nTotal save size is {size}.",
                nb=len(Results.results),
                path=save_dir.relative_to(root_dir),
                size=pretty_size(size),
            )
        )
        print("Removed the analysis results from the main directory.")

        # Some metadata on the save
        config[CONFIG_SAVES][name] = {
            "comment": comment,
            "created": datetime.now().isoformat(),
            "size": size,
        }
    ############################################################################
    ## manage.py list-saves - shows a list of saved analysis
    ############################################################################
    elif args.command == "list-saves":
        if saves:
            values = []
            for save in saves:
                if "created" in config[CONFIG_SAVES][save]:
                    created = config[CONFIG_SAVES][save]["created"][:16].replace("T", " ")
                else:
                    created = "----------------"
                if "size" in config[CONFIG_SAVES][save]:
                    save_size = pretty_size(config[CONFIG_SAVES][save]["size"])
                else:
                    save_size = "--"
                values.append(
                    [
                        save,
                        created,
                        save_size,
                        config[CONFIG_SAVES][save].get("comment", "--"),
                    ]
                )
            table = Table(
                headers=["FOLDER", "CREATED", "SIZE", "COMMENT"],
                values=values,
                column_sep="  ",
                column_align="lrrl",
            )
            pprint(table.render())
            pprint("You can run a commands on a specific save with the {FG:blue}-s/--save <folder>{Reset} flag")
            pprint("or switch to run all commands on the save with {FG:blue}./manage.py switch <folder>{Reset}")
        else:
            pprint("No saves at the moment")
            pprint("Create a save with {FG:blue}./manage.py save{Reset}")
    ############################################################################
    ## manage.py switch <save> - select a save to inspect
    ## After a switch, you can use the stats, list, list-error commands on that save
    ## 'manage.py switch .' returns to no selected save
    ############################################################################
    elif args.command == "switch":
        if args.target == ".":
            del config[CONFIG_SELECTION]
        else:
            config[CONFIG_SELECTION] = args.target
    ############################################################################
    ## manage.py rm-save <save> - delete the given save
    ############################################################################
    elif args.command == "rm-save":
        for savename in args.target:
            save = backups_dir / savename
            rmtree(save)
            del config[CONFIG_SAVES][savename]
            print("Deleted save {}".format(save))
            if savename == config.get(CONFIG_SELECTION):
                del config[CONFIG_SELECTION]
                print("Switched back to root folder")
    ############################################################################
    ## manage.py contrast <save1> <save2> - difference in run statuses between two saves
    ############################################################################
    elif args.command == "contrast":
        save1 = get_results(args.save1)
        save2 = get_results(args.save2)
        values = []
        res1 = set((elt.path.relative_to(save1.root), elt in save1.results_err) for elt in save1.results)
        res2 = set((elt.path.relative_to(save2.root), elt in save2.results_err) for elt in save2.results)
        Ok = "{FG:Green}Passed{Reset}"
        Err = "{FG:Red}Failed{Reset}"
        Abs = "{FG:Blue}Absent{Reset}"
        seen = set()
        for elt, err in res1.difference(res2):
            tgt = Target(save1.root / elt, root=save1.root)
            values.append(
                [
                    save1.pretty_target(tgt, true_path=args.true_path),
                    str(tgt.cloc()),
                    Err if err else Ok,
                    (Ok if err else Err) if (elt, not err) in res2 else Abs,
                ]
            )
            seen.add(elt)
        for elt, err in res2.difference(res1):
            if elt in seen:
                continue
            tgt = Target(save2.root / elt, root=save2.root)
            values.append(
                [
                    save2.pretty_target(tgt, true_path=args.true_path),
                    str(tgt.cloc()),
                    (Ok if err else Err) if (elt, not err) in res1 else Abs,
                    Err if err else Ok,
                ]
            )
        values.sort()
        table = Table(
            values=values, headers=["FILE", "LOC", save1.save, save2.save], column_align="lrll", column_sep="  "
        )
        pprint(table.render())

    ############################################################################
    ## manage.py compare <save1> <save2> - difference between two saves
    ## use -f to filter to files that have precision differences
    ## use -p to show the log diffs
    ############################################################################
    elif args.command == "compare":
        save1 = get_results(args.save1)
        save2 = get_results(args.save2)
        ok1 = set(elt.path.relative_to(save1.root) for elt in save1.results_ok)
        ok2 = set(elt.path.relative_to(save2.root) for elt in save2.results_ok)
        common = ok1.intersection(ok2)

        if args.filter:
            print("Only showing files with precision differences")
        values = []
        columns = args.columns
        if columns == []:
            columns = DEFAULT_DIFF_COLUMNS
        if ALL in columns:
            columns = STAT_KEYS + [DIFF]
        stats_vc: list[list[int | float] | tuple[list[int | float], list[int | float]]] = []
        for col in columns:
            if col.startswith("1.") or col.startswith("2.") or col == DIFF:
                stats_vc.append([])
            else:
                stats_vc.append(([], []))

        compute_diff = DIFF in columns or args.filter

        for analysis in sorted(common):
            target1 = Target(save1.root / analysis, save1.root)
            target2 = Target(save2.root / analysis, save2.root)
            if compute_diff:
                diff = count_diff(
                    check_output(
                        "diff -y --suppress-common-lines '{}' '{}' || true".format(
                            target1.dump_file(),
                            target2.dump_file(),
                        ),
                        shell=True,
                    )
                    .decode()
                    .strip()
                )
            else:
                diff = 0
            if args.filter:
                alarms1 = target1.get_stat("alarms_unproved").value
                alarms2 = target2.get_stat("alarms_unproved").value
                nb_alarms = 0
                if isinstance(alarms1, int) and isinstance(alarms2, int):
                    nb_alarms = alarms1 - alarms2
                assert1 = target1.get_stat("asserts_unsolved").value
                assert2 = target2.get_stat("asserts_unsolved").value
                nb_asserts = 0
                if isinstance(assert1, int) and isinstance(assert2, int):
                    nb_asserts = assert1 - assert2
                no_diff = diff == 0 and nb_alarms == 0 and nb_asserts == 0
                if no_diff:
                    continue
            if args.print_diff and diff != 0:
                diff_str = "  " + check_output(
                    "diff --color={} --minimal --unified '{}' '{}' || true".format(
                        "always" if Display.colors == "ansi" else "never",
                        target1.dump_file(),
                        target2.dump_file(),
                    ),
                    shell=True,
                ).decode().replace("\n", "\n  ")
                pprint(
                    "{FG:blue}" + "-" * 100 + "\n",
                    diff,
                    save1.pretty_target(target1, true_path=args.true_path),
                    "\n" + "-" * 100 + "{Reset}",
                    sep="",
                )
                if not args.true_path:
                    diff_str = diff_str.replace(
                        "--- " + str(target1.dump_file()),
                        "--- " + save1.save + "/" + save1.pretty_target(target1, true_path=False),
                    ).replace(
                        "+++ " + str(target2.dump_file()),
                        "+++ " + save2.save + "/" + save2.pretty_target(target2, true_path=False),
                    )
                print(diff_str)
            row = [save1.pretty_target(target1, true_path=args.true_path)]
            for i, column in enumerate(columns):
                st = stats_vc[i]
                if column == DIFF:
                    row.append("{FG:blue}" + str(diff) + "{Reset}" if diff != 0 else str(diff))
                    assert isinstance(st, list)
                    st.append(diff)
                else:
                    if column.startswith("1."):
                        stat = target1.get_stat(column[2:])
                        row.append(stat.pretty())
                        assert isinstance(st, list)
                        append_if_numeric(stat, st)
                    elif column.startswith("2."):
                        stat = target2.get_stat(column[2:])
                        row.append(stat.pretty())
                        assert isinstance(st, list)
                        append_if_numeric(stat, st)
                    else:
                        assert isinstance(st, tuple)
                        stat1 = target1.get_stat(column)
                        stat2 = target2.get_stat(column)
                        row.append(stat1.pretty_diff(stat2))
                        append_diff_if_numeric(stat1, stat2, st)
            values.append(row)

        if not args.print_diff:
            values.append(Table.SEPARATOR)
            values.append(Table.HEADERS)
            values.append(Table.SEPARATOR)
            stats_: list[Stats[float] | DiffStats[float]] = [
                Stats(acc) if isinstance(acc, list) else DiffStats(acc[0], acc[1]) for acc in stats_vc
            ]
            summary = [
                [
                    "{ST:bold}" + x + "{Reset}"
                    for x in [
                        "Sum",
                        "Min",
                        "Max",
                        "Average",
                        "Standard deviation",
                        "Non-zero",
                        "{FG:green}Number of improvements",
                        "{FG:red}Number of regressions",
                    ]
                ]
            ]
            for i in range(len(columns)):
                sta = stats_[i]
                if isinstance(sta, Stats):
                    if columns[i] == DIFF:
                        summary.append(
                            [
                                str(sta.sum),
                                str(sta.min) if sta.min is not None else "--",
                                str(sta.max) if sta.max is not None else "--",
                                str(round(sta.average, 2)) if sta.average is not None else "--",
                                str(round(sta.std_deviation, 2)) if sta.std_deviation is not None else "--",
                                str(sta.non_zero),
                                "--",
                                "--",
                            ]
                        )
                    else:
                        stat_cls = STATS[columns[i][2:]]
                        summary.append(
                            [
                                stat_cls.from_value(sta.sum).pretty(),
                                stat_cls.from_value(sta.min).pretty(),
                                stat_cls.from_value(sta.max).pretty(),
                                stat_cls.from_value(sta.average).pretty(),
                                stat_cls.from_value(sta.std_deviation).pretty(),
                                str(sta.non_zero),
                                "--",
                                "--",
                            ]
                        )
                else:
                    stat_cls = STATS[columns[i]]

                    def pp(x: int | float | None) -> str:
                        return (
                            color_sign(stat_cls.from_value(x).pretty(), x, negative_green=stat_cls.negative_green)
                            if x is not None
                            else "--"
                        )

                    summary.append(
                        [
                            pp(sta.diff.sum),
                            pp(sta.diff.min),
                            pp(sta.diff.max),
                            pp(sta.diff.average),
                            pp(sta.diff.std_deviation),
                            str(sta.diff.non_zero),
                            str(sta.negative.size) if stat_cls.negative_green else str(sta.positive.size),
                            str(sta.positive.size) if stat_cls.negative_green else str(sta.negative.size),
                        ]
                    )
            for i in range(len(summary[0])):
                values.append([line[i] for line in summary])

            table = Table(
                values=values,
                headers=["FILE"] + [x.upper() for x in columns],
                column_align="l" + "r" * len(columns),
                column_sep="  ",
            )
            pprint(table.render())
            len_common = len(common)
            len_either = len(ok1.union(ok2))
            pprint(
                "\nComparing saves {} and {}: they have {} common analyses (out of {})".format(
                    save1.save, save2.save, len_common, len_either
                )
            )
            if len_common != len_either:
                pprint(f"To see which analyses differ, run '{{FG:blue}}./manage.py contrast {save1.save} {save2.save}{{Reset}}'")
    ############################################################################
    ## manage.py list-available - list all available benchmarks
    ############################################################################
    elif args.command == "list-available":
        available = get_available()
        res = AnalysisResults(root_dir, save=".", read=False)
        for x in available:
            print(res.pretty_target(x, args.true_path))
        pprint("{FG:green}Total:", len(available), "analysis targets{Reset}")
    ############################################################################
    ## manage.py run - run the benchmarks
    ############################################################################
    elif args.command == "run":
        total = len(get_available())
        res = AnalysisResults(root_dir, save=".", read=False)
        chdir(root_dir)
        process = Popen(
            [
                "make",
                "-j",
                str(args.processes),
                "all",
                "FRAMA_C_CODEX=" + args.exe,
                "CODEX_EXTRA_FLAGS=" + (EXTRA_FLAGS + " " + args.flags).strip(),
                "COLOR=OFF",
                "SVCOMPPATH=" + SVCOMP_PATH,
            ],
            stdout=PIPE,
            stderr=STDOUT,
        )
        assert process.stdout is not None
        if args.flags:
            pprint(
                "{ST:bold}Running analysis on",
                args.processes,
                "threads, with flags",
                args.flags.strip(),
                "{Reset}",
            )
        else:
            pprint("{ST:bold}Running analysis on", args.processes, "threads{Reset}")

        def safe_remove(list, x):
            try:
                list.remove(x)
            except ValueError:
                pass

        try:
            running = []
            ok = 0
            error = 0
            for stdout_line in process.stdout:
                if b"====" not in stdout_line:
                    continue
                line = stdout_line.decode()
                line = line[line.find("====") :]
                if line.startswith("==== Running codex on "):
                    running.append(line.split(" ")[-2])
                    pass
                elif line.startswith("==== Codex analysis finished "):
                    target = line.split(" ")[-2]
                    safe_remove(running, target)
                    pprint(
                        (
                            "  {FG:green}  OK  {Reset}  "
                            + res.pretty_target(Target(root_dir / target, root_dir), args.true_path)
                        ).ljust(50)
                    )
                    ok += 1
                elif line.startswith("==== Codex analysis FAILED "):
                    target = line.split(" ")[-2]
                    safe_remove(running, target)
                    pprint(
                        (
                            "  {FG:red}FAILED{Reset}  "
                            + res.pretty_target(Target(root_dir / target, root_dir), args.true_path)
                        ).ljust(50)
                    )
                    error += 1
                pprint("{ST:bold}Ran", ok + error, "/", total, "analyses{Reset}", end="\r")
            pprint(
                "{ST:bold}DONE. Ran",
                ok + error,
                "/",
                total,
                "analyses, ",
                ok,
                "succeeded and",
                error,
                "failed.{Reset}",
            )
        except KeyboardInterrupt:
            process.kill()
            pprint("\n{FG:red}INTERRUPTED{Reset}")
            for r in running:
                print(r)
    ############################################################################
    ## manage.py clean - remove all analysis files
    ############################################################################
    elif args.command == "clean":
        for folder in svcomp_dir, root_dir / WHOLE_PROGRAM_BENCHMARKS:
            check_output(["find", folder, "-name", "*.main.cdump", "-delete"])
            check_output(["find", folder, "-name", "*.stats.txt", "-delete"])
    ############################################################################
    ## manage.py without any commands - shows a list of completed analysis
    ############################################################################
    else:
        pprint("No command passed, call {FG:blue}./manage.py --help{Reset} for a list of commands")
        print()
        Results = AnalysisResults(root_dir, save=".")
        print(
            "Current status of root folder:\n  Completed {} analysis\n  Failed {} analysis\n".format(
                len(Results.results_ok), len(Results.results_err)
            )
        )
        if saves:
            print("Saves: {}".format(", ".join(saves)))
        else:
            print("No saves.")
        if CONFIG_SELECTION in config:
            print("Currently selected save: {}".format(config[CONFIG_SELECTION]))
        else:
            print("No save currently selected")

    return 0


retcode = main()

if config:
    with open(config_file, "w") as file:
        dump(config, file)
else:
    config_file.unlink(missing_ok=True)

exit(retcode)
