# CLI Parser for the ./manage.py script

from argparse import ArgumentParser, HelpFormatter

from .table import DEFAULT_FORMAT, FORMATS


class SmartFormatter(HelpFormatter):
    def _split_lines(self, text, width):
        if text.startswith("R|"):
            return text[2:].splitlines()
        # this is the RawTextHelpFormatter._split_lines
        return HelpFormatter._split_lines(self, text, width)

    def _fill_text(self, text, width, indent):
        if text.startswith("R|"):
            return indent + text[2:].replace("\n", "\n" + indent)


ALL = "all"
DIFF = "diff"


def make_parser(
    saves: list[str],
    current_save: str | None,
    stat_keys: list[str],
    default_columns: list[str],
    default_diff_columns: list[str],
    frama_c_codex: str,
) -> ArgumentParser:
    root = current_save if current_save is not None else "root folder"

    # Common options
    parser = ArgumentParser(formatter_class=SmartFormatter)
    parser.add_argument(
        "--save",
        "-s",
        default=None,
        choices=["."] + saves,
        help="Run command on selected save",
    )
    parser.add_argument(
        "--true-path",
        "-t",
        default=False,
        action="store_true",
        help="Print true file paths instead of their shortened versions",
    )
    parser.add_argument(
        "--color",
        "-c",
        default="auto",
        choices=["always", "auto", "never"],
        help="enable or disable ANSI coloring",
    )
    parser.add_argument(
        "--raw-values",
        "-r",
        action="store_false",
        help="Show raw values/disable pretty printing (no rounding or grouping by units)",
    )
    parser.add_argument(
        "--table-format",
        "-f",
        default=DEFAULT_FORMAT,
        choices=FORMATS,
        help="Output format of tables for commands that display tables, default: " + DEFAULT_FORMAT,
    )

    # Subcommands
    subparsers = parser.add_subparsers(dest="command")
    subparsers.add_parser(
        "list-error",
        help="list analysis that failed",
        description="This just list all analysis that did NOT succeed in the current save. "
        "By succeeded, I mean 'generated a stats.txt file'.",
    )
    subparsers.add_parser(
        "list-stats",
        help="Show all available stats",
        description="This prints all stats along with their help text.",
    )

    ok_parser = subparsers.add_parser(
        "list-ok",
        help="list analysis that succeeded",
        description="This just list all analysis that succeeded in the current save. "
        "By succeeded, I mean 'generated a stats.txt file and got exit code 0'.",
    )
    ok_parser.add_argument(
        "--columns",
        "-c",
        action="append",
        default=[],
        choices=stat_keys + [ALL],
        help="select which stats to display, default is " + ", ".join(default_columns),
    )

    # Stats command line
    stat_parser = subparsers.add_parser(
        "stats",
        help="show stats.txt file of the given analysis",
        description="R|Prints various stats for the given targets, using a partial match\n"
        "so 'stats x y z' prints stats for all targets that contain 'x', 'y' or 'z'.\n\n"
        "See './manage.py list-stats' for a description of the printed stats.",
        formatter_class=SmartFormatter,
    )
    stat_parser.add_argument(
        "target",
        default=[],
        nargs="*",
        help="select files (partial match possible, "
        "so 'cwe' will print stats.txt for all results that contain 'cwe' in their name)",
    )

    # Output command line
    out_parser = subparsers.add_parser(
        "output",
        help="show result file of the given analysis",
        description="Prints the codex result for the given targets, using a partial match\n"
        "so 'output x y z' prints stats for all targets that contain 'x', 'y' or 'z'.\n\n",
    )
    out_parser.add_argument(
        "target",
        default=[],
        nargs="*",
        help="select files (partial match possible, so 'cwe' will match all results that contain 'cwe' in their name)",
    )

    # log command line
    log_parser = subparsers.add_parser(
        "log",
        help="show log file of the given analysis",
        description="Prints the codex log (stdout) for the given targets, using a partial match\n"
        "so 'log x y z' prints stats for all targets that contain 'x', 'y' or 'z'.\n\n",
    )
    log_parser.add_argument(
        "target",
        default=[],
        nargs="*",
        help="select files (partial match possible, so 'cwe' will match all results that contain 'cwe' in their name)",
    )

    save_parser = subparsers.add_parser(
        "save",
        help="save the analysis results (move to a new folder)",
        description="This moves all analysis result folders '*/.frama-c/*.codex' into a new folder "
        ".saves/<name>. This way they won't be cleaned by 'make clean' and can be reinspected at will. "
        "Use the './manage.py switch' command or the '-s/--save' flag to run an analysis on a specific save.",
    )
    save_parser.add_argument(
        "-n",
        "--name",
        nargs=1,
        help="the name of the new save, asked interactively if absent",
    )
    save_parser.add_argument("-c", "--comment", nargs=1, help="a comment describing the save")
    subparsers.add_parser(
        "list-saves",
        help="show the current saves",
        description="Print a list of the current saves, along with their size and comment.",
    )

    # Switch command line
    switch_parser = subparsers.add_parser(
        "switch",
        help="switch to a save",
        description="This changes the state so that any future command will be run on the given save. "
        + "Use './manage.py save .' to reset to the top folder. Currently, "
        + (
            "save {} is selected.".format(current_save)
            if current_save is not None
            else "no save is selected (run on root folder)"
        ),
    )
    switch_parser.add_argument("target", choices=["."] + saves, help="The save to switch to")

    # rm-save command line
    rm_parser = subparsers.add_parser(
        "rm-save",
        help="delete a save",
        description="This removes the save folder and all associated metadata",
    )
    rm_parser.add_argument("target", choices=saves, help="The save to delete", nargs="+")

    # Diff command line
    diff_parser = subparsers.add_parser(
        "compare",
        help="show a file-by-file comparison between two saves",
        description="For both analysis, compare for each file (in common) the various stats (time/memory/alarms...). "
        "Also show the number of diff lines between the two dump files. "
        "You can use the -f and -p flags to refine output.",
    )
    diff_parser.add_argument(
        "save1",
        nargs="?",
        default=None,
        choices=["."] + saves,
        help="first save to compare (defaults to current '{}')".format(root),
    )
    diff_parser.add_argument("save2", choices=["."] + saves, help="second save to compare")
    diff_parser.add_argument(
        "--filter",
        "-f",
        action="store_true",
        help="only show files with precision differences",
    )
    diff_parser.add_argument(
        "--print-diff",
        "-p",
        action="store_true",
        help="print diff of dumps when non null",
    )
    diff_parser.add_argument(
        "--columns",
        "-c",
        action="append",
        default=[],
        choices=stat_keys + [ALL, DIFF] + ["1." + x for x in stat_keys] + ["2." + x for x in stat_keys],
        help="Columns to display in the output, prefix by '1.' or '2.' to show the value for old/new."
        "Default value is: " + ", ".join(default_diff_columns),
    )

    contrast_parser = subparsers.add_parser(
        "contrast",
        help="shows which analyses are present in only one of the two saves",
        description="Prints a table showing which analyses suceeded in only one of the given saves",
    )
    contrast_parser.add_argument(
        "save1",
        nargs="?",
        default=None,
        choices=["."] + saves,
        help="first save to compare (defaults to current '{}')".format(root),
    )
    contrast_parser.add_argument("save2", choices=["."] + saves, help="second save to compare")

    subparsers.add_parser("list-available", help="list all available benchmarks (make list)")

    run_parser = subparsers.add_parser("run", help="run the benchmarks")
    run_parser.add_argument(
        "--processes",
        "-j",
        type=int,
        default=1,
        help="Number of processes to use. Should be smaller than number of cores and RAM / 10GB",
    )
    run_parser.add_argument(
        "--flags",
        "-f",
        type=str,
        default="",
        help="extra flags to pass to codex during the analysis",
    )
    run_parser.add_argument(
        "--exe",
        "-e",
        type=str,
        default=frama_c_codex,
        help="the frama_c_codex executable to use, default='" + frama_c_codex + "'",
    )
    subparsers.add_parser("clean", help="remove all analysis generated files in workdir, but keep saves")

    return parser
