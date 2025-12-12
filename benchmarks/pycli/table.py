"""
Render pretty tables to the terminal, aligning columns correctly
"""

from csv import writer
from io import StringIO
from typing import Literal

from .display import Display

TableRow = list[str | None] | Literal["---", "headers"]
TableValue = list[TableRow]

Alignement = Literal["l", "c", "r"]

FORMAT_TYPE = Literal["tty", "csv", "latex"]
FORMATS: list[FORMAT_TYPE] = ["tty", "csv", "latex"]
DEFAULT_FORMAT: FORMAT_TYPE = "tty"


def cast_alignement(x: str) -> Alignement:
    x = x.lower()
    if x == "l":
        return "l"
    elif x == "c":
        return "c"
    elif x == "r":
        return "r"
    raise ValueError(x + " is not a valid alignment")


class Table:
    """Pretty print tables"""

    SEPARATOR: Literal["---"] = "---"
    HEADERS: Literal["headers"] = "headers"

    headers: list[str] | None
    values: list[TableRow]
    width: int
    column_sep: str
    column_align: list[Alignement]
    header_style: str

    format: FORMAT_TYPE = DEFAULT_FORMAT

    def __init__(
        self,
        values: list[TableRow],
        headers: list[str] | None = None,
        column_align: list[Alignement] | str | None = None,
        column_sep: str = " ",
        header_style: str = "{ST:bold}",
    ) -> None:
        self.headers = headers
        self.values = values
        self.width = len(self.values[0])
        self.column_sep = column_sep
        if headers is not None and len(headers) != self.width:
            raise ValueError(
                "Table has width {}, but only received {} headers".format(
                    self.width, len(headers)
                )
            )
        if column_align is None:
            self.column_align = ["l"] * self.width
        else:
            self.column_align = [cast_alignement(x) for x in column_align]
        if len(self.column_align) != self.width:
            raise ValueError(
                "Table has width {}, but only received {} column aligns".format(
                    self.width, len(self.column_align)
                )
            )
        self.header_style = header_style

    def compute_widths(self) -> list[int]:
        """Return a list of widths for each column"""
        widths = [0] * self.width
        if self.headers is not None:
            for i, hd in enumerate(self.headers):
                widths[i] = Display.len(hd)
        for row in self.values:
            if row == self.SEPARATOR or row == self.HEADERS:
                continue
            for i, x in enumerate(row):
                if x is not None:
                    widths[i] = max(widths[i], Display.len(x))
        return widths

    def justify(self, cell: str | None, align: Alignement, width: int) -> str:
        """return cell padded to the specified width"""
        if cell is None:
            cell = ""
        difference = len(cell) - Display.len(cell)
        if align == "l":
            return cell.ljust(width + difference)
        elif align == "c":
            return cell.center(width + difference)
        elif align == "r":
            return cell.rjust(width + difference)

    def tty(self) -> str:
        """Render the table as a string"""
        render = ""
        widths = self.compute_widths()

        if self.headers is not None:
            headers = self.header_style
            for i, header in enumerate(self.headers):
                headers += self.justify(header, "l", widths[i])
                if i + 1 < self.width:
                    headers += self.column_sep
                else:
                    headers += "\n"
            headers += "{Reset}"
        render += headers
        sum_widths = sum(widths) + len(self.column_sep) * (self.width - 1)
        separator = "-" * sum_widths + "\n"
        for row in self.values:
            if row == self.SEPARATOR:
                render += separator
                continue
            if row == self.HEADERS:
                if self.headers is None:
                    raise ValueError("headers in table without headers")
                render += headers
                continue
            for i, cell in enumerate(row):
                render += self.justify(cell, self.column_align[i], widths[i])
                if i + 1 < self.width:
                    render += self.column_sep
                else:
                    render += "\n"

        return render

    def csv(self) -> str:
        """Return a CSV string representing the table"""
        out = StringIO()
        csv_writer = writer(out)
        if self.headers is not None:
            csv_writer.writerow(self.headers)
        for row in self.values:
            if row == self.SEPARATOR:
                csv_writer.writerow([])
            elif row == self.HEADERS:
                if self.headers is None:
                    raise ValueError("headers in table without headers")
                csv_writer.writerow(self.headers)
            else:
                csv_writer.writerow(row)

        return out.getvalue()

    def latex_format(self, cell: str|None) -> str:
        if cell is None:
            return ""
        return cell.replace("%", "\\%").replace("&", "\\&").replace("_","\\_")

    def latex(self) -> str:
        """Return a latex representation of the table"""
        render = f"\\begin{{{{tabular}}}}{"".join(self.column_align)}\n  \\hline\n"
        if self.headers is not None:
            render += "  " + " & ".join(self.headers) + "\n  \\hline\n"
        for row in self.values:
            if row == self.SEPARATOR:
                render += "  \\hline\n"
            elif row == self.HEADERS:
                if self.headers is None:
                    raise ValueError("headers in table without headers")
                render += "  " + " & ".join(self.headers) + "\n"
            else:
                render += "  " + " & ".join(self.latex_format(x) for x in row) + "\n"
        render += "\\end{{tabular}}"
        return render

    def render(self, format: FORMAT_TYPE | None = None) -> str:
        if format is None:
            format = self.format
        if format == "tty":
            return self.tty()
        if format == "csv":
            return self.csv()
        if format == "latex":
            return self.latex()
        raise ValueError("Invalid format: " + format)
