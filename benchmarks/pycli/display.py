"""
Display module - display ANSI escape sequence for colors and styles and unicode
  characters

The main export of the module is the Display class: and its main class methods are
- Display.format - classmethod - same as str.format but with custom code specifiers
    like {FG:red}, {ST:bold}, {Reset}, {FG:255,123,12}, {BG:#ff00ff} which get
    substituted for ANSI escape sequences.
    There are also code specifiers for unicode characters, often matching
    their latex command name {U:forall}, {u:cup}...
- Display.colors - "ansi", "off" - toggle Display.format rendering of ANSI chars on/off
    (defaults to "ansi" if stdout.isatty() is True, "off" otherwise)
- Display.unicode - "symbol", "alt-text", "html", "latex" - toggle Display.format
    rendering of unicode characters.
- Display.len - classmethod - same as str.len but counts length of ANSI code specifiers
    as 0 (true length of the printed sequence)
- Display.format_keep - classmethod - Same as str.format, but leaves the ANSI and
    unicode format specifiers unchanged
"""

from os import environ
from sys import stdout
from typing import Callable, Literal, Protocol


class SupportsFormat(Protocol):
    """Type hint for objects that can be passed to format"""

    def __format__(self, fmt: str, /) -> str:
        ...


RGBColorType = tuple[int, int, int]
UnicodeOutputMode = Literal["symbol", "alt-text", "html", "latex"]
ColorOutputMode = Literal["ansi", "off"]

# Support HTML color codes or color literals
# black, red, green, yellow, blue, purple, cyan, white
ColorType = str | RGBColorType
Colors: dict[str, RGBColorType] = {
    "black": (0, 0, 0),
    "red": (255, 0, 0),
    "green": (0, 255, 0),
    "blue": (0, 0, 255),
    "yellow": (255, 255, 0),
    "purple": (255, 0, 255),
    "cyan": (0, 255, 255),
    "white": (255, 255, 255),
}


def hex2rgb(hex: str) -> RGBColorType:
    """Compute RGB colors from a hex code"""
    if hex.startswith("#"):
        hex = hex[1:]
    if len(hex) == 6:
        r = int(hex[0:2], base=16)
        g = int(hex[2:4], base=16)
        b = int(hex[4:6], base=16)
    elif len(hex) == 3:
        r = int(hex[0] + hex[0], base=16)
        g = int(hex[1] + hex[1], base=16)
        b = int(hex[2] + hex[2], base=16)
    else:
        raise ValueError("Invalid hex string " + hex)
    return r, g, b


def rgb2hex(rgb: RGBColorType) -> str:
    """HTML color code out of RGB"""
    return "#" + "{:02x}{:02x}{:02x}".format(*rgb)


def color2rgb(color: ColorType) -> RGBColorType:
    """Convert a color to RGB"""
    if isinstance(color, tuple):
        return color
    if color in Colors:
        return Colors[color]
    return hex2rgb(color)


class ColorFormatter:
    """Replaces color name with their ANSI code variants"""

    FG_Codes = {
        # Foreground Colors
        "black": "\x1b[30m",
        "red": "\x1b[31m",
        "green": "\x1b[32m",
        "yellow": "\x1b[33m",
        "blue": "\x1b[34m",
        "purple": "\x1b[35m",
        "cyan": "\x1b[36m",
        "white": "\x1b[37m",
        # foreground highlight colors
        "hblack": "\x1b[90m",
        "hred": "\x1b[91m",
        "hgreen": "\x1b[92m",
        "hyellow": "\x1b[93m",
        "hblue": "\x1b[94m",
        "hpurple": "\x1b[95m",
        "hcyan": "\x1b[96m",
        "hwhite": "\x1b[97m",
        # special
        "reset": "\x1b[38m",
    }

    BG_Codes = {
        # background colors
        "black": "\x1b[40m",
        "red": "\x1b[41m",
        "green": "\x1b[42m",
        "yellow": "\x1b[43m",
        "blue": "\x1b[44m",
        "purple": "\x1b[45m",
        "cyan": "\x1b[46m",
        "white": "\x1b[47m",
        # background highlight colors
        "hblack": "\x1b[100m",
        "hred": "\x1b[101m",
        "hgreen": "\x1b[102m",
        "hyellow": "\x1b[103m",
        "hblue": "\x1b[104m",
        "hpurple": "\x1b[105m",
        "hcyan": "\x1b[106m",
        "hwhite": "\x1b[107m",
        # special
        "reset": "\x1b[48m",
    }

    fg: bool
    enabled: bool

    def __init__(self, is_fg: bool = True, enabled: bool = True):
        self.fg = is_fg
        self.enabled = enabled

    def __format__(self, fmt: str, /) -> str:
        if not self.enabled:
            return ""
        fmt = fmt.lower()
        codes = self.FG_Codes if self.fg else self.BG_Codes
        # Named colors
        if fmt in codes:
            return codes[fmt]
        # Grayscale gradient
        if fmt.startswith("gray") or fmt.startswith("grey"):  # because spelling is hard
            value = int(fmt[4:], base=0)
            if value < 0 or value >= 24:
                raise ValueError(
                    "Invalid value for grayscale color: {} should be in 0..23".format(
                        value
                    )
                )
            if self.fg:
                prefix = "\x1b[38;5;"
            else:
                prefix = "\x1b[48;5;"
            return prefix + str(232 + value) + "m"
        # RGB colors
        for sep in (",", ";", "-", "_"):
            if sep in fmt:
                split = fmt.split(sep)
                if len(split) != 3:
                    continue
                r = int(split[0], base=0)
                g = int(split[1], base=0)
                b = int(split[2], base=0)
                break
        else:
            # HTML Color code
            try:
                r, g, b = hex2rgb(fmt)
            except ValueError:
                raise ValueError(
                    "Invalid format for color: '"
                    + fmt
                    + "'. Valid values are named colors "
                    + ", ".join(codes.keys())
                    + " grayscale colors gray0 to gray23,"
                    + " RGB colors 0,0,0 to 255,255,255"
                    + " or a hex code ('#000000' to '#ffffff')."
                ) from None
        valid_range = range(256)
        if r not in valid_range or g not in valid_range or b not in valid_range:
            raise ValueError(
                "Invalid format for RGB string: '"
                + str(r)
                + ","
                + str(g)
                + ","
                + str(b)
                + "'. Values should be in range 0..255."
            )
        if self.fg:
            prefix = "\x1b[38;2;"
        else:
            prefix = "\x1b[48;2;"
        return prefix + str(r) + ";" + str(g) + ";" + str(b) + "m"


class StyleFormatter:
    """Replace style names with their ANSI code variants"""

    codes = {
        "bold": "\x1b[1m",
        "boldoff": "\x1b[22m",
        "faint": "\x1b[2m",
        "faintoff": "\x1b[22m",
        "italics": "\x1b[3m",
        "italicsoff": "\x1b[23m",
        "underline": "\x1b[4m",
        "underlineoff": "\x1b[24m",
        "blink": "\x1b[5m",
        "blinkoff": "\x1b[25m",
        "inverse": "\x1b[7m",
        "inverseoff": "\x1b[0m",
        "barred": "\x1b[9m",
        "barredoff": "\x1b[29m",
        "overline": "\x1b[53m",
        "overlineoff": "\x1b[55m",
    }

    enabled: bool

    def __init__(self, enabled: bool = True):
        self.enabled = enabled

    def __format__(self, fmt: str, /) -> str:
        if not self.enabled:
            return ""
        fmt = fmt.lower()
        if fmt in self.codes:
            return self.codes[fmt]
        raise ValueError(
            "Invalid format name for style '"
            + fmt
            + "'. Valid values are "
            + ", ".join(self.codes.keys())
        )


class UnchangedFormatter:
    """Formatter that prints itself unchanged"""

    key: str

    def __init__(self, key: str) -> None:
        self.key = key

    def __format__(self, fmt: str, /) -> str:
        if fmt:
            return "{" + self.key + ":" + fmt + "}"
        return "{" + self.key + "}"


class UnicodeFormatter:
    """Formatter for unicode characters"""

    output_mode: UnicodeOutputMode

    # name -> (unicode symbol, alt-text, latex macro)
    Unicode: dict[str, tuple[str, str, str]] = {
        "plusminus": ("±", "+/-", "\\pm"),
        "forall": ("∀", "forall", "\\forall"),
        "exists": ("∃", "exists", "\\exists"),
        "nexists": ("∄", "not exists", "\\not\\exists"),
        "in": ("∈", "in", "\\in"),
        "nin": ("∉", "not in", "\\notin"),
        "revin": ("∋", "rev in", "\\ni"),
        "nrevin": ("∌", "not rev in", "\\not\\ni"),
        "infty": ("∞", "infinity", "\\infty"),
        "sqrt": ("√", "sqrt", "\\sqrt{}"),
        "sqrt3": ("∛", "sqrt3", "\\sqrt[3]{}"),
        "sqrt4": ("∜", "sqrt4", "\\sqrt[4]{}"),
        "circ": ("∘", "circ", "\\circ"),
        "bullet": ("∙", "bullet", "\\bullet"),
        "entails": ("⊨", "entails", "\\vDash"),
        "proves": ("⊦", "proves", "\\vdash"),
        "top": ("⊤", "top", "\\top"),
        "bot": ("⊥", "bottom", "\\bot"),
        "sep": ("∗", "sep", "\\ast"),
        "and": ("∧", "and", "\\land"),
        "or": ("∨", "or", "\\lor"),
        "not": ("¬", "!", "\\lnot"),
        "times": ("×", "*", "\\times"),
        "div": ("÷", "/", "\\div"),
        "inter": ("∩", "inter", "\\cap"),
        "union": ("∪", "union", "\\cup"),
        "disjunion": ("⊎", "disjunion", "\\uplus"),
        "coloneq": ("≔", ":=", "\\coloneq"),
        "triangleeq": ("≜", "=t=", "\\triangleq"),
        "defeq": ("≝", "=def=", "\\triangleq"),
        "threeeq": ("≡", "===", "\\equiv"),
        "neq": ("≠", "!=", "\\neq"),
        "nsim": ("≁", "!~", "\\not\\sim"),
        "nthreeeq": ("≢", "!==", "\\not\\equiv"),
        "leq": ("≤", "<=", "\\leq"),
        "geq": ("≥", ">=", "\\geq"),
        "prec": ("≺", "-<", "\\prec"),
        "succ": ("≻", ">-", "\\succ"),
        "preceq": ("≼", "-<=", "\\preccurlyeq"),
        "succeq": ("≽", ">-=", "\\succcurlyeq"),
        "subset": ("⊂", "subset", "\\subset"),
        "supset": ("⊃", "supset", "\\supset"),
        "nsubset": ("⊄", "!subset", "\\nsubset"),
        "nsupset": ("⊅", "!supset", "\\nsupset"),
        "subseteq": ("⊆", "subseteq", "\\subseteq"),
        "supseteq": ("⊇", "supseteq", "\\supseteq"),
        "nsubseteq": ("⊈", "!subseteq", "\\nsubseteq"),
        "nsupseteq": ("⊉", "!supseteq", "\\nsupseteq"),
        "gtlt": ("≶", "gtlt", "\\gtrless"),
        "gele": ("⋚", "gele", "\\gtreqless"),
        "empty": ("∅", "empty", "\\emptyset"),
        "delta": ("∆", "delta", "\\delta"),
        "widen": ("∇", "widen", "\\nable"),
        "bowtie": ("⋈", "|><|", "\\bowtie"),
        "sqsubset": ("⊏", "sqsubset", "\\sqsubset"),
        "sqsupset": ("⊐", "sqsupset", "\\sqsupset"),
        "sqsubseteq": ("⊑", "sqsubseteq", "\\sqsubseteq"),
        "sqsupseteq": ("⊒", "sqsupseteq", "\\sqsupseteq"),
        "join": ("⊔", "join", "\\sqcup"),
        "meet": ("⊓", "meet", "\\sqcap"),
        "ellipsis": ("⋯", "...", "\\ldots"),
        "leftarrow": ("←", "<-", "\\leftarrow"),
        "rightarrow": ("→", "->", "\\rightarrow"),
        "leftrightarrow": ("↔", "<->", "\\leftrightarrow"),
        "Leftarrow": ("⇐", "<=", "\\Leftarrow"),
        "Rightarrow": ("⇒", "=>", "\\Rightarrow"),
        "Leftrightarrow": ("⇔", "<=>", "\\Leftrightarrow"),
        "galois": ("⇄", "galois", "\\rightleftarrows"),
        "mapsto": ("↦", "|->", "\\mapsto"),
    }
    # Some aliases
    Unicode["+/-"] = Unicode["plusminus"]
    Unicode["pm"] = Unicode["plusminus"]
    Unicode["<="] = Unicode["leq"]
    Unicode[">="] = Unicode["geq"]
    Unicode["=>"] = Unicode["Rightarrow"]
    Unicode["<=>"] = Unicode["Leftrightarrow"]
    Unicode["->"] = Unicode["rightarrow"]
    Unicode["<-"] = Unicode["leftarrow"]
    Unicode["<->"] = Unicode["leftrightarrow"]
    Unicode["!="] = Unicode["neq"]
    Unicode[":="] = Unicode["coloneq"]
    Unicode["/\\"] = Unicode["and"]
    Unicode["\\/"] = Unicode["or"]
    Unicode["cup"] = Unicode["union"]
    Unicode["cap"] = Unicode["inter"]
    Unicode["sqcup"] = Unicode["join"]
    Unicode["sqcap"] = Unicode["meet"]
    Unicode["vdash"] = Unicode["proves"]
    Unicode["vDash"] = Unicode["entails"]

    def __init__(self, output_mode: UnicodeOutputMode) -> None:
        self.output_mode = output_mode

    def __format__(self, fmt: str, /) -> str:
        char = fmt.strip()
        if char not in self.Unicode:
            if char.lower() not in self.Unicode:
                raise ValueError("Unknown unicode symbol '{}'".format(char))
            char = char.lower()
        if self.output_mode == "symbol":
            return self.Unicode[char][0]
        elif self.output_mode == "alt-text":
            text = self.Unicode[char][1]
            if fmt.startswith(" "):
                text = " " + text
            if fmt.endswith(" "):
                text = text + " "
            return text
        elif self.output_mode == "html":
            return "&#" + str(ord(self.Unicode[char][0])) + ";"
        elif self.output_mode == "latex":
            return self.Unicode[char][2]


class Display:
    unicode: UnicodeOutputMode = "symbol"
    colors: ColorOutputMode

    @classmethod
    def auto_colors(
        cls,
        parameter: Literal[
            "auto",
            "always",
            "on",
            "yes",
            "true",
            "force",
            "never",
            "off",
            "no",
            "false",
        ],
    ) -> None:
        """Check whether or not we should use ANSI colors:
        Check if the given parameter requires/disables it first
        If not, check for env variables NO_COLOR and CLICOLOR_FORCE (http://bixense.com/clicolors/)
        If not, check stdout.isatty"""
        if parameter in ["always", "on", "yes", "true", "force"]:
            cls.colors = "ansi"
        elif parameter in ["never", "off", "no", "false"]:
            cls.colors = "off"
        elif environ.get("NO_COLOR"):
            cls.colors = "off"
        elif environ.get("CLICOLOR_FORCE"):
            cls.colors = "ansi"
        else:
            cls.colors = "ansi" if stdout.isatty() else "off"

    GRAYSCALE_MAX = 24
    RGB_MAX = 256

    Codes: dict[str, SupportsFormat] = {
        # Extras
        "FG": ColorFormatter(is_fg=True),
        "BG": ColorFormatter(is_fg=False),
        "ST": StyleFormatter(),
        "Reset": "\x1b[0m",
        "Clearscreen": "\x1bc",
        "Clearline": "\x1b[2K\x1b[1G",
    }

    # Empty dict used when disabled
    EmptyCodes: dict[str, SupportsFormat] = {
        # Extras
        "FG": ColorFormatter(is_fg=True, enabled=False),
        "BG": ColorFormatter(is_fg=False, enabled=False),
        "ST": StyleFormatter(enabled=False),
        "Reset": "",
        "Clearscreen": "",
        "Clearline": "",
    }

    class Default(dict[str, SupportsFormat]):
        def __missing__(self, key: str) -> SupportsFormat:
            if key in Display.Codes.keys():
                return UnchangedFormatter(key)
            raise KeyError(key)

    @classmethod
    def format_keep(cls, string: str, **kwargs: object) -> str:
        """Return the string formatted with kwargs,
        leaving the unicode and color formatters unchanged"""
        return string.format_map(cls.Default(**kwargs))

    @classmethod
    def format(
        cls,
        string: str,
        colors: ColorOutputMode | None = None,
        unicode: UnicodeOutputMode | None = None,
        **kwargs: object,
    ) -> str:
        """Behaves like str.format, but supports extra codes for inserting color/styles:
        > Display.format("{FG:red}Red Text{Reset} normal text {StBold}Now bold{Reset}")
        > Display.format("{FG:255,125,10}RGB orange {BG:#000000}HTML codes{Reset}")
        > Display.format("{Fg:gray0}Grayscale from 0 (black) to 23 (white){Reset}")

        Accepted colors 'FG' or 'BG' are:
        - black, red, green, yellow, blue, purple, cyan, white
        - the highlight version of the above, prefixed with 'h' (e.g. hred)
        - grayscale gradient: gray0 to gray23
        - RGB colors: 0,0,0 to 255,255,255
        - HTML colors: #000 to #FFF or #000000 to #FFFFFF
        Accepted styles 'ST' are bold, faint, italics, underline, blink, inverse,
          barred, overline
        Additional codes:
        - 'Reset' resets colors and styles
        - 'Clearline' and 'Clearscreen'
        - 'U' for unicode characters, often following their latex commands
          e.g. {U:forall} for the \\forall symbol.

        You can also add you own custom arguments, just like with format, although only
        the keyword syntax is supported (no positional arguments)
        > Display.format("With argument {FG:hred}{x}{Reset}", x=3)
        Variable name should avoid clashes with the defined keys
        (FG, BG, ST, U, Reset, Clearline, Clearscreen).

        You can disable ANSI codes output and/or replace unicode sequences by an
        alt-text using the 'colors' and 'unicode' keyword arguments (for a single call)
        or class attributes (for all subsequent calls).
        By default colors is set to isatty(), and use_unicode is True.
        """
        colors = colors if colors is not None else cls.colors
        unicode = unicode if unicode is not None else cls.unicode
        codes = cls.Codes if colors == "ansi" else cls.EmptyCodes
        codes["U"] = UnicodeFormatter(unicode)
        return string.format(**kwargs, **codes)

    @classmethod
    def len(cls, string: str, unicode: UnicodeOutputMode | None = None) -> int:
        """Length of a string without counting the defined ANSI Codes keys
        ({FG:red}, {Reset}, ...). AKA the length that will be printed if the
        string is rendered with Display.format"""
        unicode = unicode if unicode is not None else cls.unicode
        return len(cls.format(string, colors="off"))

    @classmethod
    def grayscale(cls, value: int, fg: bool = True) -> str:
        """Return {FG:gray<value>}, which can then be rendered by Display.format"""
        if fg:
            return "{FG:gray" + str(value) + "}"
        return "{BG:gray" + str(value) + "}"

    @classmethod
    def rgb(cls, color: RGBColorType, fg: bool = True) -> str:
        r, g, b = color
        """Return {FG:r,g,b}, which can then be rendered by Display.format"""
        if fg:
            return "{FG:" + str(r) + "," + str(g) + "," + str(b) + "}"
        return "{BG:" + str(r) + "," + str(g) + "," + str(b) + "}"

    @classmethod
    def _to_sRGB(cls, x: float) -> int:
        """Returns a sRGB value in the range [0,255] for linear input in [0,1]"""
        return int(
            255.9999
            * (12.92 * x if x <= 0.0031308 else (1.055 * (x ** (1 / 2.4))) - 0.055)
        )

    @classmethod
    def _from_sRGB(cls, x: int) -> float:
        """Returns a linear value in the range [0,1] for sRGB input in [0,255]"""
        v = x / 255
        if v <= 0.04045:
            return v / 12.92
        return ((v + 0.055) / 1.055) ** 2.4

    @classmethod
    def _lerp(cls, color1: float, color2: float, frac: float) -> float:
        """Linear interpolation between both values"""
        return color1 * (1.0 - frac) + color2 * frac

    @classmethod
    def color_mix(cls, color1: RGBColorType, color2: RGBColorType, fraction: float):
        """Mix two colors with the given fraction (0 if full color1, 1 is full color2)
        Algorithm taken from https://stackoverflow.com/questions/22607043/color-gradient-algorithm"""

        if fraction < 0.0:
            return color1
        if fraction > 1.0:
            return color2
        gamma = 0.43
        r1 = cls._from_sRGB(color1[0])
        g1 = cls._from_sRGB(color1[1])
        b1 = cls._from_sRGB(color1[2])
        bright1 = (r1 + g1 + b1) ** gamma
        r2 = cls._from_sRGB(color2[0])
        g2 = cls._from_sRGB(color2[1])
        b2 = cls._from_sRGB(color2[2])
        bright2 = (r2 + g2 + b2) ** gamma
        intensity = cls._lerp(bright1, bright2, fraction) ** (1 / gamma)
        r = cls._lerp(r1, r2, fraction)
        g = cls._lerp(g1, g2, fraction)
        b = cls._lerp(b1, b2, fraction)
        sum_rgb = r + g + b
        if sum_rgb > 0:
            factor = intensity / sum_rgb
            r *= factor
            g *= factor
            b *= factor
        else:
            r = g = b = 0.0
        return cls._to_sRGB(r), cls._to_sRGB(g), cls._to_sRGB(b)

    @classmethod
    def gradient(
        cls, color1: RGBColorType, color2: RGBColorType, steps: int
    ) -> list[RGBColorType]:
        """Generate a RGB color gradient between both given colors in the given
        number of steps"""
        return [
            cls.color_mix(color1, color2, step / (steps - 1)) for step in range(steps)
        ]

    @classmethod
    def color_scale(
        cls, scale: list[tuple[float, ColorType]]
    ) -> Callable[[float], RGBColorType]:
        """Return a function that colors number according to the given scale
        (a list of color and values). The final color is a linear gradient of
        the colors of the two closest values, ponderated by how close they are"""
        nscale = [(nb, color2rgb(color)) for nb, color in scale]
        nscale.sort(key=lambda x: x[0])

        def format(x: float) -> RGBColorType:
            if x <= nscale[0][0]:
                return nscale[0][1]
            for i in range(1, len(scale)):
                if x <= scale[i][0]:
                    return cls.color_mix(
                        nscale[i - 1][1],
                        nscale[i][1],
                        (x - scale[i - 1][0]) / (scale[i][0] - scale[i - 1][0]),
                    )
            return nscale[-1][1]

        return format

    @classmethod
    def test_gradient(cls, hex1: str, hex2: str, steps: int) -> None:
        c1 = hex2rgb(hex1)
        c2 = hex2rgb(hex2)
        gradient = cls.gradient(c1, c2, steps)
        print(
            cls.format(
                "Gradient starting at "
                + cls.rgb(c1, False)
                + "    "
                + hex1
                + " {Reset}"
            )
        )
        for i, color in enumerate(gradient):
            print(
                cls.format(
                    cls.rgb(color, False)
                    + "Step "
                    + str(i)
                    + " "
                    + rgb2hex(color)
                    + " {Reset}"
                )
            )
        print(
            cls.format(
                "Gradient ends at " + cls.rgb(c2, False) + "    " + hex2 + " {Reset}"
            )
        )


def pprint(*values: object, sep: str | None = None, end: str | None = None) -> None:
    if sep is not None:
        sep = Display.format(sep)
    if end is not None:
        end = Display.format(end)
    print(*(Display.format(str(value)) for value in values), sep=sep, end=end)


Display.auto_colors("auto")
