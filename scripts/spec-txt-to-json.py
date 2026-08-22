#!/usr/bin/env python3
"""Convert a CommonMark-family ``spec.txt`` into the ``spec.json`` fixture shape.

CommonMark publishes ``spec.json`` alongside its ``spec.txt``; the GitHub Flavored Markdown spec publishes only the
text form, so its fixtures have to be derived. The extraction rules are the ones ``cmark``'s own ``makespec.py``
uses: an example is the text between a run of at least twenty backticks introducing ``example`` and the matching
closing run, with a lone ``.`` separating the Markdown source from the expected HTML, and ``→`` standing for a
tab throughout.

Emits the fields the conformance harness reads -- ``markdown``, ``html``, ``example`` and ``section`` -- plus
``extension`` when the fence names one (``example table``), so a harness can report an extension's score separately
without re-parsing the spec text. The published CommonMark fixture also carries ``start_line`` and ``end_line``;
those are omitted here because they are numbered against the spec source ``makespec.py`` was run over rather than
the one published for download, so they cannot be reproduced from the downloadable text.

    scripts/spec-txt-to-json.py spec.txt > spec.json
"""

from __future__ import annotations

import json
import re
import sys

FENCE = re.compile(r"^`{20,}\s*example(?P<info>[^\n]*)$")
CLOSE = re.compile(r"^`{20,}\s*$")
HEADING = re.compile(r"^#{1,2} (?P<title>.+?)\s*$")


def convert(text: str) -> list[dict]:
    examples: list[dict] = []
    section = ""
    state = "text"
    markdown: list[str] = []
    html: list[str] = []
    info = ""
    start_line = 0

    for number, line in enumerate(text.split("\n"), start=1):
        if state == "text":
            fence = FENCE.match(line)
            if fence:
                state, markdown, html, info, start_line = "markdown", [], [], fence.group("info").strip(), number
                continue
            heading = HEADING.match(line)
            if heading:
                section = heading.group("title")
            continue
        if state == "markdown":
            if line == ".":
                state = "html"
            else:
                markdown.append(line)
            continue
        if CLOSE.match(line):
            entry = {
                "markdown": "".join(l + "\n" for l in markdown).replace("→", "\t"),
                "html": "".join(l + "\n" for l in html).replace("→", "\t"),
                "example": len(examples) + 1,
                "section": section,
            }
            if info:
                entry["extension"] = info
            examples.append(entry)
            state = "text"
        else:
            html.append(line)

    if state != "text":
        raise SystemExit(f"unterminated example opened at line {start_line}")
    return examples


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(f"usage: {sys.argv[0]} <spec.txt>")
    with open(sys.argv[1], encoding="utf-8") as handle:
        examples = convert(handle.read())
    json.dump(examples, sys.stdout, indent=2, ensure_ascii=False)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
