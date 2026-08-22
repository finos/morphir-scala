# Vendored conformance fixtures

Each profile the harness measures has its examples vendored here rather than fetched at test time, so the suite is
hermetic and CI never depends on a specification host being reachable. Do not edit a fixture file: a change here is
not a change to what conformance means.

## `commonmark-0.31.2-spec.json`

The official CommonMark 0.31.2 example set, fetched verbatim from <https://spec.commonmark.org/0.31.2/spec.json>. It
holds 652 examples, each with the Markdown source and the exact HTML a conformant implementation must produce.

## `gfm-0.29-spec.json`

The GitHub Flavored Markdown 0.29-gfm example set, 672 examples. GitHub publishes the specification only as text, so
unlike CommonMark there is no JSON to fetch; this file is derived from it by `scripts/spec-txt-to-json.py`, which
applies the extraction rules cmark's own `makespec.py` uses:

```
curl -O https://raw.githubusercontent.com/github/cmark-gfm/master/test/spec.txt
scripts/spec-txt-to-json.py spec.txt > morphir/langkit/markdown/scalatags/test/resources/gfm-0.29-spec.json
```

That script is checked against this directory's CommonMark file — run over CommonMark's own `spec.txt`, it reproduces
all 652 entries field for field — which is what makes the derived GFM file trustworthy.

Entries carry one field the CommonMark set does not: `extension`, holding the name the specification's example fence
gives (`table`, `autolink`, `strikethrough`, `tagfilter`, or `disabled` for an example the reference implementation
skips). It pairs with `GfmExtension#specTag`.

Two things about this suite are worth knowing before reading its score:

- **Its base is CommonMark 0.29, not 0.31.2.** 648 of the 672 examples are inherited, and nine of them — all in
  *Emphasis and strong emphasis* — expect the pre-0.30 reading of nested strong delimiters, where `****foo****` is one
  `strong` rather than two nested. CommonMark changed that in 0.30 and GFM has not re-published since. Passing them
  would mean regressing against CommonMark 0.31.2, so they stay recorded as failures and the practical ceiling for
  this suite is 663 of 672.
- **Two examples are marked `disabled`,** both under *Task list items*, because cmark-gfm renders the checkbox input
  with different attribute order than the prose shows. They are kept and counted; nothing stops an implementation from
  matching the prose.

## `conformance-baselines.json`

What we claim to conform to, one entry per profile:

```json
{ "profile": "CommonMark", "version": "0.31.2", "fixtures": "commonmark-0.31.2-spec.json", "passing": 652, "total": 652 }
```

A ratio against a named version, because a bare number says nothing — 652 is a different claim against a suite of 652
than against one with more. `total` is checked against the fixture file, so moving to a newer spec release means
replacing the file, renaming it, and updating `version`, `fixtures`, `total` and `passing` together. That check is
there precisely so the version cannot drift away from the claim quietly.

`profile` is not decoration either: `ConformanceTests.profileOf` maps it to the `MdProfile` the examples are parsed
under, and an unrecognised name fails the suite rather than falling back to the base grammar.

`passing` is a floor, not an expectation: the suite fails when the score drops below it and prints what to raise it to
when the score climbs. Add a profile by appending an entry, vendoring its fixtures in the same shape — `markdown`,
`html`, `example` and `section` per entry — and teaching `profileOf` its name.
