# Vendored specification sources

Each file here is byte-identical to what its publisher serves. Do not edit one: a change here is not a change to
what conformance means.

| File | Source |
| --- | --- |
| `commonmark-0.31.2-spec.json` | <https://spec.commonmark.org/0.31.2/spec.json> — 652 examples, read directly by the harness |
| `commonmark-0.31.2-spec.txt` | <https://spec.commonmark.org/0.31.2/spec.txt> — the converter's oracle |
| `gfm-0.29-spec.txt` | <https://raw.githubusercontent.com/github/cmark-gfm/master/test/spec.txt> — 672 examples |

`conformance-baselines.json` is ours, not upstream's: it records what we claim per profile.

GitHub publishes no JSON form of its specification, so `gfm-0.29-spec.json` is **derived by the build** —
`markdownConformanceFixtures` in `testing.mill` — and never committed. That task proves the converter before it
uses it: run over `commonmark-0.31.2-spec.txt`, the converter must reproduce `commonmark-0.31.2-spec.json` entry
for entry, or the task fails.

Derived entries carry one field the CommonMark set does not: `extension`, holding the name the specification's
example fence gives (`table`, `autolink`, `strikethrough`, `tagfilter`, or `disabled` for an example the reference
implementation skips). It pairs with `MdExtension#specTag`.

Two things about the GFM suite are worth knowing before reading its score:

- **Its base is CommonMark 0.29, not 0.31.2.** 648 of the 672 examples are inherited, and nine of them — all in
  *Emphasis and strong emphasis* — expect the pre-0.30 reading of nested strong delimiters, where `****foo****` is
  one `strong` rather than two nested. CommonMark changed that in 0.30 and GFM has not re-published since. Passing
  them would mean regressing against CommonMark 0.31.2, so they stay recorded as failures and the practical
  ceiling for this suite is 663 of 672.
- **Two examples are marked `disabled`,** both under *Task list items*, because cmark-gfm renders the checkbox
  input with different attribute order than the prose shows. They are kept and counted; nothing stops an
  implementation from matching the prose.

## `conformance-baselines.json`

What we claim to conform to, one entry per profile:

```json
{ "profile": "CommonMark", "version": "0.31.2", "fixtures": "commonmark-0.31.2-spec.json", "passing": 652, "total": 652 }
```

A ratio against a named version, because a bare number says nothing — 652 is a different claim against a suite of
652 than against one with more. `total` is checked against the fixture file, so moving to a newer spec release
means replacing the file, renaming it, and updating `version`, `fixtures`, `total` and `passing` together. That
check is there precisely so the version cannot drift away from the claim quietly.

`profile` is not decoration either: `ConformanceTests.profileOf` maps it to the `MdProfile` the examples are parsed
under, and an unrecognised name fails the suite rather than falling back to the base grammar.

`passing` is a floor, not an expectation: the suite fails when the score drops below it and prints what to raise it
to when the score climbs. Add a profile by appending an entry, vendoring its fixtures in the same shape —
`markdown`, `html`, `example` and `section` per entry — and teaching `profileOf` its name.
