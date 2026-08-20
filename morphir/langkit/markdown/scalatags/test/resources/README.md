# Vendored conformance fixtures

`commonmark-0.31.2-spec.json` is the official CommonMark 0.31.2 example set, fetched verbatim from
<https://spec.commonmark.org/0.31.2/spec.json>. It holds 652 examples, each with the Markdown source and the exact
HTML a conformant implementation must produce.

It is vendored rather than fetched at test time so the suite is hermetic and CI never depends on spec.commonmark.org
being reachable. Do not edit it: a change here is not a change to what conformance means.

## `conformance-baselines.json`

What we claim to conform to, one entry per profile:

```json
{ "profile": "CommonMark", "version": "0.31.2", "fixtures": "commonmark-0.31.2-spec.json", "passing": 652, "total": 652 }
```

A ratio against a named version, because a bare number says nothing — 652 is a different claim against a suite of 652
than against one with more. `total` is checked against the fixture file, so moving to a newer spec release means
replacing the file, renaming it, and updating `version`, `fixtures`, `total` and `passing` together. That check is
there precisely so the version cannot drift away from the claim quietly.

`passing` is a floor, not an expectation: the suite fails when the score drops below it and prints what to raise it to
when the score climbs. Add a profile by appending an entry and vendoring its fixtures in the same shape — `markdown`,
`html`, `example` and `section` per entry — which is what a GFM suite would need.
