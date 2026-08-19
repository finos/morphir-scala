# Vendored CommonMark fixtures

`commonmark-0.31.2-spec.json` is the official CommonMark 0.31.2 example set, fetched verbatim from
<https://spec.commonmark.org/0.31.2/spec.json>. It holds 652 examples, each with the Markdown source and the exact
HTML a conformant implementation must produce.

It is vendored rather than fetched at test time so the suite is hermetic and CI never depends on spec.commonmark.org
being reachable. Do not edit it: a change here is not a change to what conformance means. To move to a newer spec
release, replace the whole file, rename it to match, and expect `conformance-baseline.txt` to move with it.
