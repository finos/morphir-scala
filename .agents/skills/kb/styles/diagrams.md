# Diagrams

A picture earns its place when prose would spend paragraphs on relationships a reader could see at a glance.
Reach for one whenever the subject is structure, flow, or state: pipelines, lifecycles, dependency graphs,
handler nesting, module boundaries, anything with more than about three interacting parts. If you catch
yourself writing "A calls B, which feeds C, unless D", stop and draw it.

## Choosing the format

| Format | Use when | Notes |
| --- | --- | --- |
| Mermaid (fenced ` ```mermaid ` block) | Default for graphs, sequences, state machines, flowcharts | Text-diffable, renders on GitHub, and agents read the source directly |
| SVG file | Layout precision Mermaid cannot express, or hand-drawn architecture | Commit the `.svg` next to the concept or in the bundle's `assets/` directory; keep the source editable, not a raster export |
| Raster image (PNG) | Screenshots and captures only | Last resort for anything authored; it cannot be diffed or edited |

Prefer Mermaid. Its source is part of the document: version control diffs it, and a reader without a renderer
still gets the structure from the text.

## Rules

- A diagram clarifies a claim the prose makes; it does not replace the claim. State the point in a sentence,
  then show it.
- Caption every figure with a numbered caption paragraph directly after it: `Figure N: what to notice`. A
  reader should know why the figure is there without decoding it, and prose can then cite it by number.
- Make the caption linkable by prefixing an HTML anchor carrying both attributes:
  `<a id="figure-N" name="figure-N"></a>Figure N: …`. Markdown gives paragraphs no automatic anchor, and no
  single attribute works everywhere: GitHub's sanitizer strips `id` and keeps `name`, while most other
  renderers pass raw HTML through and browsers navigate by `id`. Both on one tag makes
  `[Figure N](#figure-N)` links from prose resolve everywhere. Add the anchor whenever the figure is worth
  citing; always adding it costs nothing.
- Number figures 1..N in document order. `kb check` verifies this (`figure-caption-missing`,
  `figure-number-out-of-sequence`) and that a present anchor id matches its caption number
  (`figure-anchor-mismatch`); section-aware schemes such as `Figure 2.1` are not yet supported.
- Keep one idea per diagram. A drawing that needs a legend of ten node types is three drawings.
- Label edges with the relationship ("blocks", "emits", "handles"), not just arrows. An unlabeled arrow is the
  visual form of a vague pronoun.
- Non-markdown files in a bundle are assets, not concepts; they need no frontmatter. Link them with a relative
  path and give the link text meaning.
- Diagrams follow the same honesty rules as prose: mark sketches of proposed designs as proposed, and keep a
  diagram in step with the claims around it when either changes.
