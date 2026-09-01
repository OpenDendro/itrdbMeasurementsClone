# What's actually wrong with the ITRDB .rwl files

Working notes, August 2026. Andy Bunn, openDendro.

This started as a way to put the new dplR reader through its paces. The Tucson format
has no real standard — what there is dates to the 1970s and never quite worked — so
the only honest test of a parser is the whole archive, warts and all. That was the
exercise. Getting a look at the state of the ITRDB was the other half, and the two
turned out to be the same job: the reader's disagreements *are* the archive's
problems. Couldn't tell you about one without the other.

So: every `.rwl` file in `treering/measurements`, read twice by two independent
parsers, both compared against the NOAA Template files NCEI publishes alongside them.
Here's what came out. I've written it to stand on its own, since I'll probably hand
some of it to Ed and Chris.

One thing to keep straight, and I kept getting it wrong while doing this: **most of
what looks broken in this archive isn't.** It's convention. Plenty of it is convention
I don't care for, but my taste isn't a bug report. Those two things live in separate
sections below, deliberately.

---

## What came out

| | Files |
|---|---:|
| Nothing found | 7,762 |
| Normal practice I'd do differently | 1,956 |
| Worth a second look | 332 |
| Readers disagree | 103 |
| Can't be read at all | 9 |

A file's category is the worst single thing in it, not a tally. Forty cosmetic
oddities still make a fine file. One unreadable line doesn't.

---

## Can't be read at all — 9 files

**Eight NOAA Template files are wearing a `.rwl` extension.** They're perfectly good
Template v4.0 text files. The only thing wrong is the name — anything globbing `*.rwl`
grabs them and tries to parse them as Tucson decadal data. A rename fixes all eight.

```
europe/brit048i-noaa.rwl    europe/bulg002i-noaa.rwl
europe/brit048t-noaa.rwl    europe/bulg002t-noaa.rwl
europe/brit049i-noaa.rwl    europe/roma004i-noaa.rwl
europe/brit049t-noaa.rwl    europe/roma004t-noaa.rwl
```

**One file argues with itself.** `asia/kyrg014.rwl`, core `kok3a` stops with `-9999`
in 1898, starts up again, and stops with `999` in 2005. Those mean 0.001 mm and
0.01 mm. One core, two scales, and nothing in the file to say which. One reader errors
out. The other keeps the interior marker as data and cheerfully reports a ring width
of −99.99 mm.

---

## Readers disagree — 103 files

These are the ones I'd actually send along. Two parsers, same file, different numbers.
Somebody's published value is wrong and no tool can tell you whose. All of them are
fixable by editing characters in the file.

| Problem | Files | What happens |
|---|---:|---|
| Series ID reused with overlapping years | 35 | Two measurements claim the same year under one ID |
| One ID starts a second block | 31 | Two pieces of wood sharing an identifier |
| Parsers return different values | 24 | Ambiguous columns; tools disagree |
| Value shifted out of its column | 17 | Digits straddle the 6-character boundary |
| Embedded tab character | 13 | Shifts a decade, drops its last value |
| Only one parser can open it | 12 | Usually BC dates needing the 5-column year layout |
| Two records run together | 4 | Missing line break; a whole decade vanishes |

By region: North America 51, Australia 16, Europe 13, Asia 12, South America 10,
Africa 1.

`itrdb-file-findings.csv` has file, series, year, line number and the offending
characters for every one of them.

---

## Three worked examples

### 1. One shifted digit, three tools, three answers

`southamerica/arge041.rwl`, core `RH17B`. A four-digit value has its leading digit
parked at the front of the field instead of right-aligned, so the digits straddle a
column boundary. Happens a dozen or so times in this file.

Line 261 — year in columns 9–12, then ten 6-character values:

```
RH17B   19331  185   9101  299   9721  0921  005   442
```

Nothing in that line tells you whether `1  185` is 1185, or 1 followed by 185. Three
tools, three answers, and not one of them says a word about it:

| Reading 1933–1938 | 1933 | 1934 | 1935 | 1936 | 1937 | 1938 |
|---|---:|---:|---:|---:|---:|---:|
| By fixed columns | 11.85 | 9.10 | 12.99 | 9.72 | 10.92 | 10.05 |
| By splitting on spaces | 0.01 | 1.85 | 91.01 | 2.99 | 97.21 | 9.21 |
| NOAA Template | 1.85 | 9.10 | 2.99 | 9.72 | 0.92 | 0.05 |

Only the first row is a tree — young, 8–13 mm juvenile rings, tapering. The Template
reading puts a 0.05 mm ring next to a 9.72 mm ring in the same core. Nudge those
digits one column right in the `.rwl` and every tool agrees at once.

### 2. A missing line break that's already in the Template

`northamerica/usa/az621.rwl`, line 45. Two records sharing a line because somebody
lost a newline — `FR-001`'s last decade runs straight into `FR-002`'s first.

144 characters where 72 are expected:

```
FR-001  2010    28    19    30    26    21    25    19    32    23   999FR-002  1660    63    93    70    74   142   119   131    42    53    76
```

Every tool I tested quietly drops `FR-002`'s 1660–1669 decade. Ten measurements, gone.
The series starts at 1670 everywhere you look, and `az621-rwl-noaa.txt` has `NA` for
those ten years too, so the Template inherited it. One line break fixes both.

### 3. Density files are not ring widths

Not a defect. A trap, and I walked straight into it. The Tucson container holds density
series as well as widths, and a latewood density of 0.83 g/cm³ comes back looking like
an 83 mm ring. My first pass confidently flagged 217 density files as having impossible
ring widths — which, had I sent it, would have been a genuinely embarrassing thing to
put in front of Ed. `Parameter_Keywords` in the Template file tells you what a file
actually measures. The filename suffix does not. I ended up excluding 1,947 non-width
files from the range checks.

---

## Worth a second look — 332 files

Nothing ambiguous about how these parse. The numbers are just odd enough that somebody
who knows the site should have a look.

| Finding | Count |
|---|---:|
| Very short series (≤10 yr, and doesn't crossdate with its own collection) | 117 |
| File median an order of magnitude off its genus | 111 |
| File mixes precision flags across series | 99 |
| Stop marker mid-series, series carries on immediately | 99 |
| Series median far from the rest of its file | 48 |
| File has only 1–2 series | 37 |
| Single value out of range | 11 |

The precision-mix group is the weakest of these and I nearly binned it. Precision
belongs to the series, not the file, and both parsers handle a mixed file correctly.
It's just unusual inside one collection. Take it as a nudge, not a finding.

---

## Normal practice I'd do differently — 1,956 files

These are standard practices that I wish people wouldn't do.

That's the whole claim. Not defects, not errors, nothing anybody needs to fix. I'm
writing them down because I looked at them, felt the familiar twitch, and want a
record that I decided to leave them alone.

**Interior gaps filled with zeros.** A gap usually means a stretch of core nobody
could measure — rot, a branch scar, a section that crumbled. Filling it with zeros
goes back to the original DPL programs and dplR has carried it forward ever since.
Leading and trailing zeros get used the same way.

I hate it. A zero ring width is a real observation — the tree grew nothing that year —
and a gap means nobody could read the wood. Writing the first where the second is true
throws away information you can't get back, and nothing downstream flags it because
0.00 is a perfectly legal value.

I've been making this argument for years and the archive is entirely unmoved. It's
been standard for five decades, it's what everyone's software expects, and it is
absolutely not NOAA's job to relitigate it because I don't like it. Worth remembering
that dplR is one of the programs doing it, so I've been part of the problem for most
of that time.

What I *can* fix is my own reader making the choice silently. `read.tucson2` now leaves
interior gaps as `NA`, says loudly how many it found, and tells you to pass
`fill.internal.NA = 0` if you want the old behaviour back.

**One ID carrying two separate segments.** Same hobbyhorse. I'd store them as two
series, because every parser has to guess what to do with the join and they don't all
guess alike. Plenty of people I respect disagree, and they aren't wrong.

**Formatting that reads fine anyway** — 1,786 findings, mostly series IDs filling all
eight columns so there's no space before the year. Any parser that counts columns
handles them without blinking. They only break tools that split on whitespace, which
is my problem, not the archive's.

---

## How I did this, and what I don't trust

- Every file read twice, `dplR::read.tucson` and an independent parser, compared cell
  by cell. Where they disagreed I called the file ambiguous rather than picking a
  winner.
- Structural checks run on raw text at fixed Tucson positions, so every finding cites a
  line number and the exact characters.
- Range checks are relative. No fixed ceiling on a ring width anywhere. A value only
  gets flagged if it's extreme within its own series *and* against every archive
  measurement for the same genus.
- Short series got crossdated against the rest of their own collection before being
  reported. That took 452 candidates down to 117.

Now the part I'd want someone to poke at:

**Nearly every category shrank when I audited it.** Not by a little. 60,866 → 4,903
structural findings. 3,259 → 24 reader disagreements. 452 → 117 short series.
339 → 111 genus outliers. 131 → 9 unreadable. Every single time the cause was one
systematic effect wearing the costume of a thousand separate findings, and every single
time I believed the first number until I went and looked. No reason to think I've found
the last one.

**The precision check was wrong twice before it was right.** First it flagged any file
containing both markers — wrong 122 times out of 123, because precision is a property
of the series. Then it flagged any series containing both — wrong all 53 times, because
a series measured in 0.001 mm can perfectly well contain a 0.999 mm ring. Third attempt
found the one file that's genuinely broken.

**The archive is its own reference distribution**, which means it contains the errors
I'm hunting for. Robust statistics cope with a small contaminated fraction. A systematic
error spread across many files at once would be invisible to this whole approach.

**No metadata in this pass** — coordinates, elevations, species. That's a separate list
and a separate argument.

---

## Where things are

| File | What |
|---|---|
| `itrdb-file-grades.csv` | One row per file, worst category first |
| `itrdb-file-findings.csv` | Every finding: file, series, year, line, evidence |
| `short-series-adjudicated.csv` | All 452 short-series candidates with crossdating |
| `rwl-format-problems.csv` | Raw structural layer |
| `rwl_format_checks.R` | Structural checks |
| `qa10_read_all_rwl.R` | Both readers across the archive |
| `qa20_grade_rwl.R` | Plausibility checks and categories |
| `qa30_adjudicate_short.R` | Short-series crossdating |

If any of these flags turns out to be a convention I've misread — entirely possible on
today's evidence — I'd rather fix the check than pass the noise along.
