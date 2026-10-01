# read.tucson: fail on lines that cannot be read

Working note for the dplR project. Andy Bunn, September 2026.

Everything below was measured against **dplR 1.8.0 as installed on 8 Sep 2026**,
except where a figure is marked as re-measured on 10 Sep against the dev tree;
see "What changed on 10 Sep" at the end.
over the full ITRDB clone: 10,162 `.rwl` files in
`itrdbMeasurementsClone/data_files/treering/measurements`. The scripts that
produced each number are named at the bottom. No figure here is recalled from an
earlier run.

---

## The decision

Kevin Anchukaitis sent over the files his Python reader (`dplPy`, `readers.py`)
still refuses. Running his list through `read.tucson` settled a question that had
been left open since August: **where is the line between a file that is sloppy
and a file that cannot be read?**

The rule we settled on:

> A file that **records no measurement** gets `NA` and a verbose note.
> A line the reader **cannot read unambiguously** is an **error**.

`read.tucson` today only reaches the second under `strict = TRUE`. It should
reach it by default. Two things pushed the decision:

1. **The old reader shows what guessing costs.** On four of the six misaligned
   lines Kevin flagged, `read.tucson.legacy()` silently returns the fixed-column
   reading. mng044 `KLL3093x` 1302 comes back as **9.111 mm**. russ218 `rv8e`
   1858 as **3.196 mm**. ak159 `F028Bn04` 1974–75 as 0.056 and 0.49 mm. No
   warning, no error, just wrong numbers in a data frame.

2. **Warning and carrying on can lose a whole series without saying so.**
   can697 returns 92 series of 94, and can712 returns 74 of 78. `B22B1`,
   `B22B1b`, `RA13`, `RA32`, `RA7` and `RA7b` are simply absent. The reader
   warns 27 and 61 times about the decades, but never once says a series left.

---

## What to change

### 1. Add a third severity, `fail()`

`read.tucson` already has two tiers, both defined near the top of the function
body:

| Helper | Behaviour | Records the event? |
|---|---|---|
| `note(...)` | prints when `verbose = TRUE` | yes |
| `report(...)` | warns, or `stop()`s when `strict = TRUE` | yes |

Add a third that always stops:

```r
fail <- function(..., event = NA_character_, series = NA_character_,
                 n = NA_integer_) {
  msg <- paste0(...)
  prov.events[[length(prov.events) + 1L]] <<- data.frame(
    event = event, series = series, n = n, message = msg,
    stringsAsFactors = FALSE)
  stop(msg, call. = FALSE)
}
```

Keep the `prov.events` write. A caller who catches the condition should still be
able to see the event code. There is precedent for an unconditional stop already:
a series carrying two precision markers stops whatever `strict` says.

### 2. Route `COLUMN_LAYOUT` through `fail()`

| Event code | Current | Wanted | Why |
|---|---|---|---|
| `COLUMN_LAYOUT` | `report()` | `fail()` | The fixed-column and whitespace readings disagree. The reader has two candidate readings and no basis to pick. |
| `PAST_COL72` | `report()` | `report()` — **decided 10 Sep: stays a warning** | It cannot tell its own two shapes apart. A digit in column 72 and another in column 73 is sometimes a measurement straddling the boundary — can697 `B22B1` 1889 is `140` in the file and `14` after truncation — and sometimes a stray character after a record that ends correctly at 72. Every straddle in the archive is caught by `COLUMN_LAYOUT` on the same line, so this adds nothing but two false refusals. See "`PAST_COL72` stays a warning" below. |

`COLUMN_LAYOUT` already covers the one-column record shift, so **no new check is
needed for can697 and can712**. I checked whether a dedicated shift detector was
worth writing: a census of the whole archive for a blank at column 9 followed by
a four-digit year at 10–13 finds the shift in **exactly those two files, 86 lines
in all**. Not worth a bespoke code path. A better *message* for that shape is
optional and cosmetic.

### 3. Never drop a series in silence

**Landed 10 Sep 2026.** A `SERIES_DROPPED` event, reported by name with the line
count, plus `RWL_SERIES_DROPPED` in `rwl.check()`. Archive-wide it fires on two
files and six series: can697 (`B22B1`, `B22B1b`) and can712 (`RA13`, `RA32`,
`RA7`, `RA7b`). Those are the only series the reader was losing in silence
anywhere in the archive.

Independent of severity, and worth doing even if nothing else here lands.

After the tail parse, a core can end up with every cell `NA` — every one of its
lines was refused. The column is then dropped and nothing says so. Add a check:
if a core appears in the parsed block table but contributes no non-`NA` value,
report it by name.

This is a real gap in its own right. The current output makes "the file has 94
series and you got 92" something the caller has to discover by counting.

---

## Why the whole file fails, and not just the series

This was considered and rejected, so it does not need reopening.

The alternative is to fail the offending series and return the rest. It is
tempting, because the cost of failing the file looks disproportionate.
`asia/russ218.rwl` is the clearest case: **1,376 data lines, 34 series, and two
bad lines.** Failing it refuses 34 series and 600 years in order to refuse 20
cells.

Take the file down anyway. Three reasons.

1. **A partial `rwl` is the silent-drop bug with a warning bolted on.** The whole
   complaint about can697 is that the caller gets 92 series where the file holds
   94 and has to work out which two left. Returning 32 of 34 series from russ218
   is the same object. A warning does not change what the caller is holding, and
   `rwl` objects get passed straight into `detrend()` and `chron()` without
   anyone re-reading the console.

2. **A file with two bad lines is a file someone got wrong.** The lines are not
   randomly distributed. In russ218 the same fault appears in two different
   series; in can697 and can712 it runs through whole records. Whatever produced
   the bad line had every opportunity to produce others the checks do not catch.
   Refusing the file says so. Returning most of it implies the rest was audited,
   and it was not.

3. **There is already an escape hatch, and it is honest about what it does.**
   `read.tucson.legacy()` reads all 20 of these files. It is documented as the
   reader that guesses, and russ218 shows exactly what the guess is worth: at
   `rv2s` 1598-99 it returns **0.01 mm and 2.145 mm**, one twentieth and twenty
   times the neighbouring rings, with no warning at all. A caller who needs those
   files can have them on those terms, stated up front.

The failure message has to carry its weight, then, since it is all the caller
gets. It should name the file, the series, the decade, and both candidate
readings — which the `COLUMN_LAYOUT` message already does.

---

## What this costs, measured

Across all 10,162 files:

| | Files |
|---|---:|
| Fail today | 9 |
| Raise any warning today | 343 |
| **Newly fail if `COLUMN_LAYOUT` becomes fatal** | **18** |
| Fail in total after the change | 27 (0.27% of the archive) |
| (Had `PAST_COL72` gone fatal too, as first proposed) | 20 newly, 29 total |

The blast radius is small because the noisy warning family is not one of these.
Of the 343 files that warn, **306 warn only about `SPLIT_RECORD`** — one ID
entered as several separately terminated records — and that stays a warning.

Warning families, by file count:

| Event code | Files | Proposed |
|---|---:|---|
| `SPLIT_RECORD` | 311 | stays a warning |
| `COLUMN_LAYOUT` | 26 | **fatal** |
| `TAB_IN_DATA` | 13 | undecided |
| `NON_NUMERIC` | 12 | undecided |
| `SECOND_RECORD` | 8 | undecided (costs nothing, see below) |
| `PAST_COL72` | 6 | stays a warning (decided 10 Sep) |
| `BAD_YEAR` | 2 | undecided |
| `SERIES_DROPPED` | 2 | new on 10 Sep, stays a warning |
| `DUPLICATE_LINE` | 1 | stays a warning |
| `ID_RENAMED`, `YEAR_CLASH`, `NO_MEASUREMENT`, `ENCODING` | 0 | no change |

`PAST_COL72` was 6 rather than 7 when this was re-measured on 10 Sep. The file
that left, `europe/bulg002t-noaa.rwl`, was a false positive in the check itself,
now fixed; it is one of the NOAA tables that fail anyway, so no file that reads
today changed.

**These counts include files that already fail**, which is why 26 + 6 is not 20.
The eight `europe/*-noaa.rwl` tables carry `COLUMN_LAYOUT`, `TAB_IN_DATA`,
`SECOND_RECORD` and most of `NON_NUMERIC` between them and are refused before
any of it matters. Counting only files that read: `COLUMN_LAYOUT` 18,
`PAST_COL72` 6, four of them in both, so 20 — 18 once `PAST_COL72` is left as
a warning.

The three duplicate-ID paths still fire on zero archive files, which matches what
we found in August. They are kept for shapes this archive does not contain.

---

## What must not change

- **Interior gaps.** A negative sentinel or an empty field returns `NA` with a
  verbose line naming the series, the years and what the file held. That is the
  practice change from September and it is deliberate. `fill.internal.NA = 0`
  still reproduces `read.tucson.legacy()` exactly.
- **`SPLIT_RECORD`.** One ID entered as several terminated records is allowable
  and usually means just that. 311 files do it. Warn, quote the gap, move on.
- **Sloppy files that read correctly.** Two shapes from Kevin's list belong with
  NOAA, not in the reader:
  - **az030.** Series `134203` has a decade line at 1693 carrying ten values, so
    it overruns the 1700 line by three years. All three overlapping years hold
    `-8`, the file's own sentinel, so the reader takes 1700–1702 from the later
    line and gets the right answer. It reads clean, with no warning.
  - **fl014 through fl021.** Each carries its three-line header twice. The header
    predicate counts letters in columns 9–72, so a second header block is skipped
    like the first. All eight read with no warning and return the series count and
    span their own headers claim.
- **`read.tucson.legacy()` stays exported.** It becomes the escape hatch for
  anyone who has to read one of the 18 files. It should stay documented as what
  it is: the reader that guesses.

---

## Settled 10 Sep

Four families lose data or resolve an ambiguity. All four were looked at file by
file on 10 Sep and **all four stay warnings**; the reasoning is under the table
and the working is in "The last four families" at the end. The cost column is
what making each one fatal would have cost, over and above the 18 files above:

| Event code | Extra files | The case for fatal | The case against |
|---|---:|---|---|
| `TAB_IN_DATA` | +5 | A tab has no width in a fixed-width format, so the expansion is an assumption. | **Warning.** The assumption is verified twice: none of the 5 raises `COLUMN_LAYOUT`, so expansion puts every measurement back on the 6-character grid, and 2 of the 5 then match the legacy reader cell for cell. On the 3 that do not, the new reader is right and legacy is wrong. |
| `NON_NUMERIC` | +2 | `NaN` in ausl051 and `AZP3` in az615 are not measurements. | **Warning.** Three cells archive-wide, each sitting right-aligned in its own field with the rest of the line intact. There is nothing to recover and `NA` is the only honest reading. |
| `BAD_YEAR` | +2 | Lines are discarded, and a discarded line is data loss. | **Warning**, and the data loss is now fixed rather than argued about: swe347's line is a stray header and refusing it would be flatly wrong, while va024's two were readable data the reader threw away. Those are now read (`YEAR_MISPLACED`), so `BAD_YEAR` is down to swe347 alone. See below. |
| `SECOND_RECORD` | **+0** | A record appended after a missing line break is discarded. | **Warning.** "Costs nothing" is not a reason: a cost of zero means the rule is untested, not free. All 8 files are the `europe/*-noaa.rwl` tables, which fail anyway, and az621 — the one worked example in a file that reads — no longer exists. There is nothing to validate a fatal rule against. |

---

## Regression fixtures

The 18 files that would newly fail. All paths are relative to
`itrdbMeasurementsClone/data_files/treering/measurements/`.

```
africa/mar047.rwl                asia/mng044.rwl
asia/russ218.rwl                 australia/aus127.rwl
australia/ausl053.rwl            northamerica/canada/can697.rwl
northamerica/canada/can712.rwl   northamerica/canada/cana320.rwl
northamerica/mexico/mex125.rwl   northamerica/usa/ak159.rwl
northamerica/usa/ak160.rwl       northamerica/usa/az143.rwl
northamerica/usa/az606.rwl       northamerica/usa/ok022e.rwl
northamerica/usa/ok049e.rwl      northamerica/usa/or005.rwl
northamerica/usa/sc008l.rwl      southamerica/arge065.rwl
```

`ok049` and `ok049l` were on this list until 10 Sep and are off it: they raise
only `PAST_COL72`, which stays a warning. `ok049e` stays — it is `COLUMN_LAYOUT`.
They are worth keeping as **negative** fixtures instead: a complete record with a
stray character after column 72 must warn and still return the truncated value,
which is the right one.

Worked examples worth building a test around:

| File | Series | Shape |
|---|---|---|
| can697 | `B22B1`, `B22B1b` | Record shifted one column. 26 lines, 22 past column 72. Currently returns 92 of 94 series in silence. |
| can712 | `RA13`, `RA32`, `RA7`, `RA7b` | Same shape. 60 lines, 53 past column 72. Returns 74 of 78. |
| aus127 | `SPR01C` @1690 | An over-wide field mid-line. Columns give `490 750 780 100 52 40 …`; whitespace gives `490 750 780 1000 520 40 …`. |
| mng044 | `KLL3093x` @1300 | Same, and the legacy reader returns 9.111 mm for 1302. |
| ausl053 | `LJUP9C` @1172, `JDH14B` @1200 | Same. Note `read.tucson.legacy()` fails on this file outright, so it is a case where the new reader is strictly better. |

Files that must keep reading clean, as negative tests: az030, and fl014 through
fl021.

Nine files fail today and should go on failing. `asia/kyrg014.rwl` is the mixed
precision case in `kok3a`; the other eight are the `europe/*-noaa.rwl` template
tables, which are not Tucson files at all.

---

## One loose end this does not fix

`kyrg014` `kok3a` holds two records under one ID at different precisions:
1642–1898 ending in `-9999`, then 1900–2005 ending in `999`. The decade numbers
keep ascending, so the block pass never splits them, and the reader stops with
"different precision flags" — true, and useless to the person holding the file.

The general fix is a block rule that says **a record ends at its stop marker**,
so a decade line after a terminator starts a new record, and two records that
terminate with different precision markers are two series that must not be
merged. That is a separate change and it is not covered here.

Worth knowing what the legacy reader does with it: it returns **−99.99 as the
ring width for 1898** and scales the 257 earlier years by ten. One file, one
cell, archive-wide.

---

## Provenance

| Number | Script |
|---|---|
| Per-file behaviour on Kevin's 22 files | `QA_Stuff/python_reader_cases.R` → `python-reader-cases.csv` |
| Defect list, with what each should do to a reader | same script → `itrdb-format-defects.csv` |
| Cells where the two dplR readers disagree | same script → `python-reader-cases-cells.csv` |
| Archive-wide failures, warnings, warning families | `Rdatafiles/archive_warning_families.rds`, `archive_warning_family_matrix.rds` |
| The same, re-measured 10 Sep against the dev tree | `QA_Stuff/archive_warning_families_20260910.R` -> `Rdatafiles/archive_warning_families_20260910.rds` |
| One-column shift census | `Rdatafiles/archive_shifted_records.rds` |

**AGB Oct 2026: these scripts and outputs are deleted.** The clone was rebuilt
from NOAA's API, and Andy chose not to keep the censuses. The figures in this
note stand as measured in Sep 2026 but can no longer be reproduced from this
repo. `QA_Stuff/` is now `reports/`, and this note lives in `notes/`. For
current per-file reader behaviour, see `reports/rwl-read-report.csv`, which
`code/04_readRWLs.R` writes for every file in the archive.

**A trap in the sweep script itself.** It sorts warnings into families by
matching a phrase from each message, so a reworded message shows up as its
family dropping to **zero**, not as a miscount. That is the right failure mode
and it fired on 10 Sep: `PAST_COL72`'s message was softened that afternoon and
the next sweep reported the family at 0. The phrase in
`archive_warning_families_20260910.R` is updated. If a family ever reads zero
after a reader change, check the wording before believing it.

**Two stale figures to watch for.** `PAST_COL72` is 6, not the 7 this note
carried until 10 Sep. And `CLAUDE.md` in this repo says 87 files raise a
warning archive-wide. That was measured against the development reader before
`SPLIT_RECORD` was added. The figure against shipped 1.8.0 is **343**, and 306 of
those are `SPLIT_RECORD` alone. Do not quote the 87.

---

## What changed on 10 Sep

Three defects in the reader, found while implementing item 3 above. All are in
the dev tree with tests; the archive was re-swept afterwards and the sweep is in
`Rdatafiles/archive_warning_families_20260910.rds`.

**The `PAST_COL72` predicate was wrong.** It read

```r
cutNumber <- grepl('[0-9]$', raw$V1) & grepl('^[0-9]', raw$ovf)
```

but `V1` is right-trimmed before this runs, so the first test asked whether the
last non-blank character *anywhere* in columns 1-72 was a digit. Data lines end
in a measurement, so that is nearly always true, and the predicate collapsed to
"there is a digit at column 73" -- the disposable trailing count column the
check was written to ignore. Now tested at `nchar(V1) == 72L`. Archive-wide this
is one file, `bulg002t-noaa`, which fails for other reasons; the value of the
fix is that the check is about to become fatal, not that it was costing the
archive anything.

**The `series` field in the provenance events was the wrong series.**
`COLUMN_LAYOUT`, `NON_NUMERIC` and `NO_MEASUREMENT` are raised inside a
data.table `j`-expression, which evaluates once per group in one reused
environment and writes each group's column value into one reused vector, in
place. The event row stored a reference to that vector, so every such row held
the *last* group's series id. The message was always right, because `paste0()`
copies; only the structured field was wrong -- the field a sweep filters on. Any
earlier sweep that grouped `events$series` has the wrong names. Per-file counts
are unaffected, so nothing else in this note moves.

**The `az621` example for `SECOND_RECORD` is stale.** The comment in
`read.tucson.R` cites `FR-001`/`FR-002` sharing a line. The archive copy now has
exactly one line longer than 72 characters and it is the site header; az621
reads clean with no warning. All 8 `SECOND_RECORD` files are the NOAA tables, as
this note says -- but the family now has no worked example in a file that reads,
which is worth knowing before deciding its severity on the strength of one.

### Two things this leaves for you

**The `fail()` sketch above does not do what the text claims.** It writes to
`prov.events` and then calls `stop(msg)`. `prov.events` is a local, and a
`simpleError` carries only its message, so the write is dead code and "a caller
who catches the condition should still be able to see the event code" is not
true of it. Signalling a classed condition would make it true. Separately, an
immediate `stop()` aborts at the first bad line: can697 has 26 and can712 has
60, so a caller gets one of 26 in the message that this note argues is all they
get. Collecting the fatal events and stopping once at the end fixes both.

**`PAST_COL72` was the sole cause for two of the twenty**, `ok049` and `ok049l`.
Both were looked at, and the answer changed the plan: see below.

**And one consequence worth stating.** Both `SERIES_DROPPED` files, can697 and
can712, are in the newly-fatal set. Once `COLUMN_LAYOUT` is fatal, the
silent-drop check has nothing left to fire on archive-wide. It still guards
the `NON_NUMERIC` and `NO_MEASUREMENT` paths and every file outside this
archive, which is where it earns its place -- but it is not an argument for the
severity change, and the severity change removes its only current evidence.

### `PAST_COL72` stays a warning

Looked at all six files it fires on, since it is the sole cause for two of the
twenty and its predicate had been wrong until that morning. They are two
different shapes, and the split is exactly the `COLUMN_LAYOUT` overlap.

**Four are real.** The line is misaligned, the tenth value runs past 72, and the
truncated value is out of range for its own series while the joined value fits:

```
mar047 TIZ19A 1900  ...   43   104    11|6   -> 116, neighbours 29-208
mar047 TIZ02A 1880  ...  279   244    23|3   -> 233, neighbours 85-279
mex125 NIH14B 1990  ... 2910  3840   338|0   -> 3380, neighbours 2570-6940
can697 B22B1  1880  ...  110   160    14|0   -> 140, neighbours 110-550
can712 RA13   1810  ...  170  1060    87|0   -> 870, neighbours 470-1290
```

All four also raise `COLUMN_LAYOUT` on the same line. mex125's is visible in the
raw text as a doubled space at `2570   4580`; can697 and can712 are the
one-column shift.

**Two are a stray character after a record that ends correctly.** These are the
two files where `PAST_COL72` fires alone:

```
ok049  LIN152A 1830  353  835  434  390  443  267  463  372  425  303|5
ok049l LIN157A 2000  177  750  400  388  596  362  193  431 1135  815|2
```

Three things say the record is complete at column 72 and the trailing character
is junk. The ten fields are perfectly aligned on the 6-character grid, which is
why `COLUMN_LAYOUT` does not fire. The file's own format is 72 columns padded to
82 with blanks, and the overflow is blank on **4,925 of 4,926 data lines** in
ok049 and 4,962 of 4,963 in ok049l — these are the only two lines in either file
with anything out there. And the magnitudes: 303 sits inside its series
(neighbours 88–658) while 3035 would be five to ten times anything nearby; 815
sits inside its series (141–856) while 8152 is absurd. Both readers already
return 0.303 and 0.815, and `read.tucson.legacy()` agrees.

So `PAST_COL72` has **no independent true positive anywhere in the archive**.
Every genuine straddle is caught by `COLUMN_LAYOUT` on the same line. Making it
fatal buys nothing and costs two files — 443 and 446 series, 889 in all, the
largest in the set — refused on the strength of two stray keystrokes, with a
message that misdiagnoses them.

**Decided: `PAST_COL72` stays a warning and `COLUMN_LAYOUT` carries the
fatality.** Newly-fatal 20 → 18, total 27 (0.27%). The only honest fatal form
would be "fatal when the line also fails column conformance", which is
`COLUMN_LAYOUT`, so it collapses to the same rule. A check that cannot tell its
own two shapes apart has not earned the right to refuse a file, and this one is
wrong on both files where it is the only voice.

**Done regardless:** the message no longer asserts that a digit was lost. It
gives both readings, says the reader takes the shorter one, shows what is past
column 72 so the caller can see it, and points at the neighbouring years as the
way to tell. What cannot be told from the file, it does not claim.

### The last four families

`SECOND_RECORD` is dealt with in the table: its only evidence went away with
az621, and a rule with nothing to test it against should not be made fatal
because it happens to be cheap. The other three were read file by file.

**`TAB_IN_DATA` — warning.** Five files that read: ausl045, grc034, prt004,
cana588, va024. None of the five raises `COLUMN_LAYOUT`, which is the structural
check: after expansion every measurement lands on the format's 6-character grid.
Compared against `read.tucson.legacy()` with `fill.internal.NA = 0`, two
(grc034, cana588) agree cell for cell — 45,745 cells, zero differences. Three
disagree, and in every one the new reader is right:

- **ausl045** has exactly one tab in the whole file, between the 7-character id
  `mrr08nw` and the year on its 1840 line. It expands to exactly one space and
  the year lands in columns 9–12 like every other line. Legacy treats the file
  as tab-delimited and loses the entire 1840s decade — ten measurements.
- **prt004** has exactly one tab, mid-line on `WO04S2` 1910–2019, after the
  seventh measurement. It expands to exactly the field boundary. Legacy shifts
  the last three years by one and invents a zero at 2017.
- **va024** agreed with legacy until the misplaced-year recovery landed later
  the same day. It now differs by exactly 20 cells: the two shifted lines that
  both readers used to throw away. Legacy still loses them.

So the expansion is verified structurally, verified against an independent
implementation on three files, and demonstrably better than that implementation
on the other two. Making it fatal would refuse five files that are read
correctly, two of which nothing else reads correctly at all.

**`NON_NUMERIC` — warning.** Two files beyond the eighteen, three cells in
total:

```
proc53   1860   291   NaN   279   426    64   118    91   151    87   141
AZP0310A 1990  1328   650  AZP3   703   942   886   361   684   892   821
AZP0310B 1950  1273  1452  1874  1143  1409   967  1481  AZP3  1730  1100
```

Each token sits right-aligned in its own six-character field with every other
field on the line intact and aligned — which is why `COLUMN_LAYOUT` does not
fire. There is no second reading to weigh and nothing to recover: the cell is
not a number, `NA` is what it is, and the reader says so. Refusing 53 and 28
series over three correctly-reported absences is the wrong trade.

**`BAD_YEAR` — warning, and there is a defect underneath it.** The two files are
not the same shape.

- **swe347**'s discarded line is `swed347 3 Lie`, a third header line that the
  letter count lets through. Discarding it is correct and nothing is lost.
  Making this fatal would refuse a clean file over a site name.
- **va024**'s two discarded lines are data:

```
....|....|....|....|....|....|
25B     1833   324   386   368     <- normal: id 8 wide, year at 9-12
25B         1930   108    97    70    65    65    54    54    47    37
12A         1940   336   430   292   193   162   144    51    80    94
```

  The id field is written twelve wide instead of eight, so the year sits at
  columns 13–16 and the year field reads as blank. These are the **only** lines
  for 25B's 1930s and 12A's 1940s, so the file comes back with a fabricated
  ten-year hole in each of those series where the file plainly has data.
  Nineteen measurements. Legacy loses them too, without saying so.

That is not an argument for refusing va024 — refusing it costs 52 series to
avoid losing 19 cells, and would refuse swe347 for no reason at all. It is an
argument for **reading the line**. And this shape is not the `COLUMN_LAYOUT`
ambiguity: there are not two candidate readings competing, because the
fixed-column reading yields no year at all. There is exactly one reading —
tokens `25B`, `1930`, then nine numbers — and it is the one any human makes at a
glance.

**Built, 10 Sep.** When the year field does not parse, and a whitespace split
gives an id starting at column 1, a four-digit year, and between one and ten
integers after it, none too wide for the grid, the line is rebuilt into
canonical form and read; the event is `YEAR_MISPLACED`. Anything else is still
discarded, so swe347 is unaffected and `BAD_YEAR` is now that file alone.
va024's twenty measurements come back and the two fabricated ten-year holes are
gone.

Two things the build turned up that are worth recording:

- **The lines are rebuilt, not patched.** The tail parser reads columns 13–72,
  so on a shifted line the measurements are not where it looks either. Fixing
  only the year would have produced a line the rest of the reader misreads.
- **The recovery has to work from the untruncated line, and did not at first.**
  An over-wide id pushes the whole record right, so va024's tenth measurement
  sits at columns 71–76 and the truncation at column 72 had already taken it:
  the first working version returned nine values where the file holds ten, and
  said nothing. The overflow past column 72 is now kept until after the head
  parse. Worth remembering that the truncation happens early and quietly, and
  that anything reading a shifted line has to account for it.

The narrow predicate has one known limit: a shifted line that also carried a
trailing per-line count column would read the count as a measurement, because
once the record is off its columns nothing separates the two. No archive file
has both at once, and the ten-value ceiling rules out the common shape — a full
decade plus a count.
