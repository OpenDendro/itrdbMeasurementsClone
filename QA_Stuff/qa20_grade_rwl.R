## Pass 3 and 5 of the ITRDB file sweep: plausibility checks, then grades
## AGB -- Aug 2026
##
## AGB Aug 2026: takes the structural findings from rwl_format_checks.R and the
## reader summaries from qa10_read_all_rwl.R, adds checks on the numbers
## themselves, and grades every file that raises a concern.
##
## The grade is the WORST finding in a file, never a count of findings. A file
## with forty cosmetic oddities is still a clean file; one with a single
## unreadable line is not.
##
##   F  unreadable, or self-contradictory in a way no reader can resolve
##   D  the readers disagree, so some currently published number is wrong
##   C  reads unambiguously, but the numbers or the structure look wrong
##   B  non-standard, but every column-based reader gets the right answer
##
## Only C, D and F go to NOAA. B is kept here because it matters for reader
## work -- those files break whitespace-splitting readers such as dplPy -- but
## there is nothing in them for a data manager to fix, and including them would
## bury the real cases under roughly 1,900 rows of "formatted unusually".
##
## Writes QA_Stuff/itrdb-file-findings.csv (everything, all grades)
##        QA_Stuff/itrdb-file-grades.csv   (one row per file, worst grade first)

suppressPackageStartupMessages({ library(data.table) })

ROOT <- "data_files/treering/measurements"
load("RdataFiles/qa_read_all.Rdata")   # qa_files, qa_warn, qa_series, qa_ex
fmt <- as.data.table(read.csv("QA_Stuff/rwl-format-problems.csv", stringsAsFactors = FALSE))

findings <- list()
add <- function(file, series, year, grade, check, evidence)
  findings[[length(findings) + 1L]] <<- data.table(
    file = file, series = series, year = year,
    grade = grade, check = check, evidence = evidence)

## ---- species, per file ----------------------------------------------------
## From each file's own NOAA sidecar, not from cleaned_itrdb.Rdata: that
## checkpoint is keyed by study and 02_getRWLs.R kept only one .rwl per study,
## so it has no row for the earlywood/latewood variants. 10,150 of the 10,162
## files have a sidecar. The rwl header is the fallback.
get_species <- function(rel) {
  p <- file.path(ROOT, sub("[.]rwl$", "-rwl-noaa.txt", rel))
  if (file.exists(p)) {
    L <- readLines(p, warn = FALSE, n = 400)
    s <- grep("^#   Species_Name:", L, value = TRUE)
    if (length(s)) {
      nm <- trimws(sub("^#   Species_Name:", "", s[1]))
      if (nzchar(nm)) return(nm)
    }
  }
  f <- file.path(ROOT, rel)
  if (file.exists(f)) {
    L <- gsub("\r$", "", readLines(f, warn = FALSE, n = 2))
    if (length(L) > 1) return(trimws(substr(L[2], 23, 30)))
  }
  NA_character_
}
qa_files[, species := vapply(file, get_species, character(1))]
## AGB Aug 2026: only take a genus from something that actually looks like a
## binomial. The rwl-header fallback is an 8-character field, so it yields
## truncated common names -- "Atlantic", "White Sp", "Douglas-" -- and treating
## those as genera invents reference groups out of nothing.
qa_files[, genus := ifelse(grepl("^[A-Z][a-z]+ [a-z]", species),
                           sub("[^A-Za-z].*$", "", species), NA_character_)]

## AGB Aug 2026: what does this file actually MEASURE? The Tucson container is
## used for density as well as ring width, and a latewood density of 0.83 g/cm3
## comes back looking like an 83 mm ring. On the first run that put 217 density
## files into the out-of-range list -- 64% of it -- alongside the real hits.
## The range tiers below therefore run on width files only. A density file is
## not a defect and there is nothing for NOAA to fix in one.
get_param <- function(rel) {
  p <- file.path(ROOT, sub("[.]rwl$", "-rwl-noaa.txt", rel))
  if (!file.exists(p)) return(NA_character_)
  L <- readLines(p, warn = FALSE, n = 400)
  s <- grep("^#   Parameter_Keywords:", L, value = TRUE)
  if (!length(s)) return(NA_character_)
  tolower(trimws(sub("^#   Parameter_Keywords:", "", s[1])))
}
qa_files[, param := vapply(file, get_param, character(1))]
qa_files[, is_width := !is.na(param) & grepl("width", param) & !grepl("densit", param)]
cat("files measuring ring width:", sum(qa_files$is_width),
    " other parameter (density etc):", sum(!qa_files$is_width), "\n")

## ---- F: cannot be read, or contradicts itself -----------------------------
qa_files[old_ok == FALSE & new_ok == FALSE, {
  for (i in seq_len(.N)) add(file[i], NA, NA, "F", "unreadable by either reader",
    paste0("read.tucson: ", old_err[i], " | read.tucson2: ", new_err[i]))
}]
## Conflicting precision within one series: read.tucson2() refuses the file and
## names the core. That verdict is better than anything the raw-text pass can
## work out, because it knows where each block ends. See the note in
## rwl_format_checks.R for the two weaker rules this replaced.
qa_files[!is.na(new_err) & grepl("precision flag", new_err), {
  for (i in seq_len(.N)) add(file[i], NA, NA, "F",
    "conflicting precision flags in one series", new_err[i])
}]

## Eight NOAA Template files carry a .rwl extension. Graded F: nothing reading
## *.rwl can use them, and the fix is a rename, not an edit.
for (i in which(fmt$check == "NOAA template file with a .rwl extension"))
  add(fmt$file[i], NA, NA, "F", fmt$check[i], fmt$detail[i])

## ---- D: the readers disagree ----------------------------------------------
## AGB Aug 2026: "one ID starts a second block" belongs here, not with the
## cosmetic findings. Two pieces of wood sharing an identifier is exactly the
## duplicate-ID case, and neither reader mentions it: read.tucson2() renames
## only when the blocks' decades overlap, and read.tucson() welds them together
## and fills the join with zeros.
dcheck <- c("value shifted out of column", "embedded tab", "lines run together",
            "one ID starts a second block",
            "series ID reused with overlapping years",
            "stop marker mid-series, structure unclear")
for (i in which(fmt$check %in% dcheck))
  add(fmt$file[i], fmt$series[i], fmt$year[i], "D", fmt$check[i], fmt$detail[i])

## AGB Aug 2026: only the disagreements that mean the FILE is ambiguous count
## here. read.tucson() filling an interior gap with zeros while read.tucson2()
## leaves NA is a difference between the two readers, not a defect in the file,
## and it affects roughly a third of the archive. Counting it as D would have
## made the grade meaningless. The underlying gap is already reported as C.
qa_files[!is.na(dis_value) & (dis_value + dis_oldonly + dis_newonly) > 0, {
  for (i in seq_len(.N)) {
    bits <- c(if (dis_value[i])   sprintf("%d cell(s) read differently by both", dis_value[i]),
              if (dis_oldonly[i]) sprintf("%d non-zero value(s) read.tucson2 refused as ambiguous", dis_oldonly[i]),
              if (dis_newonly[i]) sprintf("%d value(s) read.tucson missed", dis_newonly[i]))
    add(file[i], NA, NA, "D", "readers return different values", paste(bits, collapse = "; "))
  }
}]
qa_files[xor(old_ok, new_ok) & !is.na(old_ok) & !is.na(new_ok), {
  for (i in seq_len(.N)) add(file[i], NA, NA, "D", "only one reader can open it",
    paste0("read.tucson ", ifelse(old_ok[i], "read it", "failed"),
           "; read.tucson2 ", ifelse(new_ok[i], "read it", "failed")))
}]

## ---- C: reads cleanly, but the data looks wrong ---------------------------
## A stop marker with the series continuing immediately: the terminator looks
## stray. Graded C -- worth a glance, not a corrupted number.
for (i in which(fmt$check == "stop marker mid-series, series continues immediately"))
  add(fmt$file[i], fmt$series[i], fmt$year[i], "C", fmt$check[i], fmt$detail[i])

## AGB Aug 2026: interior gaps and one-ID-two-segments are graded B, not C.
## They are legal and common. Andy would rather they were two series; others
## disagree, and NOAA is not the referee. They stay in the CSV for our own use
## and never reach the report. The evidence line still carries how many zeros
## read.tucson fabricates, because that is the cost to a reader.
zf <- qa_files[, .(file, dis_zerofill)]
gap <- merge(as.data.table(fmt[fmt$check == "interior year gap", ]), zf, by = "file", all.x = TRUE)
for (i in seq_len(nrow(gap)))
  add(gap$file[i], gap$series[i], gap$year[i], "B", "interior year gap",
      paste0(gap$detail[i],
             if (!is.na(gap$dis_zerofill[i]) && gap$dis_zerofill[i] > 0)
               sprintf(" (read.tucson fabricates %d zero(s) in this file)", gap$dis_zerofill[i]) else ""))

## duplicate series IDs that read.tucson2 had to rename
if (nrow(qa_warn)) {
  dup <- qa_warn[grepl("renamed|Identical rows|measured", msg)]
  for (i in seq_len(nrow(dup)))
    add(dup$file[i], NA, NA, "C", "duplicate series IDs",
        substr(gsub("[[:space:]]+", " ", dup$msg[i]), 1, 300))
}

## AGB Aug 2026: files mixing 999 and -9999 across DIFFERENT series. Both
## readers handle this correctly, so it is not an error and it is graded C, not
## F -- see the history in rwl_format_checks.R. It is reported because precision
## varying inside one collection is unusual and cheap for a person to glance at,
## and the wording says plainly that it may well be intentional.
mixchk <- fmt[fmt$check == "file mixes precision flags across series", ]
for (i in seq_len(nrow(mixchk)))
  add(mixchk$file[i], NA, NA, "C", "file mixes precision flags across series",
      paste0(mixchk$detail[i],
             " This may be intentional: precision is a property of the series, ",
             "not the file, and both readers scale it correctly. Flagged for a look, not as an error."))

## impossible values
bad <- qa_series[min < 0]
for (i in seq_len(nrow(bad)))
  add(bad$file[i], bad$series[i], NA, "C", "negative ring width",
      sprintf("minimum value %.3f mm", bad$min[i]))

## very short series, and files carrying almost nothing
## AGB Aug 2026: short series come from qa30, already adjudicated. Only the
## "investigate" tier is reported: series that are short relative to their own
## file AND do not crossdate against the rest of it. The raw count was 452,
## which is the sort of number that gets a list ignored.
adj <- "QA_Stuff/short-series-adjudicated.csv"
if (file.exists(adj)) {
  sa <- as.data.table(read.csv(adj, stringsAsFactors = FALSE))[tier == "investigate"]
  for (i in seq_len(nrow(sa)))
    add(sa$file[i], sa$series[i], sa$first[i], "C", "very short series",
        sprintf("%d year(s), %d-%d; file median span %.0f; crossdates at r = %s over %s years",
                sa$span[i], sa$first[i], sa$last[i], sa$med_span[i],
                ifelse(is.na(sa$r[i]), "NA", sprintf("%.2f", sa$r[i])),
                ifelse(is.na(sa$n_overlap[i]), "?", sa$n_overlap[i])))
} else warning("run qa30_adjudicate_short.R first; short series not reported")
thin <- qa_files[!is.na(n_series) & n_series <= 2]
for (i in seq_len(nrow(thin)))
  add(thin$file[i], NA, NA, "C", "file has 1-2 series",
      sprintf("%d series, %d measurements", thin$n_series[i], thin$n_cells[i]))

## ---- C: values out of range, three tiers, in log space --------------------
## No fixed ceiling on a ring width. Each test is relative, and each finding
## carries the numbers that produced it so a reader can judge it themselves.
qa_series <- merge(qa_series, qa_files[, .(file, genus, is_width)], by = "file", all.x = TRUE)
qa_series <- qa_series[is_width == TRUE]   # range checks apply to ring width only
file_med  <- qa_series[, .(file_med = stats::median(med, na.rm = TRUE),
                           n_ser = .N), by = .(file, genus)]
genus_ref <- file_med[!is.na(genus), .(genus_med = stats::median(file_med, na.rm = TRUE),
                                       n_files = .N), by = genus]
file_med  <- merge(file_med, genus_ref, by = "genus", all.x = TRUE)

## tier 3: a whole file an order of magnitude off its genus -- the signature of
## a precision flag read the wrong way round. Needs a genus with enough files
## to be a reference at all.
t3 <- file_med[!is.na(genus_med) & n_files >= 20 &
               (file_med > 8 * genus_med | file_med < genus_med / 8)]
for (i in seq_len(nrow(t3)))
  add(t3$file[i], NA, NA, "C", "file median far from genus median",
      sprintf("file median %.3f mm vs %.3f mm for %s (%d files); ratio %.1fx",
              t3$file_med[i], t3$genus_med[i], t3$genus[i], t3$n_files[i],
              t3$file_med[i] / t3$genus_med[i]))

## tier 2: one series an order of magnitude off the rest of its own file
qa_series <- merge(qa_series, file_med[, .(file, file_med, n_ser)], by = "file", all.x = TRUE)
t2 <- qa_series[n_ser >= 5 & !is.na(file_med) & file_med > 0 &
                (med > 10 * file_med | med < file_med / 10)]
for (i in seq_len(nrow(t2)))
  add(t2$file[i], t2$series[i], NA, "C", "series median far from rest of file",
      sprintf("series median %.3f mm vs %.3f mm for the file; ratio %.1fx",
              t2$med[i], t2$file_med[i], t2$med[i] / t2$file_med[i]))

## tier 1: a single value extreme within its own series AND beyond anything
## normal for the genus. Both conditions are required: a big release ring after
## a disturbance satisfies the first on its own and is perfectly real.
gmax <- qa_series[!is.na(genus), .(hi = stats::quantile(max, 0.999, na.rm = TRUE),
                                   n_ser = .N), by = genus][n_ser >= 50]
qa_series <- merge(qa_series, gmax[, .(genus, hi)], by = "genus", all.x = TRUE)
t1 <- qa_series[!is.na(hi) & !is.na(out_z) & out_z > 8 & out_val > hi]
for (i in seq_len(nrow(t1)))
  add(t1$file[i], t1$series[i], t1$out_year[i], "C", "single value out of range",
      sprintf("%.3f mm in %d: %.1f robust SD from this series' median, and above the 99.9th percentile (%.3f mm) for %s",
              t1$out_val[i], t1$out_year[i], t1$out_z[i], t1$hi[i], t1$genus[i]))

## ---- B: non-standard but read correctly by any column-based reader --------
## AGB Aug 2026: the precision-mix check is tagged informational in
## rwl_format_checks.R but is reported as C above, with its own hedged wording.
## Skip it here or every such file gets counted twice.
for (i in which(fmt$check == "one ID carries two separate segments"))
  add(fmt$file[i], fmt$series[i], fmt$year[i], "B", fmt$check[i], fmt$detail[i])

for (i in which(fmt$severity == "informational" &
                fmt$check != "file mixes precision flags across series"))
  add(fmt$file[i], fmt$series[i], fmt$year[i], "B", fmt$check[i], fmt$detail[i])

## ---- assemble -------------------------------------------------------------
F_all <- rbindlist(findings, fill = TRUE)
## AGB Aug 2026: "one ID starts a second block" and "series ID reused with
## overlapping years" are two detectors for the same defect and fire together on
## files like bt006. Keep one row per file/series/check-pair so the count is
## defects, not detections.
F_all <- unique(F_all, by = c("file", "series", "check"))
dupboth <- F_all[check == "series ID reused with overlapping years",
                 .(file, series)]
F_all <- F_all[!(check == "one ID starts a second block" &
                 paste(file, series) %in% dupboth[, paste(file, series)])]
ord <- c("F", "D", "C", "B")
F_all[, grade := factor(grade, levels = ord)]
setorder(F_all, grade, file, check, series)

grades <- F_all[, .(grade = min(as.integer(grade)),
                    n_findings = .N,
                    checks = paste(sort(unique(as.character(check))), collapse = "; ")),
                by = file]
grades[, grade := ord[grade]]
grades <- merge(grades, qa_files[, .(file, species, n_series, n_cells, yr_first, yr_last)],
                by = "file", all.x = TRUE)
grades[, grade := factor(grade, levels = ord)]
setorder(grades, grade, -n_findings, file)

## AGB Aug 2026: the letters stay internal to this script, where they are just a
## sort key. What goes into the CSV is the category spelled out, matching the
## write-up, and the column is called category rather than grade. A single
## letter reads as a verdict on whoever contributed the file, and most of what
## is in here is not a fault -- the largest group is ordinary practice. Anyone
## opening the CSV cold should be able to see what a row means without a key.
categories <- c(F = "Can't be read at all",
                D = "Readers disagree",
                C = "Worth a second look",
                B = "Normal practice, flagged for reference")
relabel <- function(d) {
  d[, category := factor(unname(categories[as.character(grade)]),
                         levels = unname(categories))]
  d[, grade := NULL]
  setcolorder(d, c("file", intersect(c("series", "year"), names(d)), "category"))
  d[]
}
F_all  <- relabel(F_all)
grades <- relabel(grades)

write.csv(F_all,  "QA_Stuff/itrdb-file-findings.csv", row.names = FALSE)
write.csv(grades, "QA_Stuff/itrdb-file-grades.csv",   row.names = FALSE)

cat("files graded:", nrow(grades), "of", nrow(qa_files), "\n\n")
print(table(grades$category))
cat("\nWorth someone looking at:",
      sum(grades$category != "Normal practice, flagged for reference"), "files\n\n")
print(F_all[, .N, by = .(category, check)][order(category, -N)])
