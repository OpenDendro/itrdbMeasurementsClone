## Structural checks on Tucson (.rwl) files
## AGB -- Aug 2026
##
## AGB Aug 2026: written after finding that dplR::read.tucson, read.tucson2 and
## NOAA's own .rwl-to-template converter each return DIFFERENT numbers for the
## same malformed files, all without complaint. The point is not to parse the
## data. It is to say, per file, why a reader might be guessing.
##
## Every check works on the raw text at the fixed Tucson positions:
##   cols 1-8   series ID
##   cols 9-12  decade year
##   cols 13-78 eleven 6-character measurement fields
##
## Source this file to get rwl_format_check(); run it directly to sweep the
## whole clone and write QA_Stuff/rwl-format-problems.csv.
##
## AGB Aug 2026: every row carries a severity, because the raw counts mislead
## on their own. "corrupts data" means read.tucson returns numbers that are not
## in the file. "informational" means the file is non-standard but read.tucson
## still gets it right; those rows break OTHER readers, so they matter for
## read.tucson2 and dplPy but are not worth reporting to NOAA as data errors.
## "accepted practice" means the structure is legal and common even though it
## costs the reader something -- interior gaps, and one ID carrying two
## time-separated segments. Andy would rather those were two series; other
## dendrochronologists would not, and it is not NOAA's job to settle that. They
## stay in the CSV as context and are never sent as findings.
rwl_severity <- c(
  "value shifted out of column" = "corrupts data",
  "series ID reused with overlapping years"              = "corrupts data",
  "stop marker mid-series, series continues immediately"  = "corrupts data",
  "stop marker mid-series, structure unclear"             = "corrupts data",
  "one ID carries two separate segments"                  = "accepted practice",
  "interior year gap"                                     = "accepted practice",
  "one ID starts a second block" = "corrupts data",
  "lines run together"          = "corrupts data",
  "embedded tab"                = "corrupts data",
  "ID fills all 8 columns"      = "informational",
  "blank inside series ID"      = "informational",
  "trailing text in field 11"   = "informational",
  "file mixes precision flags across series" = "informational",
  "NOAA template file with a .rwl extension" = "cannot read",
  "unreadable"                  = "cannot read",
  "no data lines"               = "cannot read")

## ---- one file -------------------------------------------------------------
rwl_format_check <- function(f) {

  field_start <- seq(13, by = 6, length.out = 11)
  field_end   <- seq(18, by = 6, length.out = 11)

  ## R's scan() silently deletes blanks inside a numeric field, so read.tucson
  ## turns "1  185" into 1185. We copy that on purpose: the check should see
  ## what read.tucson sees.
  field_value <- function(f) suppressWarnings(as.numeric(gsub("[[:space:]]", "", f)))

  hits <- list()
  add <- function(series, year, check, detail)
    hits[[length(hits) + 1L]] <<- data.frame(
      file = f, series = series, year = year, check = check, detail = detail,
      stringsAsFactors = FALSE)

  all_lines <- tryCatch(gsub("\r$", "", readLines(f, warn = FALSE)),
                        error = function(e) NULL)

  ## AGB Aug 2026: eight files in the archive are NOAA Template v4.0 text files
  ## carrying a .rwl extension (brit048i/t, brit049i/t, bulg002t, roma004i and
  ## friends). They are not malformed Tucson -- they are correctly formed
  ## templates that anything globbing for *.rwl will try to read as decadal
  ## data. That is a filing problem, reported as itself, and the Tucson checks
  ## below are skipped so the file is not also graded as broken.
  if (!is.null(all_lines) && length(all_lines) &&
      grepl("^#", all_lines[1]) && any(grepl("NOAA|Template Version", head(all_lines, 20)))) {
    return(data.frame(file = f, series = NA_character_, year = NA_integer_,
                      check = "NOAA template file with a .rwl extension",
                      detail = paste("this is a NOAA Template text file, not a Tucson decadal",
                                     "file; anything reading *.rwl will mis-parse it"),
                      stringsAsFactors = FALSE))
  }
  if (is.null(all_lines)) {
    return(data.frame(file = f, series = NA_character_, year = NA_integer_,
                      check = "unreadable", detail = "readLines() failed",
                      stringsAsFactors = FALSE))
  }

  ## Data lines are the ones with a plausible year in cols 9-12. This drops the
  ## 3 header lines and the "####" comment lines CDendro writes into some files.
  ## AGB Aug 2026: the year field must be DIGITS, not merely coercible. R turns
  ## as.integer(" Nan") into 0, which let header lines such as "NANGPW 1 Nangay"
  ## through as data and produced thousands of false hits on the first sweep.
  ##
  ## AGB Aug 2026: years before -999 need five columns, so they take column 8
  ## from the ID -- this is what dplR's long = TRUE does, and it is why the
  ## Tucson format cannot hold an 8-character ID and a BC date at the same time.
  ## The minus sign in column 8 is the only thing that disambiguates the two
  ## layouts, so that is what we key on, exactly as read.tucson2 does. Reading
  ## columns 9-12 blindly drops the sign: on the first sweep egy001 reported
  ## gaps ending in the year 2137 rather than -2137. 22 files in the clone are
  ## affected.
  ## AGB Aug 2026: a data line carries almost no letters after column 8. Without
  ## this, russ260's header line -- the site code is literally "50km", so the
  ## line reads "50km   1 54 km from Ust..." -- puts " 54 " in the year field
  ## and the whole header is graded as a malformed data line. Same heuristic
  ## read.tucson2 uses, and for the same reason. Three letters is the tolerance:
  ## some files legitimately carry NaN or a stray flag in a measurement field.
  n_alpha <- vapply(gregexpr("[[:alpha:]]", substr(all_lines, 9, 72)),
                    function(m) if (m[1] == -1L) 0L else length(m), integer(1))

  is_long <- substr(all_lines, 8, 8) == "-"
  yrtxt <- ifelse(is_long, substr(all_lines, 8, 12), substr(all_lines, 9, 12))
  idtxt <- ifelse(is_long, substr(all_lines, 1, 7),  substr(all_lines, 1, 8))
  yr <- suppressWarnings(as.integer(yrtxt))
  is_data <- grepl("^[[:space:]]*-?[0-9]+[[:space:]]*$", yrtxt) &
             !is.na(yr) & yr > -8000 & yr < 2500 & n_alpha <= 3L
  if (!any(is_data)) {
    return(data.frame(file = f, series = NA_character_, year = NA_integer_,
                      check = "no data lines", detail = "nothing parsed at cols 9-12",
                      stringsAsFactors = FALSE))
  }
  dat <- all_lines[is_data]; dyr <- yr[is_data]
  did <- trimws(idtxt[is_data]); lineno <- which(is_data)

  ## AGB Aug 2026: fields are pulled as one character matrix (one column per
  ## Tucson field) rather than line by line. Over 10,000 files the loop version
  ## was ~25x slower.
  nl   <- length(dat)
  FLD  <- vapply(seq_len(11L), function(j) substr(dat, field_start[j], field_end[j]),
                 character(nl))
  dim(FLD) <- c(nl, 11L)
  VAL  <- field_value(FLD); dim(VAL) <- c(nl, 11L)
  USED <- trimws(FLD) != ""; dim(USED) <- c(nl, 11L)

  ## 1. embedded TAB ---------------------------------------------------------
  ## read.tucson tests only the FIRST data line for a tab. A tab further down
  ## leaves it in fixed-width mode, and read.fwf (which joins fields with \t)
  ## then splits that field in two. The line gains a column, later values shift
  ## a year, and the last value falls off the end.
  for (i in grep("\t", dat))
    add(did[i], dyr[i], "embedded tab",
        sprintf("line %d has a tab; read.tucson shifts this decade and drops its last value",
                lineno[i]))

  ## 2. two lines run together ------------------------------------------------
  ## A missing newline. The next line's ID lands in a measurement field, so that
  ## whole decade is lost silently.
  ##
  ## AGB Aug 2026: only fields 1-10 count. read.tucson keeps dat[3:12], i.e. the
  ## first ten values, so text in field 11 never reaches the data. Dozens of
  ## files park a "GAP" flag there and they are not broken.
  for (i in which(rowSums(USED[, 1:10, drop = FALSE] & is.na(VAL[, 1:10, drop = FALSE])) > 0L))
    add(did[i], dyr[i], "lines run together",
        sprintf("line %d is %d chars and holds non-numeric text in a measurement field",
                lineno[i], nchar(dat[i])))

  ## 2b. trailing text in field 11 --------------------------------------------
  ## Non-standard but harmless to read.tucson, which ignores field 11. It does
  ## force read.tucson down its slow character fallback, and it breaks readers
  ## that split on whitespace.
  f11 <- which(USED[, 11] & is.na(VAL[, 11]))
  if (length(f11))
    add(NA_character_, NA_integer_, "trailing text in field 11",
        sprintf("%d line(s) carry text in cols 73-78, e.g. '%s'",
                length(f11), trimws(FLD[f11[1], 11])))

  ## 3. value shifted out of its column ---------------------------------------
  ## A 6-character field that is numeric only after blanks are removed, e.g.
  ## "1  185" or "0  894". The digits crossed a column boundary. read.tucson,
  ## read.tucson2 and NOAA's converter each recover a different number here.
  STRAD <- USED & !is.na(VAL)
  STRAD[STRAD] <- grepl("[0-9-][[:space:]]+[0-9]", FLD[STRAD])
  for (i in which(rowSums(STRAD) > 0L))
    add(did[i], dyr[i], "value shifted out of column",
        sprintf("line %d field(s) %s = %s", lineno[i],
                paste(which(STRAD[i, ]), collapse = " "),
                paste0("'", FLD[i, STRAD[i, ]], "'", collapse = " ")))

  ## 4. interior year gap -----------------------------------------------------
  ## A decade line short of values, with the next line carrying on a decade
  ## later. read.tucson fills the hole with zeros, indistinguishable from the
  ## real zeros that mark locally absent rings.
  ## 5. stop marker inside a series -------------------------------------------
  ## 999 / -9999 on a line that is not the series' last. Two series are sharing
  ## one ID. read.tucson runs them together and keeps the marker as data: in
  ## kyrg014 that yields a ring width of -99.99 mm.
  nval   <- rowSums(!is.na(VAL))
  hasend <- rowSums(VAL == 999 | VAL == -9999, na.rm = TRUE) > 0L
  for (s in unique(did)) {
    k <- which(did == s)
    covered <- unique(unlist(Map(function(y, m) if (m) y + seq_len(m) - 1L else integer(0),
                                 dyr[k], nval[k])))
    if (length(covered)) {
      gaps <- setdiff(seq.int(min(covered), max(covered)), covered)
      if (length(gaps))
        add(s, min(gaps), "interior year gap",
            sprintf("%d year(s) missing (%s); read.tucson fills these with 0",
                    length(gaps), paste(range(gaps), collapse = "-")))
    }
    ## AGB Aug 2026: the marker must be the LAST value on its line. A 999 in the
    ## middle of a line is an ordinary 9.99 mm ring, not a terminator; requiring
    ## the terminal position removed about 4% of this category as false hits.
    ## The evidence also says how the two blocks sit relative to each other,
    ## because the three cases are different problems: overlapping years are an
    ## outright conflict, a gap of 0-1 years is a stray marker in what is really
    ## one series, and a real gap means one ID carries two separate segments.
    term <- vapply(k, function(i) {
      v <- VAL[i, ]; nzi <- which(!is.na(v)); h <- which(v %in% c(999, -9999))
      length(h) > 0L && length(nzi) > 0L && all(h >= max(nzi)) }, logical(1))
    inner <- k[term]; inner <- inner[inner < max(k)]
    if (length(inner)) {
      b     <- inner[1]
      endA  <- dyr[b] + nval[b] - 2L   # nval counts the marker itself
      nxt   <- k[k > b][1]
      gap   <- dyr[nxt] - endA - 1L
      ## AGB Aug 2026: split into three named cases rather than one bucket. They
      ## are different things and only one of them is a defect.
      ##   overlapping years -- two measurements claim the same year under one
      ##     ID. A real conflict; no reader can resolve it.
      ##   marker then an immediate continuation -- the terminator is stray.
      ##   a real gap -- one ID carrying two separate segments. This is NOT an
      ##     error. Interior gaps are accepted practice in the archive; plenty
      ##     of dendrochronologists would rather see two series, but plenty
      ##     would not, and it is not NOAA's job to adjudicate style.
      chk <- if (is.na(gap)) "stop marker mid-series, structure unclear" else
             if (gap < 0)  "series ID reused with overlapping years" else
             if (gap <= 1) "stop marker mid-series, series continues immediately" else
                           "one ID carries two separate segments"
      det <- if (is.na(gap)) "could not determine how the blocks relate" else
             if (gap < 0)  sprintf("blocks overlap by %d year(s)", -gap) else
             if (gap <= 1) "the next block starts immediately, so the marker looks stray" else
                           sprintf("a %d-year gap separates the two blocks", gap)
      add(s, dyr[b], chk,
          sprintf("stop marker ends a line at %d but the ID continues; %s", dyr[b], det))
    }
  }

  ## 5b. one ID starting a second block ---------------------------------------
  ## AGB Aug 2026: the same series ID beginning a second run of decade lines,
  ## detected by the year going backwards. Two physically separate pieces of
  ## wood are sharing one identifier. read.tucson2() only renames such blocks
  ## when their decades OVERLAP, so bt006's CAMPS12B (1550-1700, then 1740 on)
  ## is silently merged into a single series with a 40-year hole, and
  ## read.tucson() merges it and fills the hole with zeros. Neither says
  ## anything. Distinct from check 5, which catches the case where the second
  ## block starts LATER and is spotted by its stop marker instead.
  brk <- c(TRUE, did[-1] != did[-length(did)] | dyr[-1] <= dyr[-length(dyr)])
  starts <- data.frame(id = did[brk], yr = dyr[brk], stringsAsFactors = FALSE)
  for (u in unique(starts$id[duplicated(starts$id)]))
    add(u, min(starts$yr[starts$id == u]), "one ID starts a second block",
        sprintf("blocks begin at %s; two series share this ID",
                paste(sort(starts$yr[starts$id == u]), collapse = " and ")))

  ## 6. precision flags -------------------------------------------------------
  ## AGB Aug 2026: there is no check here any more, and that is deliberate.
  ##   v1 flagged any file containing both 999 and -9999. Wrong 122 times out of
  ##   123: precision is a property of the series, not the file, and both
  ##   readers scale a mixed file correctly.
  ##   v2 flagged any SERIES containing both markers. Also wrong, all 53 of
  ##   them: a series terminating in -9999 is measured in 0.001 mm, so a value
  ##   of 999 inside it is an ordinary 0.999 mm ring, not a flag.
  ## The real defect is a series with two TERMINATING markers -- kyrg014's
  ## kok3a, which stops at -9999 in 1898 and again at 999 in 2005. Deciding
  ## that needs the block structure the reader has already worked out, so we
  ## take read.tucson2()'s verdict rather than re-deriving it badly here. See
  ## qa20_grade_rwl.R.

  ## 6b. file mixes precision flags across series ------------------------------
  ## Reported, but NOT as an error: precision is a property of the series and
  ## both readers scale a mixed file correctly. It is unusual enough inside one
  ## collection to be worth a person's glance, so it is graded C with wording
  ## that says it may well be intentional.
  termflag <- unlist(lapply(unique(did), function(s) {
    k <- which(did == s)
    v <- VAL[max(k), ]; nzi <- which(!is.na(v))
    if (!length(nzi)) return(NULL)
    last <- v[max(nzi)]
    if (last %in% c(999, -9999)) last else NULL
  }))
  if (length(unique(termflag)) > 1)
    add(NA_character_, NA_integer_, "file mixes precision flags across series",
        sprintf("%d series end in 999 (0.01 mm) and %d in -9999 (0.001 mm).",
                sum(termflag == 999), sum(termflag == -9999)))

  ## 7. ID shape --------------------------------------------------------------
  ## Internal blanks break any reader that splits on whitespace: md007/md008
  ## IDs like "2  21" all collapse to "2", so every series looks duplicated. An
  ## ID filling all 8 columns leaves no gap before the year, which breaks
  ## whitespace splitting the other way.
  spacey <- unique(did[grepl("[[:space:]]", did)])
  if (length(spacey))
    add(NA_character_, NA_integer_, "blank inside series ID",
        sprintf("%d ID(s) contain blanks, e.g. '%s'", length(spacey), spacey[1]))
  full8 <- unique(did[!is_long[is_data] & substr(dat, 8, 8) != " "])
  if (length(full8))
    add(NA_character_, NA_integer_, "ID fills all 8 columns",
        sprintf("%d ID(s) run up against the year field, e.g. '%s'",
                length(full8), full8[1]))

  if (!length(hits)) return(NULL)
  do.call(rbind, hits)
}

## ---- sweep ----------------------------------------------------------------
if (sys.nframe() == 0L) {

  root <- "data_files/treering/measurements"
  rwl_files <- list.files(root, pattern = "\\.rwl$", recursive = TRUE, full.names = TRUE)
  cat("checking", length(rwl_files), "files\n")

  res <- vector("list", length(rwl_files))
  for (i in seq_along(rwl_files)) {
    res[[i]] <- rwl_format_check(rwl_files[i])
    if (i %% 500 == 0) cat("  ", i, "\n")
  }
  out <- do.call(rbind, res)
  out$file <- sub(paste0("^", root, "/"), "", out$file)
  out$severity <- unname(rwl_severity[out$check])
  out <- out[, c("file", "series", "year", "severity", "check", "detail")]
  out <- out[order(out$severity, out$file, out$check, out$series), ]

  write.csv(out, "QA_Stuff/rwl-format-problems.csv", row.names = FALSE)
  cat("\nfiles checked :", length(rwl_files),
      "\nfiles flagged :", length(unique(out$file)),
      "\nproblems      :", nrow(out), "\n\n")
  for (sv in c("cannot read", "corrupts data", "accepted practice", "informational")) {
    k <- out[out$severity == sv, ]
    if (!nrow(k)) next
    cat("\n--", sv, ":", nrow(k), "rows in", length(unique(k$file)), "files --\n")
    print(data.frame(rows = as.integer(table(k$check)),
                     files = tapply(k$file, k$check, function(x) length(unique(x))),
                     row.names = names(table(k$check))))
  }
}
