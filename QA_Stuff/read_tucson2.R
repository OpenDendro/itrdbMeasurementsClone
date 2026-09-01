## AGB Aug 2026: this file used to open with library(data.table) and
## library(dplR). Both are gone. A function that attaches packages as a side
## effect of being sourced cannot go into dplR, and it made the QA runs depend on
## load order. Every data.table call is now written data.table:: instead, so the
## reader needs the package installed but not attached. dplR is only needed for
## fill.internal.NA(), also called with a :: prefix.
##
## That is enough while this is a sourced script, because data.table's cedta()
## check treats the global environment as data.table aware, so the := and .SD
## evaluation inside [ works normally.
##
## It will NOT be enough inside dplR. Once this code lives in a package
## namespace, :: prefixes alone make [.data.table quietly fall back to
## [.data.frame, and every := in here breaks. dplR will need either an
## importFrom(data.table, ...) in NAMESPACE or .datatable.aware <- TRUE in R/.
## See vignette("datatable-importing", package = "data.table").

## AGB Aug 2026: signature moved toward dplR::read.tucson() compatibility.
##  - first argument renamed file -> fname, to match read.tucson()
##  - edge.zeros added; read.tucson() has it and this reader had the TRUE
##    branch hardcoded, so callers could not ask for the other behaviour
##  - fill.internal.NA added, replacing the unconditional gap filling that used
##    to happen in fill_middle_NAs(), which is gone; see the note just below
##  - columns now come back in file order rather than sorted alphabetically
##  - header dropped. It was accepted and then ignored, and there is nothing
##    sensible for it to do here. In read.tucson() it only sets
##    skip.lines <- if (is.head) 3 else 0, so it cannot express the 4- and
##    6-line headers this archive actually contains (cana7rw, fl014), and the
##    header content is never returned to the caller either way. Even in
##    read.tucson() the header rows are really removed by its "requires non-NA
##    year" filter rather than by the skip. This reader classifies every line by
##    content instead, which handles all of cana7rw, dza004, fl014, fran10 and
##    ct001 identically to read.tucson(). If a manual override is ever needed the
##    right shape is skip = n, a line count -- not a logical. Do not add it until
##    a real file needs it.
##  - fix.duplicates dropped, for a different reason than header. It was also
##    accepted and ignored, the rename below always ran, but here there is no
##    sensible FALSE to implement. When one core has two measurements for the
##    same year and we do not rename, dcast() finds duplicate row/column pairs,
##    falls back to fun.aggregate = length, and returns COUNTS in place of ring
##    widths -- a column of 2s that still looks like data. So the fix is not
##    optional and should not be presented as if it were.
##    fix.dup.char stays, but the renaming it feeds has been rewritten to work on
##    blocks of decade lines rather than on rows, and to report what it did. See
##    the long note further down. read.tucson() has no equivalent of either
##    argument, and no duplicate handling at all.
##  - long deliberately NOT added, decided Aug 2026. read.tucson() needs it
##    because years before -999 take five columns and therefore steal column 8
##    from the series ID: 8-char IDs and BC dates cannot coexist in a Tucson
##    file. read.tucson() handles that with a per-file switch, c(7, 5, ...)
##    instead of c(8, 4, ...), which is the wrong shape for the problem. The
##    layout varies per LINE, so a file mixing 8-char IDs with BC dates is
##    corrupted whichever way the switch is set. Worse, the default long = FALSE
##    does not warn: it simply returns nothing usable.
##    This reader decides per line instead, in the "Split head" block below, by
##    keying on the minus sign in column 8. That sign is the only thing that
##    disambiguates the two layouts -- a digit in column 8 could equally be the
##    end of an 8-char ID or the start of a 5-char year, so width alone cannot
##    settle it. Checked against all 10 study files in the archive with years
##    < -999 (brit036/037/039, ausl024, chin067, chin069, swed334,
##    turk044/045/046): 900,000+ cells, identical to read.tucson(long = TRUE),
##    while read.tucson(long = FALSE) fails on every one. Do not add a long
##    argument to re-expose the switch. If drop-in compatibility ever forces the
##    name into the signature, accept it and ignore it -- do not let it override
##    the per-line detection.
##  - strict added Aug 2026. FALSE, the default, keeps the reader going and
##    reports every recoverable problem as a warning. TRUE turns the same set
##    into an error, so a pipeline can refuse a file rather than carry a warned
##    guess forward. Everything routed through report() below is covered,
##    duplicate series IDs included.
read.tucson2 <- function(fname,
                         comment.char = '#',
                         fix.dup.char = 'X',
                         edge.zeros = TRUE,
                         fill.internal.NA = NULL,
                         strict = FALSE,
                         verbose = TRUE) {

  ## AGB Aug 2026: every recoverable problem now goes through report(), so the
  ## strict switch lives in one place instead of being repeated at each call
  ## site. Conditions that cannot be localised to a line, such as a file mixing
  ## precision flags, still stop() unconditionally: there is no sensible reading
  ## to hand back, strict or not.
  report <- function(...) {
    msg <- paste0(...)
    if (isTRUE(strict)) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
  }

  ## AGB Aug 2026: fill_middle_NAs() used to live here. It filled every interior
  ## gap in a series with zero, on the reasoning that dplR does the same. dplR
  ## does -- inside the C readloop, undocumented -- but that is the behaviour we
  ## are trying to get away from, not match. bt006 core CAMPS12B is stored as two
  ## segments with a -9999 stop marker between them, and the fill invented 30
  ## years of zero rings, i.e. 30 years of a tree growing nothing. Interior gaps
  ## now stay NA and the caller decides, via the fill.internal.NA argument, which
  ## hands off to dplR::fill.internal.NA().

  ## AGB Aug 2026: used only when edge.zeros = FALSE. read.tucson() gets this
  ## effect as a side effect of NA-ing every zero and then refilling the interior
  ## ones in its readloop. We no longer refill anything, so the trimming has to
  ## be done directly, on the leading and trailing runs of each series.
  trim_edge_zeros <- function(x) {
    idx <- which(!is.na(x))
    if (length(idx) == 0) return(x)
    v <- x[idx]
    k <- 1L
    while (k <= length(v) && v[k] == 0) k <- k + 1L
    if (k > 1L) x[idx[1:(k - 1L)]] <- NA
    m <- length(v)
    while (m >= 1L && v[m] == 0) m <- m - 1L
    if (m < length(v)) x[idx[(m + 1L):length(v)]] <- NA
    x
  }

  count_letters <- function(char.vector) sapply(gregexpr("[[:alpha:]]", substr(char.vector, 9, 72)), length)
  
  # First, read the whole file into a data.table, one row per row.
  # This data.table has a single column with name V1 by default.
  # strip.white = FALSE because sometimes core IDs have spaces in front.
  raw <- data.table::fread(fname, header = FALSE, sep = '\n', 
               blank.lines.skip = TRUE, strip.white = FALSE)
  
  # Clean up ----------------------------------------------------------------------
  # Sometimes the file has mixed EOL chars, esp. between the headers and the body, and fread can fail. 
  # This will result in one or four rows only in the read result.
  # Special case: pak042 has a wrong EOL in PSL0, likely edited in a Mac to a file from Windows
  # This caused two lines to merge
  # Replace \r with \n and reread the text (not the file)

  # if (nrow(raw) == 1) {
  #   if (regexpr('\r', raw) > 0) {
  #     raw <- gsub('\r', '\n', raw)
  #     raw <- fread(text = raw, sep = '\n', header = FALSE)
  #   }
  # } else if (nrow(raw) == 4) {
  #   if (regexpr('\r', raw$V1[4]) > 0) {
  #     raw2 <- gsub('\r', '\n', raw$V1[4])
  #     raw2 <- fread(text = raw2, sep = '\n', header = FALSE)
  #     raw  <- rbind(raw[1:3], raw2)
  #   }
  # }
  
  rIdx <- which(regexpr('\r', raw$V1) > 0)
  if (length(rIdx) > 0) {
    reRead <- data.table::rbindlist(lapply(rIdx, \(k) {
      raw2 <- gsub('\r', '\n', raw$V1[k])
      data.table::fread(text = raw2, sep = '\n', header = FALSE)
    }))
    raw <- rbind(raw[-rIdx], reRead)  
  }
  
  raw <- raw[regexpr(comment.char, V1) < 0] # Remove lines with comments    
  raw <- raw[substr(V1, 1, 1) != '\032']    # Strange EOF 
  
  ## AGB Aug 2026: the overflow past column 72 is reported further down, once
  ## header lines have been dropped. Doing it here fired on every file whose
  ## header carries an end year past column 72, e.g. brit046.
  raw[, ovf := substr(V1, 73, nchar(V1))]
  raw[, V1 := substr(V1, 1, 72)]

  # Trim trailing white 
  # Leave leading white spaces because we can safely handles a lot of cases
  # with 8-char IDs that has spaces in front.
  raw[, V1 := trimws(V1, 'right')]          
  raw <- raw[nchar(V1) > 12]                # Remove rows that are too short
  
  # Remove headers
  # In principal a data line should not have any non-numeric character after the 13th position
  # so we can use grepl("[[:alpha:]]", substr(line, 9, 72)) to detect header lines
  # however, there are some files with several letters e.g. az615
  # Some files use NaN to mark missing rings
  # So we say a data line should not have "too many" letters
  # How many is too many? Let's keep it at 3 (as it is now with the problematic files).
  raw <- raw[count_letters(V1) <= 3]                     

  ## AGB Aug 2026: report what truncating at column 72 threw away, but only when
  ## it cost us something. Most overflow is a trailing note or a per-line count
  ## column and is genuinely disposable; warning about it buried the real cases.
  ## Two patterns are not disposable:
  ##   1. a measurement cut in half by the column boundary -- a digit in column
  ##      72 and another in column 73. mar047's TIZ19A 1900 line is one
  ##      character over, so the truncation turned 116 into 11, i.e. 1.16 mm
  ##      reported as 0.11 mm, with nothing said.
  ##   2. a whole second record appended because a newline is missing. az621's
  ##      FR-001 and FR-002 share a line, and both this reader and read.tucson
  ##      drop FR-002's first decade without a word. NOAA's own template file
  ##      for az621 has the same ten measurements missing, so their converter
  ##      hit it too.
  cutNumber <- grepl('[0-9]$', raw$V1) & grepl('^[0-9]', raw$ovf)
  secondRec <- grepl('^[[:space:]]*-?[0-9]+[[:space:]]*$', substr(raw$ovf, 9, 12))
  if (any(cutNumber))
    report('In ', fname, ', ', sum(cutNumber), ' line(s) run past column 72 with a ',
           'measurement split across the boundary, so the truncation at column 72 ',
           'drops part of a number. First one: ', trimws(raw$V1[cutNumber][1]))
  if (any(secondRec))
    report('In ', fname, ', ', sum(secondRec), ' line(s) appear to hold a second ',
           'record appended after column 72, which means a missing line break. ',
           'The appended record is discarded. First one: ',
           trimws(raw$V1[secondRec][1]), ' <<overflow>> ', trimws(raw$ovf[secondRec][1]))
  ## drop before the duplicate check below, which compares whole rows
  raw[, ovf := NULL]

  ## AGB Aug 2026: dropped a stray "# Check for header" comment that sat here.
  ## Header removal happens above, by letter count; nothing is checked here.
  # Remove duplicated rows due to copy-paste
  dups <- duplicated(raw)
  if (any(dups) > 0) {
    report(paste('Identical rows detected and removed in', fname, '\n'),
            paste0(capture.output(raw[dups][order(V1)]), collapse = '\n'))
    raw <- raw[!dups]
  }
  
  # Parsing 
  # A row has two parts: head and tail
  # Head is ID + year (which can be bunched). 
  #    This should be 12 chars ending with a digit
  #    Cana209 is an exception
  # Tail should be a bunch of numbers
  #    Max 3 characters allowed
  #    Those with characters will be converted to NA
  
  # Split head ----------------------------------------------------------
  
  startDigits <- c(as.character(1:9), '-')
  raw[, c('core', 'startYear') := {
    
    headString <- substr(V1, 1, 12)
    if (substr(headString, 12, 12) != ' ') {
      if (substr(headString, 8, 8) == '-') {
        startYear <- substr(headString, 8, 12)
        core      <- substr(headString, 1, 7)
      } else {
        startYear <- substr(headString, 9, 12)
        core      <- substr(headString, 1, 8)
      }
    } else { 
      # cana209, nj001, nj002: year shifted left, not bunch
      # japa018: year shifted left, bunched
      if (substr(headString, 7, 7) == '-') {
        startYear <- substr(headString, 7, 11)
        core      <- substr(headString, 1, 6)
      } else {
        if (substr(headString, 8, 8) %in% startDigits) {
          startYear <- substr(headString, 8, 11)
          core      <- substr(headString, 1, 7)
        } else {
          startYear <- substr(headString, 9, 11)
          core      <- substr(headString, 1, 8)
        }
      }
    }
    list(core = core, startYear = startYear)
  }, by = seq_len(nrow(raw))]
  
  raw[, ':='(startYear = as.integer(startYear),
             core = trimws(core))]  
  raw <- raw[!is.na(startYear)]

  ## AGB Aug 2026: resolve repeated series IDs here, before anything else touches
  ## the core names. This used to happen much further down, row by row on the long
  ## table -- duplicated(parsed, by = c('core','year')), then one suffix pasted
  ## onto the marked rows. That handled exactly one shape of problem, a clean
  ## two-way duplicate covering identical years, and failed silently on the rest:
  ##   * three copies of a core: copies 2 and 3 both became <core>X, which
  ##     recreated the duplicate. dcast() then fell back to fun.aggregate =
  ##     length, so every measurement in the file became a row count.
  ##   * a file that already contained <core>X: the rename collided with the real
  ##     series, same result.
  ##   * a repeat running longer than the original: only the overlapping rows were
  ##     renamed, so the first series ended up with the head of one copy welded to
  ##     the tail of the other. No warning at all in that case.
  ## The unit of duplication is a block of decade lines, not a row, so that is
  ## what we work on now. A new block starts wherever the core ID changes or the
  ## decade stops advancing. A block sharing no decade with an existing series of
  ## the same name is merged into it -- that is how a core split across two parts
  ## of a file stays one series (bt006 CAMPS12B). A block that does share a decade
  ## is a real duplicate and gets a new name, suffixed until it is genuinely
  ## unused, so three copies and pre-existing <core>X names both resolve.
  ## Note the rename is order dependent: if a real series is called <core>X and an
  ## earlier <core> is duplicated, the later real one is what gets suffixed. No
  ## data is lost either way, and both are reported.
  if (nrow(raw) > 0L) {
    nr  <- nrow(raw)
    brk <- if (nr == 1L) TRUE else
      c(TRUE, raw$core[-1L] != raw$core[-nr] | raw$startYear[-1L] <= raw$startYear[-nr])
    raw[, segId := cumsum(brk)]

    blocks  <- raw[, .(core = core[1L], decades = list(startYear)), by = segId]
    blocks  <- blocks[order(segId)]
    claimed <- list()                     # final name -> decades it already owns
    newName <- character(nrow(blocks))
    renamed <- character(0)

    for (i in seq_len(nrow(blocks))) {
      id   <- blocks$core[i]
      dec  <- blocks$decades[[i]]
      cand <- id
      while (!is.null(claimed[[cand]]) && any(dec %in% claimed[[cand]])) {
        cand <- paste0(cand, fix.dup.char)
      }
      newName[i]      <- cand
      claimed[[cand]] <- c(claimed[[cand]], dec)
      if (!identical(cand, id))
        renamed <- c(renamed, sprintf('  %s  decades %d-%d  ->  %s',
                                      id, min(dec), max(dec) + 9L, cand))
    }

    raw[, core := newName[segId]]
    raw[, segId := NULL]

    ## Tell the user. Duplicated IDs are a perennial problem in this archive and
    ## silence about them is worse than the duplication.
    if (length(renamed) > 0L)
      report(paste0('In ', fname, ', ', length(renamed),
                     ' repeated series ID(s) had overlapping years and were ',
                     'renamed so that no measurements are lost:\n'),
              paste(renamed, collapse = '\n'))

    if (verbose) {
      for (m in names(which(table(newName) > 1L))) {
        d <- sort(unlist(claimed[[m]]))
        cat('Series ', m, ' was entered in ', sum(newName == m),
            ' separate blocks with no shared decade, read as one series (decades ',
            min(d), '-', max(d) + 9L, ').\n', sep = '')
      }
    }
  }

  ## AGB Aug 2026: remember the order in which series first appear in the file,
  ## before the sort below. dcast() returns columns in alphabetical order, but
  ## dplR::read.tucson() returns them in file order. Sorting made every
  ## whole-object comparison between the two readers fail on column order alone,
  ## which masked the value differences we actually wanted to see. It would also
  ## silently reorder the data of anyone who swapped one reader for the other.
  coreOrder <- unique(raw$core)
  raw <- raw[order(core, startYear)]
  
  # Looking for the precision flag at the last row of each core ----
  raw[, flag := {
    V1 <- .SD[.N, V1]
    tailStrings <- strsplit(substr(V1, 13, nchar(V1)), ' ')[[1]]
    tailStrings <- tailStrings[nzchar(tailStrings)]
    
    # Handling dash, -9999 can be bunched (mexi077)
    M <- length(tailStrings)
    dashLoc <- gregexpr("-", tailStrings)
    hasDash <- which(dashLoc > 1)
    if (length(hasDash) == 1) {
      tmp <- tailStrings[hasDash]
      tailStrings[hasDash] <- substr(tmp, 1, dashLoc[[hasDash]] - 1)
      if (hasDash == M) {
        tailStrings <- c(tailStrings[1:hasDash],
                         substr(tmp, dashLoc[[hasDash]], nchar(tmp)))
      } else {
        tailStrings <- c(tailStrings[1:hasDash],
                         substr(tmp, dashLoc[[hasDash]], nchar(tmp)),
                         tailStrings[(hasDash + 1) : M])
      }
    } 
    tailNums <- suppressWarnings(as.numeric(tailStrings))
    if (tailNums[length(tailNums)] == -9999) -9999 else 999
  }, by = core]
  raw <- raw[!is.na(flag)]
  # Split tail ----
    
  ## AGB Aug 2026: dropped a live "V1 <- raw$V1[546]" that sat here uncommented
  ## under a "# Uncomment to debug problematic lines" note. It was inert only
  ## because the data.table j-expressions below rebind V1 to the column.

  cols <- paste0('Y', 0:9)                # Year 0 to year 9 for each row
  
  raw[, c(cols) := {
    
    tailStrings <- substr(V1, 13, nchar(V1))
    
    # Check if empty spaces are used for missing rings.
    # In this case we have 6 empty spaces in a row
    # Read fix-width 
    if (grepl('      ', tailStrings)) {
      pos <- seq(from = 13, by = 6, length.out = 10)
      tailStrings <- sapply(pos, \(x) substr(V1, x, x + 5))
    } else {
    # Otherwise, split the numbers by spaces
      tailStrings <- strsplit(tailStrings, ' ')[[1]]
      tailStrings <- tailStrings[nzchar(tailStrings)]
    }
    
    tailNums <- suppressWarnings(as.integer(tailStrings))
    
    # String to numbers ----
    
    # Handling dash ----
    # Sometimes measurements look like this 1234-50623, e.g. ak165
    # Need to detect "-" and split it.
    # This is rare, I don't expect more than once per row
    M <- length(tailStrings)
    dashLoc <- gregexpr("-", tailStrings)
    hasDash <- which(dashLoc > 1)
    if (length(hasDash) == 1) {
      tmp <- tailStrings[hasDash]
      tailStrings[hasDash] <- substr(tmp, 1, dashLoc[[hasDash]] - 1)
      if (hasDash == M) {
        tailStrings <- c(tailStrings[1:hasDash],
                         substr(tmp, dashLoc[[hasDash]], nchar(tmp)))
      } else {
        tailStrings <- c(tailStrings[1:hasDash],
                         substr(tmp, dashLoc[[hasDash]], nchar(tmp)),
                         tailStrings[(hasDash + 1) : M])
      }
    } 
    #   ----
    
    tailNums <- suppressWarnings(as.numeric(tailStrings))
    N <- length(tailNums)
    
    # Special cases -----------------------------------------------
    if (N == 0) {
      ## AGB Aug 2026: was message(); now goes through report() so strict can
      ## refuse the file. A decade line holding no measurement at all is not
      ## something to mention in passing.
      report('In ', fname, ', a line holds no measurement and was skipped: ', trimws(V1))
      tailNums <- rep(NA, 10)
    } else {
      # Check for non-numeric in measurements  
      hasNA <- is.na(tailNums)
      if (any(hasNA)) {
        ## AGB Aug 2026: this said "converted to zeros" and nothing in the
        ## function ever did that -- the values stay NA. Wrong text on a
        ## message is worse than no message, because it tells the reader zeros
        ## in their data are expected. Corrected, and routed through report().
        report('In ', fname, ', core ', core, ', decade starting ', startYear, ': ',
               sum(hasNA), ' measurement(s) are not numeric and are left as NA.')
      }
      
      # Check for very large numbers
      # Numbers > 999999 will be bunched up. In this case, read by fixed width
      if (length(which(tailNums > 999999)) > 0) {
        pos <- seq(from = 13, by = 6, length.out = 10)
        tailStrings <- sapply(pos, \(x) substr(V1, x, x + 5))
        tailNums <- as.numeric(tailStrings)
        N <- length(tailNums)
      }

      ## AGB Aug 2026: conformance check, added after finding that this reader,
      ## read.tucson() and NOAA's own converter each return DIFFERENT numbers
      ## for the same shifted lines, none of them complaining. Columns 13-72
      ## hold ten 6-character fields. That is the format, so read the span that
      ## way as well, and compare against whatever the parse above produced.
      ##
      ## On a conforming line the two readings are identical, so this is silent
      ## on good files. Where they differ the line does not conform, and nothing
      ## in the line says which reading was meant: arge041's "1  185" is either
      ## 1185 or 1 and 185, and the file cannot tell you. So we return NA for
      ## the whole line and report it, rather than pick a side. Deliberately no
      ## file-specific rules and no arbitration -- guessing better is still
      ## guessing, and a wrong ring width is worse than an absent one.
      ##
      ## The check sits here, after the dash and bunched-number recoveries, so
      ## those known deterministic idioms are settled before we compare.
      fixedRaw  <- substring(substr(V1, 13, 72),
                             seq(1, by = 6, length.out = 10),
                             seq(6, by = 6, length.out = 10))
      fixedRaw  <- fixedRaw[trimws(fixedRaw) != '']
      fixedNums <- suppressWarnings(
        as.numeric(gsub('[[:space:]]', '', fixedRaw)))   # scan() drops inner blanks; match it
      cmpNums   <- tailNums[seq_len(min(length(tailNums), 10))]
      sameLen   <- length(fixedNums) == length(cmpNums)
      if (!sameLen || !isTRUE(all.equal(fixedNums, cmpNums))) {
        report('In ', fname, ', core ', core, ', decade starting ', startYear,
               ': the ten fixed-width columns and a whitespace split of the same ',
               'line give different measurements, so the line does not conform ',
               'to the Tucson column layout and cannot be read unambiguously. ',
               'Returned as NA. Columns give (',
               paste(fixedNums, collapse = ' '), '); whitespace gives (',
               paste(cmpNums, collapse = ' '), '). Line: ', trimws(V1))
        tailNums <- rep(NA_real_, 10)
        N <- 10L
      }
      
      tailNums[tailNums < 0 & tailNums != -9999] <- NA # some files use negative numbers for missing rings
      ## AGB Aug 2026: the line above is read.tucson()'s edge.zeros = TRUE branch,
      ## which used to be all this reader did. The zeros themselves are trimmed
      ## later, per series, by trim_edge_zeros(); they cannot be trimmed here
      ## because at this point we are still inside one decade of one line.
      tailNums[tailNums == flag] <- NA      
      # Convert to measurements  
      if (N < 10) tailNums <- c(tailNums, rep(NA, 10 - N)) # pad NA to have length 10
    }
    split(tailNums, cols)
  }, by = seq_len(nrow(raw))]
  
  raw[, V1 := NULL]
  
  # Convert to long format
  parsed <- data.table::melt(
    raw,
    id.vars = c('core', 'startYear', 'flag'),
    variable.name = 'yearOrder',
    variable.factor = FALSE,
    value.name = 'rw')[order(core, startYear)][!is.na(rw)]
  
  # At this point, if there is still a -9999 value in rw
  # That means the flag is different from -9999 -> two flags
  twoFlags <- parsed[rw < 0]
  if (nrow(twoFlags) > 0) {
    stop('In ', fname, ', core(s) ', paste(twoFlags$core, collapse = ' '), ' have different precision flags.')
  }
  
  parsed[, precision := data.table::fifelse(flag == 999, 0.01, 0.001)]
  parsed[, rw := rw * precision]
  
  # Finally we calculate the year from the startYear and the yearOrder
  parsed[, year := startYear + as.integer(substr(yearOrder, 2, 2))]
  
  parsed[, c('startYear', 'yearOrder', 'flag') := NULL]

  ## AGB Aug 2026: the row-level duplicate fix that used to sit here has moved up
  ## to just after the head is parsed, and now works on blocks of decade lines.
  ## See the note there for what it was getting wrong. By this point (core, year)
  ## is unique by construction, so the check below is an assertion about our own
  ## logic rather than a check on the file. If it ever fires it is a bug in the
  ## block code above, not a problem with fname.
  ## AGB Aug 2026: anything still duplicated here is a different problem from a
  ## repeated series ID, and must not be handled the same way. It means two lines
  ## of one block claim the same year, which happens when a line's columns are
  ## bunched or shifted so it parses as more values than it holds. arge041 is the
  ## worked example: RH17A's first line reads "RH17A   19421  351 ..." and comes
  ## out as 1942 plus nine values, running into 1950, which the next line also
  ## starts. The file is malformed, not duplicated, so renaming would invent a
  ## one-year series out of a parsing artefact.
  ## We still read the file. We keep the first value, drop the second, and say
  ## exactly which ones so the user can go and look at the lines themselves --
  ## there is no way from here to tell which of the two is the real measurement.
  dupIdx <- duplicated(parsed, by = c('core', 'year'))
  if (any(dupIdx)) {
    clash <- merge(parsed[!dupIdx, .(core, year, kept = rw)],
                   parsed[ dupIdx, .(core, year, dropped = rw)],
                   by = c('core', 'year'))
    report(paste0('In ', fname, ', ', nrow(clash), ' year(s) were measured ',
                   'twice within a single series, by lines whose year ranges ',
                   'overlap. That normally means the columns on one of those ',
                   'lines are bunched or shifted, i.e. the file is malformed ',
                   'rather than merely duplicated. The first value was kept and ',
                   'the second discarded; check these lines by hand:\n'),
            paste(sprintf('  %s  %d  kept %s, discarded %s', clash$core,
                          clash$year, format(clash$kept), format(clash$dropped)),
                  collapse = '\n'))
    parsed <- parsed[!dupIdx]
  }

  # Now cast to wide to fill middle NA
  out <- data.table::dcast(parsed[, .(year, core, rw)], year ~ core, value.var = 'rw')
  out <- as.data.frame(out)
  rownames(out) <- out$year
  out$year <- NULL
  
  # In rare cases the longest core has a missing segment before the next longest core begins 
  # and this won't be filled by dcast
  # So fill manually here
  repeat {
    years <- as.integer(rownames(out))
    yearDiff <- diff(years)
    gapIdx <- which(yearDiff != 1)
    if (length(gapIdx) == 0) break
    M <- ncol(out)
    N <- nrow(out)
    filler <- matrix(NA, yearDiff[gapIdx[1]] - 1, M)
    colnames(filler) <- colnames(out)
    rownames(filler) <- (years[gapIdx[1]] + 1) : (years[gapIdx[1]+1]-1)
    out <- rbind(
      out[1:gapIdx[1], ],
      filler,
      out[(gapIdx[1]+1):N, ])  
  }
  
  ## AGB Aug 2026: this line used to be
  ##   out <- as.data.frame(apply(out, 2, fill_middle_NAs))
  ## i.e. every interior gap was filled with zero, always, with no way to opt out.
  ## The three steps below replace it: restore file order, honour edge.zeros, and
  ## fill interior gaps only if the caller asked for it.

  ## Columns back into file order. Anything renamed by the duplicate fix above
  ## will not be in coreOrder, so those go on the end rather than being dropped.
  out <- out[, c(intersect(coreOrder, names(out)),
                 setdiff(names(out), coreOrder)), drop = FALSE]

  ## edge.zeros = FALSE: leading and trailing runs of zeros are treated as no
  ## data rather than as absent rings, so the series is shortened.
  if (!isTRUE(edge.zeros)) out[] <- lapply(out, trim_edge_zeros)

  ## Interior gaps stay NA unless the caller names a fill. The value is passed
  ## straight through to dplR::fill.internal.NA(), so fill.internal.NA = 0
  ## reproduces what dplR::read.tucson() does today, and "Mean" / "Spline" /
  ## "Linear" are available too. NULL, the default, fills nothing.
  ##
  ## AGB Aug 2026: say so, loudly, either way. Filling interior gaps with zero
  ## is a long-standing convention -- dplR does it, and the DPL programs before
  ## it did too. It is normally applied where a stretch of a core cannot be
  ## measured: rot, a branch scar, a crumbled section. So it is legal and
  ## common, and this reader must not pretend otherwise.
  ##
  ## What it must not do is make the choice silently. A zero ring width means a
  ## locally absent ring, which is a real biological observation; a gap means
  ## nobody could measure. Writing the first where the second is true, without
  ## telling anyone, is how 250,000 measurements across the ITRDB came to say
  ## "this tree grew nothing" when the file only ever said "unknown". The
  ## default here leaves NA, and either way the user is told what happened and
  ## how to get the other behaviour.
  gapRuns <- vapply(out, function(v) {
    k <- which(!is.na(v))
    if (length(k) < 2L) return(0L)
    inner <- v[k[1]:k[length(k)]]
    r <- rle(is.na(inner))
    sum(r$values)
  }, integer(1))
  nGapSeries <- sum(gapRuns > 0L)
  nGapCells  <- sum(vapply(out, function(v) {
    k <- which(!is.na(v))
    if (length(k) < 2L) return(0L)
    sum(is.na(v[k[1]:k[length(k)]]))
  }, integer(1)))

  if (nGapSeries > 0L) {
    if (is.null(fill.internal.NA)) {
      report('In ', fname, ', ', nGapSeries, ' series contain interior gaps ',
             '(', nGapCells, ' year(s) in total) where the file records no ',
             'measurement. They are returned as NA. dplR::read.tucson() fills ',
             'these with zero, which is the long-standing DPL convention and is ',
             'not an error -- but a zero ring width means an absent ring, not a ',
             'missing one. Pass fill.internal.NA = 0 to reproduce the old ',
             'behaviour, or "Mean", "Spline" or "Linear" to interpolate.')
    } else {
      report('In ', fname, ', ', nGapCells, ' interior gap year(s) across ',
             nGapSeries, ' series were filled with "', fill.internal.NA,
             '" at your request. These values are not measurements.')
    }
    out <- if (is.null(fill.internal.NA)) out else
           dplR::fill.internal.NA(out, fill = fill.internal.NA)
  }


  if (verbose) {
    summary <- parsed[, .(start = year[1], end = year[.N], precision = precision[1]), by = core]
    cat('There are ', nrow(summary), ' series.\n')
    print(summary)
  }
  ## AGB Aug 2026: dplR::read.tucson() stores row names as character, this reader
  ## was storing them as numeric. The years matched, but all.equal() then reported
  ## an attribute difference on every file, which is noise in any comparison.
  rownames(out) <- as.character(rownames(out))

  # AGB making the output class rwl as well as df for dplR compatibility.
  class(out) <- c("rwl","data.frame")
  out
}

rwl_to_dt <- function(rwl) {
  rwl <- data.table::as.data.table(rwl, keep.rownames = 'year')
  rwl[, year := as.integer(year)]
  rwl <- data.table::melt(rwl, id.vars = 'year', variable.name = 'core', variable.factor = FALSE, value.name = 'rw')[!is.na(rw)]
  data.table::setcolorder(rwl, c('core', 'year', 'rw'))
  rwl[]
}

crn_to_dt <- function(crn) {
  crn <- data.table::as.data.table(crn, keep.rownames = 'year')
  crn[, year := as.integer(year)]
  crn[]
}

# # Duplicates due to copying and pasting twice: chin027
# # Duplicated ID but different measurements: chin038
# # Duplicated due to hanging -9999: nepa010
# # Records with years -1000 and before: chin067, chin069
# # Records with mixed precision: KYRG012
# # Has year 999 : CHIN005
# # Has a chunk of 0 in the middle: BT006 core CAMPS12B
#
# file <- 'data/chin069.rwl'  # Change file name to test different cases
# r00 <- dplR::read.tucson(file, long = TRUE)
# r11 <- read.tucson2(file, verbose = TRUE)
#
# microbenchmark::microbenchmark(
#   dplR::read.tucson(file, long = TRUE),
#   read.tucson2(file),
#   times = 1
# )
#
# file <- 'data/ca535.rwl'  # Change file name to test different cases
# r00 <- dplR::read.tucson(file)
# r11 <- read.tucson2(file, verbose = TRUE)
#
# rwl_to_dt(r11)
#
# microbenchmark::microbenchmark(
#   dplR::read.tucson(file),
#   read.tucson2(file),
#   times = 1
# )
