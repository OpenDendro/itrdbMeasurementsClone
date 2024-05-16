library(data.table)
library(dplR)


read.tucson2 <- function(file, header = NULL, 
                         comment.char = '#',
                         fix.duplicates = TRUE, fix.dup.char = 'X', 
                         verbose = TRUE) {

  # Fix those cores with a huge chunk of missing rings in the middle
  # e.g. BT006 core CAMPS12B
  # Each core was entered as two separate segments in BT006.rwl
  # dplR fills zero
  # So I'm doing the same here to be consistent
  fill_middle_NAs <- function(x) {
    idx1 <- which(!is.na(x))[1]       # First non-NA
    idx2 <- tail(which(!is.na(x)), 1) # Last non-NA
    naIdx <- which(is.na(x))          # All NAs
    # Only fill NAs beween idx1 and idx2
    naToFill <- naIdx[naIdx %between% c(idx1, idx2)]
    x[naToFill] <- 0
    x
  }

  count_letters <- function(char.vector) sapply(gregexpr("[[:alpha:]]", substr(char.vector, 9, 72)), length)
  
  # First, read the whole file into a data.table, one row per row.
  # This data.table has a single column with name V1 by default.
  # strip.white = FALSE because sometimes core IDs have spaces in front.
  raw <- fread(file, header = FALSE, sep = '\n', 
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
    reRead <- rbindlist(lapply(rIdx, \(k) {
      raw2 <- gsub('\r', '\n', raw$V1[k])
      fread(text = raw2, sep = '\n', header = FALSE)
    }))
    raw <- rbind(raw[-rIdx], reRead)  
  }
  
  raw <- raw[regexpr(comment.char, V1) < 0] # Remove lines with comments    
  raw <- raw[substr(V1, 1, 1) != '\032']    # Strange EOF 
  
  # Find lines that are too long. Sometimes a file has notes at the end of the line
  raw[nchar(V1) > 72, V1 := substr(V1, 1, 72)] 

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

  # Check for header
  # Remove duplicated rows due to copy-paste
  dups <- duplicated(raw)
  if (any(dups) > 0) {
    warning(paste('Identical rows detected and removed in', file, '\n'),
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
  raw <- raw[!is.na(startYear)][order(core, startYear)]
  
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
    
  # Uncomment to debug problematic lines
  V1 <- raw$V1[546]

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
      message('In ', file, ', line ', V1, ' has no number, skipped.')
      tailNums <- rep(NA, 10)
    } else {
      # Check for non-numeric in measurements  
      hasNA <- is.na(tailNums)
      if (any(hasNA)) {
        message('In ', file, ' core ', core, ' decade starting ', startYear, ', ', sum(hasNA), ' measurement(s) have non-numeric characters, converted to zeros.')
      }
      
      # Check for very large numbers
      # Numbers > 999999 will be bunched up. In this case, read by fixed width
      if (length(which(tailNums > 999999)) > 0) {
        pos <- seq(from = 13, by = 6, length.out = 10)
        tailStrings <- sapply(pos, \(x) substr(V1, x, x + 5))
        tailNums <- as.numeric(tailStrings)
        N <- length(tailNums)
      }
      
      tailNums[tailNums < 0 & tailNums != -9999] <- NA # some files use negative numbers for missing rings
      tailNums[tailNums == flag] <- NA      
      # Convert to measurements  
      if (N < 10) tailNums <- c(tailNums, rep(NA, 10 - N)) # pad NA to have length 10
    }
    split(tailNums, cols)
  }, by = seq_len(nrow(raw))]
  
  raw[, V1 := NULL]
  
  # Convert to long format
  parsed <- melt(
    raw,
    id.vars = c('core', 'startYear', 'flag'),
    variable.name = 'yearOrder',
    variable.factor = FALSE,
    value.name = 'rw')[order(core, startYear)][!is.na(rw)]
  
  # At this point, if there is still a -9999 value in rw
  # That means the flag is different from -9999 -> two flags
  twoFlags <- parsed[rw < 0]
  if (nrow(twoFlags) > 0) {
    stop('In ', file, ', core(s) ', paste(twoFlags$core, collapse = ' '), ' have different precision flags.')
  }
  
  parsed[, precision := fifelse(flag == 999, 0.01, 0.001)]
  parsed[, rw := rw * precision]
  
  # Finally we calculate the year from the startYear and the yearOrder
  parsed[, year := startYear + as.integer(substr(yearOrder, 2, 2))]
  
  parsed[, c('startYear', 'yearOrder', 'flag') := NULL]

  # Fix duplicated core and year but with different measurements
  dups <- duplicated(parsed, by = c('core', 'year'))

  if (any(dups)) {
    warning(paste0('Duplicated core names in ', file,
                   '. Core IDs for the second measurements are affixed with ',
                   fix.dup.char, '\n'),
            merge(parsed[!dups, .(core, year, rw)], parsed[dups, .(core, year, rw)],
                  by = c('core', 'year'),
                  suffixes = c('.1', '.2')) |>
              capture.output() |>
              paste0(collapse = '\n'))
    parsed[dups, core := paste0(core, fix.dup.char)]
  }

  # Now cast to wide to fill middle NA
  out <- dcast(parsed[, .(year, core, rw)], year ~ core, value.var = 'rw')
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
  
  out <- as.data.frame(apply(out, 2, fill_middle_NAs))
  
  if (verbose) {
    summary <- parsed[, .(start = year[1], end = year[.N], precision = precision[1]), by = core]
    cat('There are ', nrow(summary), ' series.\n')
    print(summary)
  }
  # AGB making the output class rwl as well as df for dplR compatibility.
  class(out) <- c("rwl","data.frame")
  out
}

rwl_to_dt <- function(rwl) {
  rwl <- as.data.table(rwl, keep.rownames = 'year')
  rwl[, year := as.integer(year)]
  rwl <- melt(rwl, id.vars = 'year', variable.name = 'core', variable.factor = FALSE, value.name = 'rw')[!is.na(rw)]
  setcolorder(rwl, c('core', 'year', 'rw'))
  rwl[]
}

crn_to_dt <- function(crn) {
  crn <- as.data.table(crn, keep.rownames = 'year')
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
# r11 <- read.tucson2(file, fix.duplicates = TRUE, verbose = TRUE)
#
# microbenchmark::microbenchmark(
#   dplR::read.tucson(file, long = TRUE),
#   read.tucson2(file, fix.duplicates = TRUE),
#   times = 1
# )
#
# file <- 'data/ca535.rwl'  # Change file name to test different cases
# r00 <- dplR::read.tucson(file)
# r11 <- read.tucson2(file, fix.duplicates = TRUE, verbose = TRUE)
#
# rwl_to_dt(r11)
#
# microbenchmark::microbenchmark(
#   dplR::read.tucson(file),
#   read.tucson2(file, fix.duplicates = TRUE),
#   times = 1
# )
