library(data.table)
library(dplR)

read.tucson2 <- function(file, fix.duplicates = FALSE, fix.dup.char = 'X', verbose = TRUE) {

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

  # First, read the whole file into a data.table, one row per row.
  # This data.table has a single column with name V1 by default.
  raw <- fread(file, sep = '\n', header = FALSE)[V1 != '']

  # If there is a header, the second line will have more than 72 characters
  if (nchar(raw[2, V1]) > 72) raw <- raw[-(1:3)]

  # Remove duplicated rows due to copy-paste
  dups <- duplicated(raw)
  if (any(dups) > 0) {
    warning(paste('Identical rows detected and removed in', file),
            paste0(capture.output(raw[dups][order(V1)]), collapse = '\n'))
    raw <- raw[!dups]
  }

  # Parsing --------------------------------------

  raw[, firstSpace := regexpr(' ', V1)]   # Determine the location of the first space character in each line
  cols <- paste0('Y', 0:9)                # Year 0 to year 9 for each row

  raw[, c('core', 'startYear', cols) := {
    # Get the numbers after the first space
    tailString <- substr(V1, firstSpace, nchar(V1))
    tailNums <- strsplit(tailString, ' ')[[1]] |> as.integer()
    tailNums <- tailNums[!is.na(tailNums)]
    N <- length(tailNums)

    # If the first space is after 8, core IDs may be bunched up with the year e.g., ABCD101A1750 100 200 300
    # Scenarios:
    #   If there are 10 numbers and no -9999 -> year bunching
    #   If there are 10 numbers or less and -9999, needs to check the previous row
    #   However, since the operation is vectorized, we can't check the previous row immediately
    #   So, assume there is year bunching (more common), and check back with the second pass (TODO)
    if ((firstSpace <= 9) || (N == 11)) {
      # coreID is before firstSpace, startYear is first 4 chars after space, firstMeasure is 5 chars after that
      core <- substr(V1, 1, firstSpace - 1)
      startYear <- tailNums[1]
      if ((tailNums[N] == 999) && (N < 11)) tailNums[N] <- -999
      measures <- split(tailNums[2:11], cols)
    } else {
      # When year bunching occurs, if there is the negative sign, year < -999
      if (regexpr('-', V1)[[1]] > 0) {
        core <- substr(V1, 1, firstSpace - 6)
        startYear <- as.integer(substr(V1, firstSpace - 5, firstSpace - 1))
      } else {
        core <- substr(V1, 1, firstSpace - 5)
        startYear <- as.integer(substr(V1, firstSpace - 4, firstSpace - 1))
      }
      if (tailNums[N] == 999) tailNums[N] <- -999 # Convert 999 flag to -999
      tailNums <- c(tailNums, rep(NA, 10 - N)) # pad NA to avoid split warning when length(cols) > length(split)
      measures <- split(tailNums, cols)
    }
    c(list(core, startYear), measures)
  }, by = seq_len(nrow(raw))
  ]
  raw[, c('V1', 'firstSpace') := NULL]

  # Convert to long format
  parsed <- melt(
    raw,
    id.vars = c('core', 'startYear'),
    variable.name = 'yearOrder',
    variable.factor = FALSE,
    value.name = 'rw')[order(core, startYear)][!is.na(rw)]

  # Now apply precision
  # Split cores may have more than one precision flag -> only keep one. If the flags are different, throw error
  precisionDT <- parsed[rw == -9999 | rw == -999]
  precisionCount <- precisionDT[, .N, by = core]

  if (any(precisionCount$N > 1)) {
    twoFlags <- precisionDT[core %in% precisionCount[N > 1, core]]
    diffFlag <- twoFlags[, .(diff = diff(rw)), by = core]
    if (any(diffFlag$diff > 0))
      stop(paste0('In ', file, ', different precision flags on the same core', diffFlag[diff > 0, core]))
  }

  precisionDT <- precisionDT[, tail(.SD, 1), by = core]
  precisionDT[, precision := fifelse(rw == -9999, 0.001, 0.01)]

  parsed <- merge(parsed, precisionDT[, .(core, precision)], by = 'core', all.x = TRUE)
  parsed[, rw := rw * precision]

  # Now we can convert those negative flags to NA
  # This also remove the hanging -9999
  parsed[rw < 0, rw := NA]
  parsed <- parsed[!is.na(rw)]

  # Finally we calculate the year from the startYear and the yearOrder
  parsed[, year := startYear + as.integer(substr(yearOrder, 2, 2))]
  # Clean up
  parsed[, c('startYear', 'yearOrder', 'precision') := NULL]

  # Fix duplicated core and year but with different measurements
  dups <- duplicated(parsed, by = c('core', 'year'))

  if (any(dups)) {
    warning(paste0('Duplicated core names in ', file,
                   '. Core IDs for the second measurements are affixed with',
                   fix.dup.char, '\n'),
            merge(parsed[!dups], parsed[dups],
                  by = c('core', 'year'),
                  suffixes = c('.1', '.2')) |>
              capture.output() |>
              paste0(collapse = '\n'))
    parsed[dups, core := paste0(core, fix.dup.char)]
  }

  # Now cast to wide to fill middle NA
  out <- dcast(parsed, year ~ core, value.var = 'rw')
  out[, (2:ncol(out)) := lapply(.SD, fill_middle_NAs), .SDcols = 2:ncol(out)]
  out <- as.data.frame(out)
  rownames(out) <- out$year
  out$year <- NULL
  if (verbose) print(out)
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
