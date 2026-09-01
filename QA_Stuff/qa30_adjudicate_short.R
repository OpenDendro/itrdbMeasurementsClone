## Pass 4 of the ITRDB file sweep: adjudicate the short-series candidates
## AGB -- Aug 2026
##
## AGB Aug 2026: a series of ten years or fewer is unusual, but it is not by
## itself an error -- a young tree, a fire-scar sample or a fragment are all
## real. Sending NOAA 452 raw candidates would be the same mistake as the first
## structural sweep, where most of a big number turned out to be one artefact.
##
## So each candidate is put in context before it is reported:
##
##   1. How short is it RELATIVE to its own file? A five-year series among forty
##      two-hundred-year series is odd. The same series in a file where half the
##      collection is short is a collection style, not a mistake.
##   2. Does it crossdate? Correlate the candidate against a master built from
##      the rest of the file. This is weak evidence on ten points and is
##      reported as a number, never as a verdict -- with n and r side by side so
##      a reader can discount it themselves.
##
## Only files that actually hold a candidate get re-read, not the whole archive.
##
## Writes QA_Stuff/short-series-adjudicated.csv

suppressPackageStartupMessages({ library(dplR); library(data.table); library(future.apply) })

ROOT <- "data_files/treering/measurements"
load("RdataFiles/qa_read_all.Rdata")

cand <- qa_series[span <= 10]
cat("short-series candidates:", nrow(cand), "in", length(unique(cand$file)), "files\n")

## file-level context, from the summaries we already have
ctx <- qa_series[, .(n_series = .N,
                     med_span = as.numeric(stats::median(span)),
                     n_short  = sum(span <= 10)), by = file]
cand <- merge(cand, ctx, by = "file", all.x = TRUE)
cand[, frac_short := n_short / n_series]

## ---- crossdate each candidate against the rest of its own file ------------
adjudicate_file <- function(rel) {
  p <- file.path(ROOT, rel)
  x <- tryCatch(suppressWarnings(suppressMessages(read.tucson2(p, verbose = FALSE))),
                error = function(e) NULL)
  if (is.null(x)) return(NULL)
  tgt <- cand[file == rel, series]
  tgt <- tgt[tgt %in% colnames(x)]
  if (!length(tgt)) return(NULL)
  yrs <- as.integer(rownames(x))
  rbindlist(lapply(tgt, function(s) {
    others <- setdiff(colnames(x), s)
    if (length(others) < 3L) return(data.table(file = rel, series = s, n_overlap = NA_integer_, r = NA_real_))
    ## master = mean of the other series, each scaled by its own mean so that a
    ## fast-growing tree does not dominate. No detrending: over ten years the
    ## trend is not the thing we are testing.
    M <- as.matrix(x[, others, drop = FALSE])
    M <- sweep(M, 2, colMeans(M, na.rm = TRUE), "/")
    master <- rowMeans(M, na.rm = TRUE)
    v <- x[[s]]
    ok <- !is.na(v) & !is.na(master) & is.finite(master)
    if (sum(ok) < 5L) return(data.table(file = rel, series = s, n_overlap = sum(ok), r = NA_real_))
    ## first differences: removes the common level and tests year-to-year
    ## agreement, which is what crossdating actually means
    dv <- diff(v[ok]); dm <- diff(master[ok])
    r  <- if (length(dv) >= 4L && stats::sd(dv) > 0 && stats::sd(dm) > 0)
            suppressWarnings(stats::cor(dv, dm)) else NA_real_
    data.table(file = rel, series = s, n_overlap = sum(ok), r = r)
  }))
}

if (sys.nframe() == 0L) {
  files <- unique(cand$file)
  nw <- max(1L, min(8L, parallel::detectCores() - 2L))
  plan(multisession, workers = nw)
  res <- future_lapply(files, function(f) {
    suppressPackageStartupMessages({ library(dplR); library(data.table) })
    source("QA_Stuff/read_tucson2.R", local = FALSE)
    adjudicate_file(f)
  }, future.globals = c("adjudicate_file", "cand", "ROOT"),
     future.packages = c("dplR", "data.table"), future.seed = TRUE)

  xd  <- rbindlist(res, fill = TRUE)
  out <- merge(cand, xd, by = c("file", "series"), all.x = TRUE)

  ## The reportable ones: short relative to their own file, and a minority of it.
  ## Everything else stays in the CSV with its numbers, marked "context only".
  out[, report := med_span >= 5 * span & frac_short < 0.20]
  ## AGB Aug 2026: three tiers, so the CSV keeps all the evidence but only the
  ## top tier is worth NOAA's time. Crossdating over eight years is weak -- a
  ## correlation of 0.5 on that many points is not significant -- so it is used
  ## to RANK, never to acquit or convict on its own.
  out[, tier := fifelse(!report, "collection style",
               fifelse(is.na(r) | r < 0.3, "investigate", "likely a real short core"))]
  setorder(out, -report, r, file, series)
  write.csv(out, "QA_Stuff/short-series-adjudicated.csv", row.names = FALSE)

  cat("\ncandidates            :", nrow(out), "\n")
  print(table(out$tier))
}
