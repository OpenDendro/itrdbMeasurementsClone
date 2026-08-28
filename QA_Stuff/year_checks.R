# AGB -- Aug 2026
# Look for dating problems in the parsed rwl objects. Run after 02_getRWLs.R.
#
# Two tests:
#
#   1. Years in the future. A ring cannot postdate the collection, so any series
#      running past this year is wrong.
#
#   2. A series that ends long after every other series in its study. This is
#      the NC4 signature: WNCH15-1 was dated 1894-2063 while the other 31 series
#      in the study ended in 1992, because its decade fields were keyed 71 years
#      late. The absolute future-year test caught that one only because the
#      error happened to push it past the present day. A smaller offset -- say
#      the same mistake made in 1930 -- would have passed unnoticed. This test
#      catches the shape of the error rather than its size.
#
# Test 2 only applies where the study looks like a living-tree collection, i.e.
# most series end within a few years of each other because they were cut or
# cored on one trip. Subfossil and archaeological collections legitimately have
# series ending centuries apart, so they are skipped.
#
# Test 2 on its own is a weak screen -- a tree that simply outlived its
# neighbours looks the same as one dated wrongly. So every candidate is then
# crossdated against the rest of its own study: detrend, build a master from the
# other series, and slide the candidate against it. If it correlates best where
# the archive puts it, the dating is fine and the flag was noise. On the first
# run this took four candidates down to one worth investigating.

rm(list=ls())
suppressPackageStartupMessages(library(dplR))

rwls <- readRDS("Rdatafiles/rwls.rds")
meta <- readRDS("Rdatafiles/rwls_meta.rds")
stopifnot(length(rwls) == nrow(meta))

this_year <- as.integer(format(Sys.Date(), "%Y"))

## how tightly the bulk of a study's series must end for test 2 to apply
cluster_window <- 20   # years
cluster_share <- 0.75  # share of series that must fall inside it
outlier_gap <- 30      # years beyond the rest before we call it out

ids <- if (!is.null(names(rwls))) names(rwls) else rownames(meta)
future <- list()
outliers <- list()

pb <- txtProgressBar(0, length(rwls), style = 3)
for (i in seq_along(rwls)) {
  setTxtProgressBar(pb, i)
  x <- rwls[[i]]
  if (inherits(x, "try-error") || is.null(dim(x)) || ncol(x) == 0) next
  yrs <- suppressWarnings(as.numeric(rownames(x)))
  if (all(is.na(yrs))) next
  ## last year with a measurement, per series
  last_yr <- vapply(x, function(s) {
    ok <- which(!is.na(s))
    if (length(ok) == 0) NA_real_ else yrs[max(ok)]
  }, numeric(1))
  if (all(is.na(last_yr))) next

  ## ---- test 1: the future ----
  fut <- which(last_yr > this_year)
  if (length(fut) > 0) {
    future[[length(future) + 1]] <- data.frame(
      StudyID = ids[i], XML_FileName = meta$XML_FileName[i],
      series = names(x)[fut], series_last_year = last_yr[fut],
      study_last_year = max(last_yr, na.rm = TRUE),
      n_series = ncol(x), test = "ends in the future")
  }

  ## ---- test 2: one series far past the rest ----
  if (ncol(x) >= 5) {
    med <- median(last_yr, na.rm = TRUE)
    inside <- sum(abs(last_yr - med) <= cluster_window, na.rm = TRUE) / sum(!is.na(last_yr))
    if (inside >= cluster_share) {
      for (j in which(!is.na(last_yr))) {
        rest <- last_yr[-j]
        if (last_yr[j] - max(rest, na.rm = TRUE) >= outlier_gap) {
          outliers[[length(outliers) + 1]] <- data.frame(
            StudyID = ids[i], XML_FileName = meta$XML_FileName[i],
            series = names(x)[j], series_last_year = last_yr[j],
            study_last_year = max(rest, na.rm = TRUE),
            n_series = ncol(x), test = "ends long after the rest of the study")
        }
      }
    }
  }
}
close(pb)

cat("\nseries ending after", this_year, ":", length(future), "\n")
cat("series ending well after the rest of their study:", length(outliers), "\n")

## ---- adjudicate test 2 by crossdating ------------------------------------
## Slide the flagged series against a master built from the rest of its study.
## Best offset of zero means the archive has it right.
crossdate_offset <- function(id, series, lags = -120:120, min_overlap = 50) {
  x <- rwls[[id]]
  rwi <- try(suppressWarnings(detrend(x, method = "Spline", nyrs = 32,
                                      verbose = FALSE)), silent = TRUE)
  if (inherits(rwi, "try-error")) return(c(NA, NA, NA))
  s <- rwi[[series]]
  master <- rowMeans(rwi[, setdiff(names(rwi), series), drop = FALSE], na.rm = TRUE)
  r <- vapply(lags, function(L) {
    idx <- seq_along(s) - L
    keep <- idx >= 1 & idx <= length(s)
    a <- s[idx[keep]]; b <- master[keep]
    ok <- !is.na(a) & !is.na(b) & is.finite(a) & is.finite(b)
    if (sum(ok) < min_overlap) return(NA_real_)
    suppressWarnings(cor(a[ok], b[ok]))
  }, numeric(1))
  if (all(is.na(r))) return(c(NA, NA, NA))
  c(r_at_zero = r[lags == 0], best_offset = lags[which.max(r)],
    r_at_best = max(r, na.rm = TRUE))
}

if (length(outliers) > 0) {
  cat("crossdating the flagged series against their own studies...\n")
  for (k in seq_along(outliers)) {
    o <- crossdate_offset(outliers[[k]]$StudyID, outliers[[k]]$series)
    outliers[[k]]$r_at_archive_dating <- round(o[1], 3)
    outliers[[k]]$best_offset_yr <- o[2]
    outliers[[k]]$r_at_best_offset <- round(o[3], 3)
    outliers[[k]]$verdict <- ifelse(is.na(o[2]), "too little overlap to test",
      ifelse(o[2] == 0, "crossdates where the archive puts it -- dating looks fine",
             "crossdates better elsewhere -- worth investigating"))
  }
}

for (k in seq_along(future)) {
  future[[k]]$r_at_archive_dating <- NA_real_
  future[[k]]$best_offset_yr <- NA_real_
  future[[k]]$r_at_best_offset <- NA_real_
  future[[k]]$verdict <- "impossible date"
}

out <- do.call(rbind, c(future, outliers))
if (is.null(out)) out <- data.frame()

if (nrow(out) > 0) {
  out <- out[order(out$test, -out$series_last_year), ]
  write.csv(out, "QA_Stuff/year-oddities.csv", row.names = FALSE)
  cat("\nwrote QA_Stuff/year-oddities.csv\n\n")
  print(head(out, 25), row.names = FALSE)
} else {
  cat("\nnothing to report. QA_Stuff/year-oddities.csv not written.\n")
}
