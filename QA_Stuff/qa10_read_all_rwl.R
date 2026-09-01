## Pass 2 of the ITRDB file sweep: read every .rwl with both readers
## AGB -- Aug 2026
##
## AGB Aug 2026: reads all *.rwl on disk with dplR::read.tucson() and with
## read.tucson2(), records what each one said, and boils each file down to a few
## small tables. It deliberately does NOT keep the parsed rwl objects: chin067
## alone is 442,586 measurements and the archive will not fit in memory. The
## later passes work from the summaries written here.
##
## The file list is taken fresh from disk rather than from cleaned_itrdb.Rdata.
## 02_getRWLs.R keeps only the shortest filename per study, so the checkpoint has
## no row at all for germ012l, ok049e, va20l and every other earlywood/latewood
## variant. We want all of them.
##
## Writes RdataFiles/qa_read_all.Rdata

suppressPackageStartupMessages({
  library(dplR); library(data.table); library(future.apply)
})

ROOT <- "data_files/treering/measurements"

## ---- capture whatever a reader says, without letting it stop the sweep -----
quietly <- function(expr) {
  w <- character(); e <- NA_character_
  v <- withCallingHandlers(
    tryCatch(suppressMessages(expr), error = function(x) { e <<- conditionMessage(x); NULL }),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  list(value = v, warn = w, err = e)
}

## ---- long form, for comparing the two readers cell by cell ----------------
as_long <- function(x) {
  if (is.null(x) || !ncol(x)) return(NULL)
  d <- data.table(series = rep(colnames(x), each = nrow(x)),
                  year   = rep(as.integer(rownames(x)), ncol(x)),
                  rw     = unlist(x, use.names = FALSE))
  d[!is.na(rw)]
}

## ---- per-series summary ---------------------------------------------------
## Ring widths are roughly lognormal, so the robust z used later is computed on
## logs of the positive values. Zeros are left out of that: a zero is a locally
## absent ring, which is normal and must not be treated as an outlier.
series_stats <- function(x, f) {
  if (is.null(x) || !ncol(x)) return(NULL)
  yrs <- as.integer(rownames(x))
  rbindlist(lapply(colnames(x), function(s) {
    v <- x[[s]]; ok <- !is.na(v)
    if (!any(ok)) return(NULL)
    vv <- v[ok]; yy <- yrs[ok]
    pos <- vv[vv > 0]
    lg  <- if (length(pos)) log(pos) else numeric(0)
    md  <- if (length(lg)) stats::median(lg) else NA_real_
    ma  <- if (length(lg)) stats::mad(lg)    else NA_real_
    z   <- if (!is.na(ma) && ma > 0) abs(log(pmax(vv, 1e-6)) - md) / ma else rep(NA_real_, length(vv))
    k   <- if (all(is.na(z))) NA_integer_ else which.max(z)
    data.table(
      file = f, series = s,
      first = min(yy), last = max(yy), span = max(yy) - min(yy) + 1L,
      n = length(vv), n_zero = sum(vv == 0),
      med = stats::median(vv), min = min(vv), max = max(vv),
      out_z = if (is.na(k)) NA_real_ else z[k],
      out_val = if (is.na(k)) NA_real_ else vv[k],
      out_year = if (is.na(k)) NA_integer_ else yy[k])
  }))
}

## ---- one file -------------------------------------------------------------
read_one <- function(f) {
  old <- quietly(read.tucson(f, verbose = FALSE))
  new <- quietly(read.tucson2(f, verbose = FALSE))

  ## Plausibility stats come from read.tucson2 where it succeeded, because it is
  ## the reader that refuses to invent values. read.tucson() is the fallback so
  ## that a file only read by the old reader still gets checked.
  prim <- if (!is.null(new$value)) new$value else old$value
  src  <- if (!is.null(new$value)) "read.tucson2" else if (!is.null(old$value)) "read.tucson" else NA_character_

  ## AGB Aug 2026: the disagreement count has to be split by KIND, or it says
  ## nothing. On the first full run 3,259 files "disagreed", and almost all of
  ## it was one known, deliberate difference: read.tucson() fills interior gaps
  ## with zeros and read.tucson2() leaves them NA. That is a single fact about
  ## the two readers, not 3,259 broken files. Only dis_value and dis_oldonly
  ## mean the file itself is ambiguous.
  ndis <- NA_integer_; ex <- NULL
  dz <- dv <- do <- dn <- NA_integer_
  if (!is.null(old$value) && !is.null(new$value)) {
    A <- as_long(old$value); B <- as_long(new$value)
    setkey(A, series, year); setkey(B, series, year)
    M <- merge(A, B, all = TRUE, suffixes = c(".old", ".new"))
    bad <- M[is.na(rw.old) | is.na(rw.new) | abs(rw.old - rw.new) > 1e-8]
    ndis <- nrow(bad)
    dz <- bad[!is.na(rw.old) & rw.old == 0 & is.na(rw.new), .N]   # zero-fill only
    do <- bad[!is.na(rw.old) & rw.old != 0 & is.na(rw.new), .N]   # new refused a real value
    dn <- bad[is.na(rw.old) & !is.na(rw.new), .N]                 # old missed a value
    dv <- bad[!is.na(rw.old) & !is.na(rw.new), .N]                # both read it, differently
    if (ndis) ex <- cbind(file = f, head(bad[order(-abs(rw.old - rw.new))], 5))
  }

  list(
    file = data.table(
      file      = f,
      old_ok    = !is.null(old$value), new_ok = !is.null(new$value),
      old_err   = old$err,             new_err = new$err,
      n_old_warn = length(old$warn),   n_new_warn = length(new$warn),
      n_series  = if (is.null(prim)) NA_integer_ else ncol(prim),
      n_cells   = if (is.null(prim)) NA_integer_ else sum(!is.na(prim)),
      yr_first  = if (is.null(prim)) NA_integer_ else min(as.integer(rownames(prim))),
      yr_last   = if (is.null(prim)) NA_integer_ else max(as.integer(rownames(prim))),
      n_disagree = ndis, dis_zerofill = dz, dis_value = dv,
      dis_oldonly = do, dis_newonly = dn, stats_from = src),
    warn   = if (length(old$warn) + length(new$warn))
               data.table(file = f,
                          reader = rep(c("read.tucson", "read.tucson2"),
                                       c(length(old$warn), length(new$warn))),
                          msg = c(old$warn, new$warn)) else NULL,
    series = series_stats(prim, f),
    ex     = ex)
}

## ---- main -----------------------------------------------------------------
if (sys.nframe() == 0L) {
  files <- list.files(ROOT, pattern = "\\.rwl$", recursive = TRUE, full.names = TRUE)
  cat("files:", length(files), "\n")

  nw <- max(1L, min(8L, parallel::detectCores() - 2L))
  plan(multisession, workers = nw)
  cat("workers:", nw, "\n")

  t0 <- Sys.time()
  res <- future_lapply(files, function(f) {
    suppressPackageStartupMessages({ library(dplR); library(data.table) })
    source("QA_Stuff/read_tucson2.R", local = FALSE)
    tryCatch(read_one(f), error = function(e)
      list(file = data.table(file = f, old_ok = NA, new_ok = NA,
                             old_err = NA_character_, new_err = paste("SWEEP:", conditionMessage(e)),
                             n_old_warn = NA_integer_, n_new_warn = NA_integer_,
                             n_series = NA_integer_, n_cells = NA_integer_,
                             yr_first = NA_integer_, yr_last = NA_integer_,
                             n_disagree = NA_integer_, dis_zerofill = NA_integer_,
                             dis_value = NA_integer_, dis_oldonly = NA_integer_,
                             dis_newonly = NA_integer_, stats_from = NA_character_),
           warn = NULL, series = NULL, ex = NULL))
  }, future.globals = c("read_one", "quietly", "as_long", "series_stats"),
     future.packages = c("dplR", "data.table"), future.seed = TRUE)
  cat("elapsed:", format(Sys.time() - t0), "\n")

  qa_files  <- rbindlist(lapply(res, `[[`, "file"),   fill = TRUE)
  qa_warn   <- rbindlist(lapply(res, `[[`, "warn"),   fill = TRUE)
  qa_series <- rbindlist(lapply(res, `[[`, "series"), fill = TRUE)
  qa_ex     <- rbindlist(lapply(res, `[[`, "ex"),     fill = TRUE)
  qa_files[,  file := sub(paste0("^", ROOT, "/"), "", file)]
  if (nrow(qa_warn))   qa_warn[,   file := sub(paste0("^", ROOT, "/"), "", file)]
  if (nrow(qa_series)) qa_series[, file := sub(paste0("^", ROOT, "/"), "", file)]
  if (nrow(qa_ex))     qa_ex[,     file := sub(paste0("^", ROOT, "/"), "", file)]

  dir.create("RdataFiles", showWarnings = FALSE)
  save(qa_files, qa_warn, qa_series, qa_ex, file = "RdataFiles/qa_read_all.Rdata")

  cat("\nfiles read by both  :", sum(qa_files$old_ok & qa_files$new_ok, na.rm = TRUE), "\n")
  cat("read.tucson failed  :", sum(!qa_files$old_ok, na.rm = TRUE), "\n")
  cat("read.tucson2 failed :", sum(!qa_files$new_ok, na.rm = TRUE), "\n")
  cat("\ndisagreement, split by kind (files affected):\n")
  cat("  zero-fill only (read.tucson invents 0, read.tucson2 says NA):",
      sum(qa_files$dis_zerofill > 0, na.rm = TRUE), "\n")
  cat("  both read a value, values differ                           :",
      sum(qa_files$dis_value > 0, na.rm = TRUE), "\n")
  cat("  read.tucson2 refused a non-zero value read.tucson accepted :",
      sum(qa_files$dis_oldonly > 0, na.rm = TRUE), "\n")
  cat("  read.tucson2 found a value read.tucson missed              :",
      sum(qa_files$dis_newonly > 0, na.rm = TRUE), "\n")
  cat("series rows:", nrow(qa_series), " warning rows:", nrow(qa_warn), "\n")
}
