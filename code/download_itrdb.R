library(httr)
library(XML)
library(utils)
library(curl)

dif_url <-
    "http://www1.ncdc.noaa.gov/pub/data/metadata/published/paleo/dif/xml/"
base_dir <- getwd()
k_max_tries <- 3

if (!dir.exists(base_dir)) {
    dir.create(base_dir)
}
dif_dir <- file.path(base_dir, "dif_files")
if (!dir.exists(dif_dir)) {
    dir.create(dif_dir)
}
study_dir <- file.path(base_dir, "study_files")
if (!dir.exists(study_dir)) {
    dir.create(study_dir)
}
data_dir <- file.path(base_dir, "data_files")
if (!dir.exists(data_dir)) {
    dir.create(data_dir)
}

## Get the names of all tree-related DIF metadata files.
foo <- content(GET(dif_url), "text")
dif_names <-
    regmatches(foo,
               gregexpr("[hH][rR][eE][fF]\\s*=\\s*\"[^\"]+",
                        foo, perl = TRUE))[[1L]]
dif_names <- grep("-tree-", dif_names, fixed = TRUE, value = TRUE)
dif_names <- sub("^[hH][rR][eE][fF]\\s*=\\s*\"", "", dif_names, perl = TRUE)

n_dif <- length(dif_names)

## Download DIF metadata files. Only if file does not exist.
cat("Downloading / checking presence of DIF files...\n")
pb <- txtProgressBar(0, n_dif, style = 3)
for (k in seq_len(n_dif)) {
    local_path <- file.path(dif_dir, dif_names[k])
    if (!file.exists(local_path)) {
        download.file(paste0(dif_url, dif_names[k]), local_path, quiet = TRUE)
    }
    setTxtProgressBar(pb, k)
}
close(pb)

## Process. This example finds the linked "study" URL (which can also
## be deduced from the DIF filename) and any other linked URLs in each
## DIF file.
dif_study <- character(n_dif)
dif_other <- vector(mode = "list", length = n_dif)
for (k in seq_len(n_dif)) {
    local_path <- file.path(dif_dir, dif_names[k])
    tree_k <- xmlTreeParse(local_path, useInternalNodes = TRUE)
    ns_url <- xmlNamespaceDefinitions(tree_k)[[1]][["uri"]]
    stopifnot(grepl("dif", ns_url, fixed = TRUE))
    study_k <- xpathSApply(tree_k, "//d:Data_Set_Citation//d:Online_Resource/text()",
                           xmlValue,
                           namespaces = c(d = ns_url))
    stopifnot(length(study_k) == 1)
    dif_study[k] <- study_k
    related_k <-
        suppressWarnings(xpathSApply(tree_k, "//d:Related_URL//d:URL/text()",
                                     xmlValue,
                                     namespaces = c(d = ns_url)))
    dif_other[[k]] <- setdiff(related_k, study_k)

    free(tree_k)
}

## Download "study" files (only if not already present).
cat("Downloading / checking presence of study files...\n")
pb <- txtProgressBar(0, n_dif, style = 3)
for (k in seq_len(n_dif)) {
    local_path <- file.path(study_dir,
                            paste0("study_", basename(dif_study[k]), ".html"))
    if (!file.exists(local_path)) {
        cat(content(GET(dif_study[k]), "text"), "\n", sep = "",
            file = local_path)
    }
    setTxtProgressBar(pb, k)
}
close(pb)

## List non-metadata files linked to by each "study" intro page
study_urls <- vector(mode = "list", length = n_dif)
for (k in seq_len(n_dif)) {
    local_path <- file.path(study_dir,
                            paste0("study_", basename(dif_study[k]), ".html"))
    foo <- paste0(readLines(local_path), collapse = "\n")
    data_urls <-
        regmatches(foo,
                   gregexpr("[hH][rR][eE][fF]\\s*=\\s*\"[^\"]+",
                            foo, perl = TRUE))[[1L]]
    data_urls <- grep("/pub/data/", data_urls, fixed = TRUE, value = TRUE)
    data_urls <- grep("/dif/", data_urls, fixed = TRUE, value = TRUE,
                       invert = TRUE)
    data_urls <- sub("^[hH][rR][eE][fF]\\s*=\\s*\"", "", data_urls, perl = TRUE)
    study_urls[[k]] <- data_urls
}


## Normalize URLs by removing prefix until "pub/data/"
normalize_url <- function (x) {
    sub("^.*pub/data/paleo/", "", x)
}

dif_other_norm <- lapply(dif_other, normalize_url)
study_urls_norm <- lapply(study_urls, normalize_url)

files1 <- unlist(dif_other_norm)
files2 <- unlist(study_urls_norm)
setdiff(files2, files1)

tmp <- c(files1, files2)
files_combined <- c(unlist(dif_other), unlist(study_urls))
files_combined <- files_combined[!duplicated(tmp) & !grepl("^http", tmp)]
files_combined <- grep("/$", files_combined, invert = TRUE, value = TRUE)
stopifnot(all(grepl("pub/data/paleo/", files_combined, fixed = TRUE)))
length(files_combined)

## Download data files (only if not already present).
n_data <- length(files_combined)
files_norm <- normalize_url(files_combined)
files_norm <- strsplit(files_norm, "/", fixed = TRUE)
files_norm <- vapply(files_norm,
                     function (x) do.call(file.path, as.list(x)), "")
unique_dirs <- unique(dirname(files_norm))
for (ud in unique_dirs) {
    local_path <- file.path(data_dir, ud)
    if (!dir.exists(local_path)) {
        dir.create(local_path, recursive = TRUE)
    }
}
cat("Downloading / checking presence of data files...\n")
pb <- txtProgressBar(0, n_data, style = 3)
queue <- rep(TRUE, n_data)
counter <- 0
n_tries <- rep(0, n_data)
download_or_remove <- function(src, dest) {
    on.exit(unlink(dest))
    tryCatch({
        curl_download(src, dest)
        if (file.exists(dest) && file.size(dest) > 0)
            on.exit()
    }, error = function (e) e)
}
while (counter < n_data) {
    counter0 <- counter
    for (k in which(queue)) {
        local_path <- file.path(data_dir, files_norm[k])
        if ((file.exists(local_path) && file.size(local_path) > 0) ||
            n_tries[k] == k_max_tries ||
            !inherits(tryCatch(download_or_remove(files_combined[k],
                                                  local_path),
                               error = function (e) {
                                   Sys.sleep(2)
                                   e
                               }),
                      "error")) {
            counter <- counter + 1
            queue[k] <- FALSE
            setTxtProgressBar(pb, counter)
        } else {
            n_tries[k] <- n_tries[k] + 1
        }
    }
    if (all(n_tries %in% c(0, k_max_tries))) {
        for (k in which(n_tries == k_max_tries)) {
            warning(sprintf("could not download '%s'", files_combined[k]))
        }
        break
    }
    if (counter == counter0) {
        warning("repeated errors, retrying with a delay of 1 minute")
        Sys.sleep(60)
    }
}
close(pb)

# How to filter and keep only measurements using workflow above?
# Tricky because of the way the dif files are central
# Easiest thing is to post process and remove.
# Lame but direct.


