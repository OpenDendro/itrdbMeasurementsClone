library(httr)
library(XML)
library(utils)
library(curl)

## Dendrochronology Species Database, values extracted manually
## http://www.wsl.ch/dienstleistungen/produkte/glossare/dendro_species/index_EN
k_code_table <-
    c(ACSH = "Acer saccharum Marsh.",
      AGAU = "Agathis australis (D. Don) Loudon",
      CDAT = "Cedrus atlantica (Endl.) Manetti ex Carri\ue8re",
      CUCH = "Cupressus chengiana S.Y.Hu", # AGB added
      FOHO = "Fokienia hodginsii (Dunn) A.Henry & H.H.Thomas", # AGB added
      HEHE = "Hedera helix L. (Araliaceae)", # AGB added
      LADE = "Larix decidua Mill.",
      PCAB = "Picea abies (L.) H. Karst.",
      PCGL = "Picea glauca (Moench) Voss",
      PHAS = "Phyllocladus aspleniifolius (Labill.) Hook. f.",
      PIED = "Pinus edulis Engelm.",
      PISY = "Pinus sylvestris L.",
      PITB = "Pinus tabuliformis Carri\ue8re",
      PIWA = "Pinus wallichiana A.B. Jacks.",
      PPTM = "Populus tremuloides Michx.", # AGB added
      PSME = "Pseudotsuga menziesii (Mirb.) Franco",
      PTLE = "Pistacia lentiscus L.", # AGB added
      QUSP = "Quercus L.",
      THOC = "Thuja occidentalis L.",
      TSCA = "Tsuga canadensis (L.) Carri\ue8re")

StudyID <- character(n_dif)
Species <- character(n_dif)
Lat <- numeric(n_dif)
Long <- numeric(n_dif)
Altitude <- numeric(n_dif)
EntryTitle <- character(n_dif)
itrdb_rwl <- vector(mode = "list", length = n_dif)
itrdb_crn <- vector(mode = "list", length = n_dif)
itrdb_other <- vector(mode = "list", length = n_dif)
get_one <- function(...) {
    tags <- unlist(list(...))
    query <- paste0(paste0("//d:", tags, collapse = ""), "/text()")
    ## may return NULL
    suppressWarnings(xpathSApply(tree_k, query, xmlValue,
                                 namespaces = c(d = ns_url)))
}
get_nodeset <- function(...) {
    tags <- unlist(list(...))
    query <- paste0("//d:", tags, collapse = "")
    ## may return NULL
    getNodeSet(tree_k, query,  namespaces = c(d = ns_url))
}
get_pages_species <- function(keywords) {
    idx_upper <- grep("^[[:upper:]]+$", keywords)
    if (length(idx_upper) > 0 && idx_upper[1] < length(keywords)) {
        keywords[idx_upper[1] + 1]
    } else {
        NA_character_
    }
}
get_species_code <- function(filename) {
    local_path <- file.path(data_dir, filename)
    raw_bytes <- readBin(local_path, "raw", 65)
    if (any(raw_bytes[1:61] %in% c(charToRaw("\n"), charToRaw("\r")))) {
        NA_character_
    } else {
        tmp <- gsub("^[[:space:]]+|[[:space:]]+$", "",
                    rawToChar(raw_bytes[62:65]))
        if (grepl("^[[:upper:]]+$", tmp)) {
            tmp
        } else {
            NA_character_
        }
    }
}
get_species_code_name <- function(filename) {
    local_path <- file.path(data_dir, filename)
    raw_bytes <- readBin(local_path, "raw", 300)
    newline_bytes <- c(charToRaw("\n"), charToRaw("\r"))
    is_newline <- raw_bytes %in% newline_bytes
    if (any(is_newline[1:61])) {
        c(NA_character_, NA_character_)
    } else {
        tmp <- gsub("^[[:space:]]+|[[:space:]]+$", "",
                    rawToChar(raw_bytes[62:65]))
        if (grepl("^[[:upper:]]+$", tmp)) {
            species_code <- tmp
            if (!any(is_newline[1:277])) {
                species_name <- NA_character_
            } else {
                start_2nd <- which.max(is_newline[1:277]) + 1
                while (start_2nd <= 278 && is_newline[start_2nd]) {
                    start_2nd <- start_2nd + 1
                }
                if (start_2nd == 279) {
                    species_name <- NA_character_
                } else {
                    idx_2nd <- seq(from = start_2nd,
                                   to = min(300, start_2nd + 39))
                    if (any(is_newline[idx_2nd])) {
                        idx_2nd <- seq(from = start_2nd,
                                       to = start_2nd - 2 +
                                           which.max(is_newline[idx_2nd]))
                    }
                    n_2nd <- length(idx_2nd)
                    if (n_2nd < 23) {
                        species_name <- NA_character_
                    } else {
                        species_name <-
                            iconv(list(raw_bytes[idx_2nd[23:n_2nd]]),
                                  from = "latin1")
                        species_name <- gsub("^[[:space:]]+|[[:space:]]+$", "",
                                             species_name)
                        if (grepl("[[:digit:]]", species_name) ||
                            !nzchar(species_name)) {
                            species_name <- NA_character_
                        }
                    }
                }
            }
            c(species_code, species_name)
        } else {
            c(NA_character_, NA_character_)
        }
    }
}
not_ring_width <- integer(0)
not_rwl_crn <- integer(0)
keywords_not_ring_width <- list()
detailed_not_ring_width <- list()
species_prefix <- "^earth science>paleoclimate>tree-ring>tree species>"
species_start <- nchar(species_prefix)
# coord issues
check_coord_info <- integer(0)
check_coord_info2 <- integer(0)

cat("Building file lists, species, altitude, coordinates, etc.\n")
pb <- txtProgressBar(0, n_dif, style = 3)
for (k in seq_len(n_dif)) {
    ## Require that either raw measurements or (original) chronologies
    ## are present.
    file_list <- dif_other_norm[[k]]
    if (!any(grepl("\\bmeasurements\\b", file_list, perl = TRUE) &
             !grepl("\\bmeasurements/correlation\\b",
                    file_list, perl = TRUE)) &&
        !any(grepl("\\bchronologies\\b.*\\.crn", file_list, perl = TRUE))) {
        not_rwl_crn <- c(not_rwl_crn, k)
        next
    }
    local_path <- file.path(dif_dir, dif_names[k])
    tree_k <- xmlTreeParse(local_path, useInternalNodes = TRUE)
    ns_url <- xmlNamespaceDefinitions(tree_k)[[1]][["uri"]]
    keywords <- get_one("Keyword")
    ## Skip studies that don't have ring width as a variable
    if (!any(grepl("earth science>paleoclimate>tree-ring>width>ring width",
                   keywords, fixed = TRUE))) {
        detailed_var <- get_one("Parameters", "Detailed_Variable")
        parameters <- get_nodeset("Parameters")
        width_found <- FALSE
        species <- NA_character_
        for (l in seq_along(parameters)) {
            values <- vapply(xmlChildren(parameters[[l]]), xmlValue, "")
            if (all(c("Term", "Variable_Level_1") %in% names(values)) &&
                identical(values[["Term"]], "tree-ring")) {
                if (identical(values[["Variable_Level_1"]], "width")) {
                    width_found <- TRUE
                } else if ("Detailed_Variable" %in% names(values) &&
                           identical(values[["Variable_Level_1"]],
                                     "tree species")) {
                    species <- values[["Detailed_Variable"]]
                }
            }
            if (width_found && !is.na(species)) {
                break
            }
        }
        if (width_found ||
            any(grepl("\\bring width\\b", detailed_var, perl = TRUE,
                      ignore.case = TRUE))) {
            if (is.na(species)) {
                Species[k] <- get_pages_species(keywords)
            } else {
                Species[k] <- species
            }
            Species[k] <- gsub("[[:space:]]+", " ", Species[k])
        } else {
            not_ring_width <- c(not_ring_width, k)
            keywords_not_ring_width[length(not_ring_width)] <-
                list(keywords)
            detailed_not_ring_width[length(not_ring_width)] <-
                list(detailed_var)
            next
        }
    } else {
        species_idx <- grep(species_prefix, keywords)
        stopifnot(length(species_idx) == 1)
        Species[k] <- gsub("[[:space:]]+", " ",
                           substr(keywords[species_idx], species_start,
                                  nchar(keywords[species_idx])))
    }
    itrdb_rwl[[k]] <- grep("\\.rwl$", dif_other_norm[[k]],
                           ignore.case = TRUE, value = TRUE)
    itrdb_crn[[k]] <- grep("\\.crn$", dif_other_norm[[k]],
                           ignore.case = TRUE, value = TRUE)
    itrdb_other[[k]] <- setdiff(dif_other_norm[[k]],
                                c(itrdb_rwl[[k]], itrdb_crn[[k]]))
    ## Try extracting species code from rwl headers
    if (is.na(Species[k])) {
        tmp <- vapply(c(itrdb_rwl[[k]], itrdb_crn[[k]]),
                      get_species_code_name, c("", ""))
        tmp <- tmp[, which(!is.na(tmp[1, ])), drop = FALSE]
        coded_species <- k_code_table[tmp[1, ]]
        tmp <- ifelse(is.na(coded_species), tmp[2, ], coded_species)
        tmp <- gsub("[[:space:]]+", " ", unique(tmp[!is.na(tmp)]))
        if (length(tmp) > 0) {
            ## After running the script, it is evident that this
            ## branch has not been visited.
            ##
            ## > grep("/", itrdb_meta[["Species"]], value = TRUE)
            ## character(0)
            Species[k] <- paste0(tmp, collapse = " / ")
        }
    }
    southern <- get_one("Spatial_Coverage", "Southernmost_Latitude")
    northern <- get_one("Spatial_Coverage", "Northernmost_Latitude")
    western <- get_one("Spatial_Coverage", "Westernmost_Longitude")
    eastern <- get_one("Spatial_Coverage", "Easternmost_Longitude")
    min_alt <- get_one("Spatial_Coverage", "Minimum_Altitude")
    max_alt <- get_one("Spatial_Coverage", "Maximum_Altitude")
    EntryTitle[k] <- get_one("Entry_Title")
    StudyID[k] <- sub(".*\\b([[:alpha:]]+[[:digit:]]*)[[:space:]]*$", "\\1",
                      EntryTitle[k], perl = TRUE)
    # write these to a warning message?
    if (!identical(southern, northern)) {
      check_coord_info <- c(check_coord_info,k)
        # message(sprintf("\nsouthern- and northernmost latitudes differ, study ID %s:",
        #                 StudyID[k]))
        # cat(sprintf("south = %s, north = %s\n", southern, northern))
    }
    if (!identical(western, eastern)) {
      check_coord_info2 <- c(check_coord_info2,k)
      # message(sprintf("\nwestern- and easternmost longitudes differ, study ID %s:",
      #                   StudyID[k]))
      #   cat(sprintf("west = %s, east = %s\n", western, eastern))
    }
    # ^^^^^
    Lat[k] <- mean(as.numeric(c(southern, northern)))
    Long[k] <- mean(as.numeric(c(western, eastern)))
    tmp <- mean(as.numeric(c(min_alt, max_alt)))
    if (is.finite(tmp) && tmp > -500) {
        Altitude[k] <- tmp
    } else {
        Altitude[k] <- NA_real_
    }
    tmp <- get_one("Spatial_Coverage", "Altitude_Unit")
    if (!is.null(tmp)) {
        warning("Altitude_Unit present, please check")
    }
    free(tree_k)
    setTxtProgressBar(pb, k)
}
close(pb)

## sink("~/temp/foo.txt")
## for (k in not_ring_width) {
##     local_path <- file.path(dif_dir, dif_names[k])
##     tree_k <- xmlTreeParse(local_path, useInternalNodes = TRUE)
##     print(k)
##     print(tree_k)
##     cat("\n\n\n\n\n\n\n\n\n\n")
##     free(tree_k)
## }
## sink()

bad_id <- StudyID[nchar(StudyID) > 10 | grepl("[[:space:]]", StudyID)]
if (length(bad_id) > 0) {
    warning("Study ID extraction may not work, see the list of strange IDs")
    cat("Strange IDs:\n")
    print(bad_id)
}

itrdb_meta <- data.frame(Species = factor(Species),
                         Lat = Lat, Long = Long, Altitude = Altitude,
                         RWL_Count = vapply(itrdb_rwl, length, 0),
                         CRN_Count = vapply(itrdb_crn, length, 0),
                         XML_FileName = dif_names,
                         stringsAsFactors = FALSE)
idx_drop <- c(not_ring_width, not_rwl_crn)
itrdb_meta <- itrdb_meta[-idx_drop, ]
itrdb_rwl <- itrdb_rwl[-idx_drop]
itrdb_crn <- itrdb_crn[-idx_drop]
tmp <- StudyID[-idx_drop]

# are there dups?
any(duplicated(tmp))
#tmp[which(duplicated(tmp))]
#itrdb_meta[which(duplicated(tmp)),]


row.names(itrdb_meta) <- tmp
names(itrdb_rwl) <- tmp
names(itrdb_crn) <- tmp
rm(tmp)

# areas with >1 coord. Might be a feature.
# check_coord_info
# check_coord_info2
# all(check_coord_info %in% check_coord_info2)
# all(check_coord_info2 %in% check_coord_info)
#
# StudyID[check_coord_info[1]]
# EntryTitle[check_coord_info[1]]
# dif_study[check_coord_info[1]]
# paste0(dif_url,dif_names[check_coord_info[1]])
#
#
# dif_study[check_coord_info]
