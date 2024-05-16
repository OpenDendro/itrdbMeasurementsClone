# # HN: compare old and new read.tucson.
# # Making sure that the new one gets it right whenever the old one gets it right
# # and improve on the other cases
# 
# library(doFuture)
# source("QA_Stuff/read_tucson2.R")
# 
# load("RdataFiles/cleaned_itrdb.Rdata")
# head(itrdb_meta)
# # get studies with RWL files
# studies2get <- itrdb_meta$RWL_Count > 0
# summary(studies2get)
# studies_gt_one <- itrdb_meta$RWL_Count > 1
# summary(studies_gt_one)
# 
# rwls_meta <- itrdb_meta[studies2get, ]
# rwls_meta <- droplevels(rwls_meta)
# head(rwls_meta)
# 
# rwls2get <- itrdb_rwl[studies2get]
# 
# fnameUsed <- sapply(rwls2get, \(s) {
#    fname <- if (length(s) > 1) {
#       getShorty <- which.min(nchar(s))
#       s[getShorty]
#    } else s
#    paste0("./data_files/", fname)
# })
# 
# plan(multisession)
# checks <- foreach(s = fnameUsed, .final = unlist, .inorder = FALSE) %dofuture% {
#    foo1 <- try(rwl_to_dt(read.tucson2(s, verbose = FALSE)),
#                silent = TRUE)
#    foo2 <- try(rwl_to_dt(read.tucson(s)),
#                silent = TRUE)
#    if (!inherits(foo2, "try-error") && !inherits(foo1, "try-error")) {
#       all.equal(foo1[order(core, year)], foo2[order(core, year)])
#    } else FALSE
# }
# 
# mismatch <- which(checks != '1' & checks != 'TRUE')
out <- data.table(
   crn = names(fnameUsed[mismatch]),
   fname = fnameUsed[mismatch],
   diff = checks[mismatch])
fwrite(out, 'QA_stuff/mismatches.csv')

fnameMismatch <- fread('QA_Stuff/mismatches.csv', header = TRUE)[, fname]
file <- fnameMismatch[129]
file

foo1 <- read.tucson2(file, verbose = TRUE) |> rwl_to_dt()
foo2 <- read.tucson(file) |> rwl_to_dt()
# foo2 <- read.tucson(file, long = TRUE) |> rwl_to_dt()
setkey(foo1, core, year)
setkey(foo2, core, year)

foo1
foo2

merged <- merge(foo1, foo2, by = c('core', 'year'))
merged[abs(rw.x - rw.y) > 1e-4]

foo2[!foo1]
foo1[!foo2]

all.equal(foo1, foo2)
 file

