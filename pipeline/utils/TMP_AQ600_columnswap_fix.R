# TMP_AQ600_columnswap_fix.R
# Some of the AquaTROLL600s at TEMPEST got mis-configured; see
# https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/245
# This script fixes the problem, writing out corrected files
# BBL April 2025

# Note: this script should NOT have to be run in the future!
# It's committed to the repo as documentation of what was done,
# and how, and in case useful as a model in future work

library(readr)

fixmap <- read_csv("pipeline/utils/TMP_AQ600_bad_column_mapping.csv", col_types = "cc")

test <- as.data.frame(matrix(ncol = nrow(fixmap), data = seq_len(nrow(fixmap))))
colnames(test) <- fixmap$Column
swapcols <- function(x, fixmap) {
    newx <- x
    n_swap <- 0
    for(i in seq_len(nrow(fixmap))) {
        src <- fixmap$Column[i]
        dest <- fixmap$Should_be_mapped_to[i]
        if(src != dest) {
            newx[dest] <- x[src]
            n_swap <- n_swap + 1
        }
    }
    message("\tMoved ", n_swap, " columns")
    invisible(newx)
}
message("TESTING...")
swapcols(test, fixmap)


# Prefix of files to fix -- files with potentially swapped columns
file_prefix <- "PNNL_[0-9]{2}_WaterLevel600"

# Where to put the edited files
EDITED <- "pipeline/data/Raw/Raw_edited/"

# Files to fix are assumed to be in the repo's data/Raw folder
files <- list.files("pipeline/data/Raw/Raw_original/",
                    pattern = file_prefix, full.names = TRUE)

for(f in files) {

    message("Reading ", basename(f))
    x <- read_lines(f)

    # Read the lines of data, forcing everything to be character except
    # Water_Density600, which is our indicator column for a problem
    dat <- read_csv(I(x[c(-1, -3, -4)]),
                    na = "NAN",
                    col_types = cols(Water_Density600 = col_number(),
                                     .default = col_character()))

    # Do any Water_Density600 values fall out of expected range 0.9-1.1?
    fixrows <- dat$Water_Density600 < 0.9 | dat$Water_Density600 > 1.1

    n_fixrows <- sum(fixrows, na.rm = TRUE)

    # QUESTION: do we fix rows with Water_Density600 of NA?

    message("\tRows = ", nrow(dat))
    message("\tfixrows = ", n_fixrows)

    if(n_fixrows == 0) next  # nothing to do

    # Fix
    message("\tFixing...")
    newdat <- swapcols(dat, fixmap)

    # Write out data, read, add Campbell header lines back
    tf <- tempfile()
    write_csv(newdat, tf)
    x_out <- read_lines(tf)
    x_out <- c(x[1:4], x_out[-1])

    # Write to a modified version of the original file
    bn <- basename(f)
    message("Writing ", bn)
    write_lines(x_out, file.path(EDITED, bn))
}

message("All done")
