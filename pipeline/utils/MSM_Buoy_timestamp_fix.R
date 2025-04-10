# MSM_Buoy_timestamp_fix.R
# The MSM buoy was accidentally set to EDT until 2025-03-31 05:58 EDT
# See https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/236
# This script fixes the problem, writing out corrected files
# BBL April 2025

# Note: this script should NOT have to be run in the future!
# It's committed to the repo as documentation of what was done,
# and how, and in case useful as a model in future work

library(readr)

# Prefix of files to fix -- files with potentially timestamps
file_prefix <- "Compass_MSM_Buoy"

# These times are when the wx_par_tot15 problem was fixed,
# via visual inspection of the L1 data
fix_timestamp <- "2025-03-31 05:58" # MSM

# Where to put the edited files
EDITED <- "pipeline/data/Raw/Raw_edited/"

# Files to fix are assumed to be in the repo's data/Raw folder
files <- list.files("pipeline/data/Raw/Raw_original/",
                    pattern = file_prefix, full.names = TRUE)

for(f in files) {

    message("Reading ", f)
    x <- read_lines(f)

    # Read the lines of data, forcing everything to be character except
    # the PAR_Tot_C_Tot columns that we want to fix
    dat <- read_csv(I(x[c(-1, -3, -4)]),
                    col_types = cols(
                        TIMESTAMP = col_datetime(), .default = col_character()))

    fixrows <- dat$TIMESTAMP < fix_timestamp
    n_fixrows <- sum(fixrows, na.rm = TRUE)

    message("Rows = ", nrow(dat))
    message("fixrows = ", n_fixrows)

    if(n_fixrows == 0) next  # nothing to do

    # Fix
    message("Fixing...")
    dat$TIMESTAMP[fixrows] <- dat$TIMESTAMP[fixrows] - 60*60
    dat$TIMESTAMP <- format(dat$TIMESTAMP, "%Y-%m-%d %H:%M:%S") # to ensure correct format written

    # Write out data, read, add Campbell header lines back
    tf <- tempfile()
    write_csv(dat, tf)
    x_out <- read_lines(tf)
    x_out <- c(x[1:4], x_out[-1])

    # Write to a modified version of the original file
    bn <- basename(f)
    message("Writing ", bn)
    write_lines(x_out, file.path(EDITED, bn))

}

message("All done")
