# PTR_AQ600_2023_salinity_fix.R
# There was a calibration problem with PTR AquaTROLLs in 2023
# See https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/359
# This script fixes the problem, writing out corrected files
# BBL November 2025

# Note: this script should NOT have to be run in the future!
# It's committed to the repo as documentation of what was done,
# and how, and in case useful as a model in future work

library(readr)

# These times are when the wx_par_tot15 problem was fixed,
# via visual inspection of the L1 data
library(lubridate)
fix_ts_range <- ymd_hms(c("2023-08-01 00:00:00",
                          "2023-12-04 23:59:59"),
                        tz = "UTC")
# We use UTC above because that's what readr::read_csv assumes

# Where to put the edited files
EDITED <- "pipeline/data/Raw/Raw_edited/"

# Files to fix are assumed to be in the repo's data/Raw folder
files <- list.files("pipeline/data/Raw/Raw_original/",
                    pattern = "^Compass_PTR_[A-Z]{1,2}_31[1-3]_WaterLevel600[A-C]_2023[0-9]*\\.dat$",
                    full.names = TRUE)

for(f in files) {

    message("Reading ", f)
    x <- read_lines(f)

    # Parse the data, forcing everything to be character except TIMESTAMP
    dat <- read_csv(I(x[c(-1, -3, -4)]),
                    col_types = cols(
                        TIMESTAMP = col_datetime(), .default = col_character()))

    fixrows <- dat$TIMESTAMP >= fix_ts_range[1] & dat$TIMESTAMP <= fix_ts_range[2]

    n_fixrows <- sum(fixrows, na.rm = TRUE)

    message("Rows = ", nrow(dat))
    message("fixrows = ", n_fixrows)

    if(n_fixrows == 0) next  # nothing to do

    # Fix
    message("Fixing...")
    # Because the columns are named Salinity600A, Salinity600B, etc.,
    # i.e. have the troll ID appended to them (NOT a great design decision),
    # we use a regular expression to find the columns we want
    dat[fixrows, grep("Salinity600", colnames(dat))] <- ""
    dat[fixrows, grep("Actual_Conductivity600", colnames(dat))] <- ""
    dat[fixrows, grep("Specific_Conductivity600", colnames(dat))] <- ""
    dat[fixrows, grep("Water_Density600", colnames(dat))] <- ""
    dat[fixrows, grep("TDS600", colnames(dat))] <- ""

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
