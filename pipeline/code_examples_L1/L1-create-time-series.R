# Create a time series of a single variable at a site
# This script assumes that the working directory is the "L1/" folder
# of the COMPASS-FME Level 1 environmental sensor data
# NOTE: this script is for working with the "v2" version of the data
# BBL June 2025

SITE <- "CRC" # change this to e.g. "CRC_W" if you want a particular plot
#SITE <- "CRC_W" # example: a particular plot
#SITE <- "CRC_[A-Z]+_2023" # example: a particular year
#SITE <- "CRC_W_2023" # example: a particular plot AND year
VARIABLE <- "soil-temp-10cm"

# Construct a "regular expression" to find the files we want: in this case,
# CSV files starting with the site code above
regex <- paste0("^", SITE, ".*", VARIABLE, "_L1_v2.*csv$")

# Get the names of the files we need. Note this assumes that your
# working directory is the main directory of the L1 data
files <- list.files("./", pattern = regex, recursive = TRUE, full.names = TRUE)

# Use readr::read_csv for easy timestamp handling
# Note that we set "col_types" to force the "Plot" column to be read as a
# character; see https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/186
library(readr)
dat <- lapply(files, function(f) {
    message("Reading ", basename(f))
    # NOTE this assumes you're reading a numeric variable, not a
    # time-of-min/max like wx-tempmax15, wx-bptmn24, etc.
    read_csv(f, col_types = "ccTccccdcclll")
})
dat <- do.call("rbind", dat)

# Plot the data
library(ggplot2)
p <- ggplot(dat, aes(TIMESTAMP, Value, color = Sensor_ID)) +
    geom_line() +
    facet_grid(Plot~.) +
    ggtitle(paste(SITE, VARIABLE, paste0("(n=", nrow(dat), ")")))
print(p)

# Plot the data but color by F_MAD, which is an outlier flag (see documentation)
p_mad <- ggplot(dat, aes(TIMESTAMP, Value, color = F_MAD, group = Sensor_ID)) +
    geom_line() +
    facet_grid(Plot~.)
print(p_mad)

# All done!
