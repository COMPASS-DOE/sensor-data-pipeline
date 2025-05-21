# Create a time series of a single variable at a site
# This script assumes that the working directory is the "L1/" folder
# of the COMPASS-FME L1 (Level 1) environmental sensor data
# BBL May 2025

SITE <- "CRC" # change this to e.g. "CRC_W" if you want a particular plot
VARIABLE <- "soil_temp_10cm"

# Construct a "regular expression" to find the files we want: in this case,
# CSV files starting with the site code above
regex <- paste0("^", SITE, ".*csv$")

# Get the names of the files we need. Note this assumes that your
# working directory is the main directory of the L1 data
files <- list.files("./", pattern = regex, recursive = TRUE, full.names = TRUE)

# Helper function to read the files and filter to just the variable we want
# Use readr::read_csv for easy timestamp handling
# Note that we set "col_types" to force the "Plot" column to be read as a
# character; see https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/186
library(readr)
f <- function(f) {
    message("Reading ", basename(f))
    x <- read_csv(f, col_types = "ccTccccdccii")
    x[x$research_name == VARIABLE,]
}

# Read the files
dat <- lapply(files, f)
dat <- do.call("rbind", dat)

# Plot the data
library(ggplot2)
p <- ggplot(dat, aes(TIMESTAMP, Value, color = Sensor_ID)) +
    geom_line() +
    facet_grid(Plot~.) +
    ggtitle(paste(SITE, VARIABLE, paste0("(n=", nrow(dat), ")")))
print(p)

# All done!
