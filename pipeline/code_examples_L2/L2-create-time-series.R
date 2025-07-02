# Create a time series of a single variable at a site
# This script assumes that the working directory is the "L2/" folder
# of the COMPASS-FME Level 2 environmental sensor data
# BBL June 2025

SITE <- "CRC" # change this to e.g. "CRC_W" if you want a particular plot
#SITE <- "CRC_W" # example: a particular plot
#SITE <- "CRC_[A-Z]+_2023" # example: a particular year
#SITE <- "CRC_W_2023" # example: a particular plot AND year
VARIABLE <- "soil-temp-10cm"

# Construct a "regular expression" to find the files we want: in this case,
# CSV files starting with the site code above
regex <- paste0("^", SITE, ".*", VARIABLE, "_L2_v2.*parquet$")

# Get the names of the files we need. Note this assumes that your
# working directory is the main directory of the L1 data
files <- list.files("./", pattern = regex, recursive = TRUE, full.names = TRUE)

# L2 files are written in Parquet, a high performance, space
# efficient format for tabular data. These files can be read
# using R's `arrow` package (https://arrow.apache.org/docs/r/).
library(arrow)
dat <- lapply(files, function(f) {
    message("Reading ", basename(f))
    read_parquet(f)
})
# Wasn't that fast!
dat <- do.call("rbind", dat)

# Plot the data
library(ggplot2)
p <- ggplot(dat, aes(TIMESTAMP, Value, color = Sensor_ID)) +
    geom_line(na.rm = TRUE) +
    facet_grid(Plot~.) +
    ggtitle(paste(SITE, VARIABLE, paste0("(n=", nrow(dat), ")")))
print(p)

# All done!
