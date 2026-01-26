# TMP_AQ600_columnswap_fix.R
# Some of the AquaTROLL600s at TEMPEST got mis-configured; see
# https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/245
# This script fixes the problem by swapping columns in bad files
# and writing out corrected files
# BBL April 2025

# Note: this script should NOT have to be run in the future!
# It's committed to the repo as documentation of what was done,
# and how, and in case useful as a model in future work

library(readr)

# This map file tells us how to move data between columns in bad files
fixmap <- read_csv("utils/TMP_AQ600_bad_column_mapping.csv", col_types = "ccc")

# Helper function to swap data between columns
swapcols <- function(x, fixrows, fixmap) {
    # There needs to be a 1-to-1 mapping
    stopifnot(identical(sort(fixmap$Column), sort(fixmap$Should_be_mapped_to)))
    # fixrows has to be same length as data frame
    stopifnot(length(fixrows) == nrow(x))

    newx <- x
    n_swap <- 0
    for(i in seq_len(nrow(fixmap))) {
        src <- fixmap$Column[i]
        dest <- fixmap$Should_be_mapped_to[i]
        if(src != dest) {
            newx[fixrows, dest] <- x[fixrows, src]
            n_swap <- n_swap + 1
        }
    }
    message("\tMoved ", n_swap, " columns")
    invisible(newx)
}
# Example
message("TESTING...")
test <- as.data.frame(matrix(ncol = nrow(fixmap), data = seq_len(nrow(fixmap))))
colnames(test) <- fixmap$Column
test <- rbind(test, test)
swapcols(test, c(TRUE, FALSE), fixmap)

# Prefix of files to fix -- files with potentially swapped columns
file_prefix <- "PNNL_[0-9]{2}_WaterLevel600"

# Where to put the edited files
EDITED <- "data/Raw/Raw_edited/"

# Files to fix are assumed to be in the repo's data/Raw folder
files <- list.files("data/Raw/Raw_original/",
                    pattern = file_prefix, full.names = TRUE)

fixed_data <- list()

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
    fixrows <- with(dat, !is.na(Water_Density600) &
                        (Water_Density600 < 0.9 | Water_Density600 > 1.1))

    n_fixrows <- sum(fixrows, na.rm = TRUE)

    # Convert to character so everything swaps cleanly
    dat$Water_Density600 <- as.character(dat$Water_Density600)

    message("\tRows = ", nrow(dat))
    message("\tfixrows = ", n_fixrows)

    if(n_fixrows == 0) {
        fixed_data[[f]] <- dat
        fixed_data[[f]]$fixed <- FALSE
        next  # nothing to do
    }

    # Fix
    message("\tFixing...")
    fixed_data[[f]] <- swapcols(dat, fixrows, fixmap)

    # Write out data, read, add Campbell header lines back
    tf <- tempfile()
    write_csv(fixed_data[[f]], tf)
    x_out <- read_lines(tf)
    file.remove(tf)
    x_out <- c(x[1:4], x_out[-1])

    fixed_data[[f]]$fixed <- fixrows

    # Write to a modified version of the original file
    bn <- basename(f)
    message("Writing ", bn)

    out_fn <- file.path(EDITED, bn)
    if(file.exists(out_fn)) {
        warning("This file already exists in ", EDITED, "!")
    }
    write_lines(x_out, out_fn)
    # Confirm that our final file can be read cleanly
    # read_datalogger_file() is in L0_utils.R
    tryCatch(read_datalogger_file(out_fn),
             warning = function(e) { stop("Generated a read warning")})
} # for

message("All done with fixes")

# Plot the data for QA/QC checking

all_data <- dplyr::bind_rows(fixed_data)
all_data$TIMESTAMP <- lubridate::ymd_hms(all_data$TIMESTAMP)
library(ggplot2)
theme_set(theme_bw())


all_data$TDS600 <- as.numeric(all_data$TDS600)
all_data$TDS600[all_data$TDS600 == -99999.00] <- NA
p <- ggplot(all_data, aes(TIMESTAMP, TDS600, color = fixed)) +
    geom_point() + facet_grid(Statname ~ ., scales = "free")
print(p)
ggsave("~/Desktop/p1-TDS600.png", width = 10, height = 6)
print(p %+% all_data[all_data$TDS600 < 1,])
ggsave("~/Desktop/p2-TDS600.png", width = 10, height = 6)

all_data$Water_Density600 <- as.numeric(all_data$Water_Density600)
all_data$Water_Density600[all_data$Water_Density600 == -99999.00] <- NA
p <- ggplot(all_data, aes(TIMESTAMP, Water_Density600, color = fixed)) +
    geom_point() + facet_grid(Statname ~ ., scales = "free")
print(p)
ggsave("~/Desktop/p3-Water_Density600.png", width = 10, height = 6)

all_data$Resistivity600 <- as.numeric(all_data$Resistivity600)
all_data$Resistivity600[all_data$Resistivity600 == -99999.00] <- NA
p <- ggplot(all_data, aes(TIMESTAMP, Resistivity600, color = fixed)) +
    geom_point() + facet_grid(Statname ~ ., scales = "free")
print(p)
ggsave("~/Desktop/p4-Resistivity600.png", width = 10, height = 6)

# A non-swapped column
all_data$Temperature600 <- as.numeric(all_data$Temperature600)
all_data$Temperature600[all_data$Temperature600 == -99999.00] <- NA
p <- ggplot(all_data, aes(TIMESTAMP, Temperature600, color = fixed)) +
    geom_point() + facet_grid(Statname ~ ., scales = "free")
print(p)
ggsave("~/Desktop/p5-Temperature600.png", width = 10, height = 6)


all_data$pH600 <- as.numeric(all_data$pH600)
all_data$pH600[all_data$pH600 == -99999.00] <- NA
p <- ggplot(all_data, aes(TIMESTAMP, pH600, color = fixed)) +
    geom_point() + facet_grid(Statname ~ ., scales = "free")
print(p)
ggsave("~/Desktop/p6-pH600.png", width = 10, height = 6)


