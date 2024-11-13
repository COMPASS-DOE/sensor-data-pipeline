# flmd-generator.R
# Generate file-level metadata for an L1 release
# https://github.com/ess-dive-community/essdive-file-level-metadata/
# BBL August 2024

library(tidyr)
library(dplyr)

# Need to run this script from within pipeline directory
if(basename(getwd()) != "pipeline") {
    stop("Working directory needs to be pipeline/")
}

# Get all the files and fill in path and name data
library(tibble)
results <- tibble(file = list.files(path = "./data/L1/",
                                    recursive = TRUE,
                                    full.names = TRUE),
                  sort = seq_along(file),
                  File_Path = gsub("^\\./data/L1//", "", dirname(file)),
                  File_Name = basename(file)
)
results$file <- NULL
message("Found ", nrow(results), " files to process")

# Isolate the data files - pattern of xxx_xx_nnnnnnnn-nnnnnnnn_*.csv
message("Processing data files...")
data_files <- grep("^[A-Za-z]+_[A-Za-z]+_[0-9]{8}-[0-9]{8}.+\\.csv$", results$File_Name)
df <- results[data_files,]

find_start_end_dates <- function(x) {
    x <- separate(x, File_Name, sep = "_", into = c("Site", "Plot", "Timerange","Level","version"), remove = FALSE)
    x <- separate(x, Timerange, sep = "-", into = c("Start_Date", "End_Date"))
    x$Start_Date <- as.Date(x$Start_Date, format = "%Y%m%d")
    x$End_Date <- as.Date(x$End_Date, format = "%Y%m%d")
    return(x)
}
df <- find_start_end_dates(df)
df$File_Description <- paste(format(df$Start_Date, "%b %Y"),
                             "sensor data for",
                             df$Plot,
                             "plot at",
                             df$Site,
                             "site")
df$Missing_Value_Codes <- "'NA'"

# Isolate the plot files
message("Processing plot files...")
plot_files <- grep("^[A-Za-z]+_[A-Za-z]+_[0-9]{8}-[0-9]{8}.+\\.pdf$", results$File_Name)
pf <- results[plot_files,]
pf <- find_start_end_dates(pf)
pf$File_Description <- paste("Plots of",
                             format(pf$Start_Date, "%b %Y"),
                             "sensor data for",
                             paste0(pf$Site, "-", pf$Plot))

# Isolate the site-year metadata files
message("Processing metadata files...")
metadata_files <- grep("metadata.txt$", results$File_Name)
mdf <- results[metadata_files,]
mdf$File_Description <- paste("Metadata for all data files in", mdf$File_Path, "folder")

# Isolate the special files (currently, sample R scripts)
message("Processing special files...")
special_files_info <-
    tribble(~File_Name,                  ~File_Description,
            "README_v1-1.txt",           "Overall documentation file for the v1-1 release",
            "README.md",                 "Minimal README about the folder",
            "create-time-series.R",      "Sample R code to create a time series from data",
            "cumulative-observations.R", "Sample R code to plot cumulative observations")
special_files <- which(results$File_Name %in% special_files_info$File_Name)
sf <- results[special_files,]
sf <- left_join(sf, special_files_info, by = "File_Name")

# Other files
message("Checking other files...")
other_files <- results[-c(data_files, plot_files, metadata_files, special_files),]
if(nrow(other_files) > 0) {
    print(other_files)
    stop("There are 'other' files, i.e. with no description. You may need to ",
    "update the 'special_files_info' in the script if this is a new data release")
}

bind_rows(df, pf, mdf, sf) %>%
    arrange(sort) %>%
    mutate(Standard = "") %>%
    select(File_Name, File_Description, Standard, Start_Date,
           End_Date, Missing_Value_Codes, File_Path) ->
    flmd

readr::write_csv(flmd, "flmd.csv", na = "")

message("All done!")
