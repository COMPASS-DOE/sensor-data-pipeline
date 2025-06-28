# fme-oos.R

# Need to run this script from within pipeline directory
if(basename(getwd()) != "pipeline") {
    stop("Working directory needs to be pipeline/")
}

library(lubridate)
library(readr)
library(tibble)

# We read in a version of the Aquatroll Calibration/Removal Log
# (in Monitoring Documents on the COMPASS Google Drive) and restructure it
# into a form ready for out-of-service calculations in L1_normalize.qmd
prep_troll_oos_table <- function(troll) {
    # If no time_pulled given, assume 6 AM
    tp <- troll$Time_pulled
    tp[is.na(tp) | tp == ""] <- "06:00"

    # If no time_replaced given, assume 6 PM
    tr <- troll$Time_replaced
    tr[is.na(tr) | tr == ""] <- "18:00"

    # If no date_replaced given, assume ongoing
    dr <- troll$Date_replaced
    dr[is.na(dr) | dr == ""] <- "12/31/2999"

    # Calculate out of service windows
    troll$oos_begin <- mdy(troll$Date_pulled, tz = "EST") + hm(tp)
    troll$oos_end <- mdy(dr, tz = "EST") + hm(tr)
    # Per Peter R., we throw out all data for 24 hours after replacement
    troll$oos_end <- troll$oos_end + 60 * 60 * 24

    # The L1 data have character, not PosixCT, timestamps, so for correct
    # comparisons in oos() below need to have that be true here too
    troll$oos_begin <- as.character(troll$oos_begin)
    troll$oos_end <- as.character(troll$oos_end)

    # Rename columns to match the design table entries
    troll$Instrument_ID <- troll$Troll
    troll$Plot <- troll$Location

    # Return a data frame with the needed columns: the oos begin and end,
    # as well as the additional columns to match
    troll[c("Site", "Plot", "Instrument_ID", "oos_begin", "oos_end")]
}

# Restructure the EXO calibration/deployment log
# (https://docs.google.com/spreadsheets/d/1y08XAOzzpLZbIOMJquVUv-jSHXhHJpF9neG5kcJpnjQ/edit#gid=913708372)
prep_exo_oos_table <- function(exo) {
    # Out of service until deployment
    exo$oos_begin <-ymd(paste0(exo$year, "-01-01"), tz = "EST")
    exo$oos_end <- force_tz(exo$deployed + hm("21:00"), tzone = "EST")

    # Out of service after retrieval
    exo2 <- tibble(
        Site = exo$Site,
        oos_begin = force_tz(exo$retrieved + hm("03:00"), tzone = "EST"),
        oos_end = ymd(paste0(exo$year, "-12-31"), tz = "EST")
    )

    # And, out of service when out of water
    exo3 <- tibble(
        Site = exo$Site,
        oos_begin = force_tz(exo$out_of_water + hm("03:00"), tzone = "EST"),
        oos_end = force_tz(exo$out_of_water + hm("21:00"), tzone = "EST")
    )

    exo <- exo[c("Site", "oos_begin", "oos_end")]
    exo <- rbind(exo, exo2, exo3)

    # The L1 data have character, not PosixCT, timestamps, so for correct
    # comparisons in oos() below need to have that be true here too
    exo$oos_begin <- as.character(exo$oos_begin)
    exo$oos_end <- as.character(exo$oos_end)

    exo[!is.na(exo$oos_begin),]
}


# Read the Aquatroll out-of-service table
troll <- read_csv("out-of-service/compass-fme-oos/troll_maintenance.csv",
                  col_types = "ccccccccc")
oos_troll <- prep_troll_oos_table(troll)
# This tibble should be written as "WaterLevel600.csv"

# Read the EXO out-of-service table
exo <- read_csv("out-of-service/compass-fme-oos/exo_log.csv",
                col_types = "cdDDDDcc")
oos_exo <- prep_exo_oos_table(exo)
# This tibble should be written as "ExoTable.csv"
