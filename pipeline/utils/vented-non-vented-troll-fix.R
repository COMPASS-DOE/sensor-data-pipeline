# Correct the unvented Aquatroll600 data in TEMPEST
# See https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/342
# BBL July 2025

# This script assumes that the working directory is the "L1/" folder
# of the COMPASS-FME L1 (Level 1) environmental sensor data

EDITED <- "~/Desktop/edited/"

# Weather station pressure data
wx_files <- list.files("./",
                       pattern = "GCW_W_.*wx-barpress15.*csv$",
                       recursive = TRUE, full.names = TRUE)

library(dplyr)
library(lubridate)
library(readr)
lapply(wx_files, read_csv) %>%
    bind_rows() %>%
    select(TIMESTAMP, atm_pressure = Value) %>%
    distinct() ->
    wxdat

# Raw troll gw-pressure is psi; L1 wx-barpress15 is mbar
# psi to mbar conversion = x * 68.948

# Read in raw troll files, check for bad values, fix, write out

troll_files <- list.files("~/Dropbox/TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive/",
                          pattern = "_WaterLevel600", full.names = TRUE)

smry <- list()
all_data <- list()
for(f in troll_files) {
    #f <- troll_files[100]
    bnf <- basename(f)
    message(bnf)
    x <- read_lines(f)

    # Read the lines of data, forcing everything to be character except
    # the Pressure600 column that we want to fix
    dat <- read_csv(I(x[c(-1, -3, -4)]),
                    col_types = cols(Pressure600 = col_double(),
                                     .default = col_character()))

    dat$Pressure600_mbar <- dat$Pressure600 * 68.948
    oob_entries <- dat$Pressure600_mbar > 900
    oob_entries[is.na(oob_entries)] <- FALSE

    if(any(oob_entries, na.rm = TRUE)) {
        message("\t", sum(oob_entries, na.rm = TRUE), " Pressure600 entries need fixing")
        dat$ts <- ymd_hms(dat$TIMESTAMP)
        dat_new <- left_join(dat, wxdat,
                             by = c("ts" = "TIMESTAMP"),
                             # some data files have duplicated rows, so many to one
                             relationship = "many-to-one")
        if(all(is.na(dat_new$atm_pressure))) {
            message("\tNo atmospheric pressure data available; moving on")
            next
        }
        # Fix the bad Pressure600 entries
        dat_new$Pressure600[oob_entries] <- (dat_new$Pressure600_mbar[oob_entries] -
                                                 dat_new$atm_pressure[oob_entries]) / 68.948

        p <- ggplot(dat_new, aes(ts, Pressure600_mbar)) +
            geom_line(na.rm = TRUE) +
            geom_line(aes(y = atm_pressure), color = "blue", na.rm = TRUE) +
            ggtitle(label = bnf, subtitle = "Blue is atmosphere")
        # Remove extra columns we added and save out new file
        dat_new$Pressure600_mbar <- dat_new$ts <- dat_new$atm_pressure <- NULL

        # Write out data, read, add Campbell header lines back
        tf <- tempfile()
        write_csv(dat_new, tf, na = "", quote = "needed")
        x_out <- read_lines(tf)
        message("\tRead back ", length(x_out), " lines")
        x_out <- c(x[1:4], x_out[-1])

        # Write to a modified version of the original file
        message("\tWriting ", file.path(EDITED, bnf))
        write_lines(x_out, file.path(EDITED, bnf))
        bnp <- gsub(".dat", ".png", bnf)
        ggsave(file.path(EDITED, bnp), plot = p, width = 8, height = 6)
    } else {
        message("\tNothing needs fixing in this file")
    }
}

message("All done! Put those files into ./data/Raw/Raw_edited")
