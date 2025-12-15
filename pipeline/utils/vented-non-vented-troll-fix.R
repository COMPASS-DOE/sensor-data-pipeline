# Correct the unvented Aquatroll600 data in TEMPEST
# See https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/342
# and https://github.com/COMPASS-DOE/sensor-data-pipeline/issues/369
# BBL July and December 2025

# This script assumes that the working directory is the "L1/" folder
# of the COMPASS-FME L1 (Level 1) environmental sensor data

EDITED <- "~/Desktop/edited/"

# I downloaded ERA5 surface pressure data, all days, all months,
# 2019-2024, for this sub-region:
# North: 38.88°, West: -76.56°, South: 38.86°, East: -76.54°
# from "ERA5 hourly data on single levels from 1940 to present"
# The resulting GRIB file was reshaped to a CSV file by this code:
# x <- terra::rast("~/Downloads/722bc89876a2bfc18b3cd1156c8f8247.grib")
# y <- terra::extract(x, data.frame(lon = -76.5516, lat = 38.87403))
# z <- tidyr::pivot_longer(y, -ID)
# z$TIMESTAMP <- seq.POSIXt(from = ymd_hms("2019-01-01 00:00:00"), by = "1 hour", length.out = nrow(z))
# z$ID <- z$name <- NULL
pressure_data <- read_csv("~/Code/data-workflows/pipeline/utils/era5_pressure_tempest.csv")
pressure_data$atmosphere_psi <- pressure_data$value * 0.00014504 # Pa to PSI
pressure_data$value <- NULL

library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
theme_set(theme_bw())

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

    dat$ts <- round(ymd_hms(dat$TIMESTAMP), "hours")
    all_data[[f]] <- dat[c("TIMESTAMP", "Pressure600")]
    all_data[[f]]$corrected <- FALSE

    dat$Pressure600_mbar <- dat$Pressure600 * 68.948
    oob_entries <- dat$Pressure600_mbar > 900
    oob_entries[is.na(oob_entries)] <- FALSE

    if(any(oob_entries, na.rm = TRUE)) {
        message("\t", sum(oob_entries, na.rm = TRUE), " Pressure600 entries need fixing")

        dat_new <- left_join(dat, pressure_data,
                             by = c("ts" = "TIMESTAMP"),
                             # some data files have duplicated rows, so many to one
                             relationship = "many-to-one")
        if(all(is.na(dat_new$atmosphere_psi))) {
            message("\tNo atmospheric pressure data available; moving on")
            next
        }

        # Fix the bad Pressure600 entries by subtracting ERA5 atmospheric pressure
        dat_new$Pressure600[oob_entries] <- dat_new$Pressure600[oob_entries] -
            dat_new$atmosphere_psi[oob_entries]

        p <- ggplot(dat_new, aes(ts, Pressure600)) +
            geom_line(na.rm = TRUE) +
            geom_line(aes(y = atmosphere_psi), color = "blue", na.rm = TRUE) +
            ggtitle(label = bnf, subtitle = "Black is corrected troll pressure; blue is ERA5 atmosphere")
        print(p)
        # Remove extra columns we added and save out new file
        dat_new$ts <- dat_new$atmosphere_psi <- NULL
        all_data[[f]] <- dat_new[c("TIMESTAMP", "Pressure600")]
        all_data[[f]]$corrected <- oob_entries

        #stop()

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

all_data %>%
    bind_rows() %>%
    mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) ->
    all_data_df
p <- ggplot(all_data_df, aes(TIMESTAMP, Pressure600, color = corrected)) +
    geom_point(na.rm = TRUE)
print(p)


message("All done! Put those files into ./data/Raw/Raw_edited")
