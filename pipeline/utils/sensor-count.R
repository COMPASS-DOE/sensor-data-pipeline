# Count dataloggers and sensors

library(readr)
library(dplyr)

loc <- here::here("pipeline")
dt <- read_csv(file.path(loc, "metadata", "design_table.csv"))
dt %>%
    filter(!is.na(research_name)) ->
    dt

message("Loggers = ", length(unique(dt$Logger)))
message("Variable types = ", length(unique(dt$research_name)))
message("Sensors = ", nrow(dt))
