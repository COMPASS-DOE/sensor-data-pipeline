
# Quick script to edit design table
# Stephanie Pennington | Created 07 Jan 2025

pacman::p_load(dplyr, tidyr, readr)

read_csv("pipeline/metadata/design_table.csv", col_types = "cccccccccDcc") %>%
    mutate(Site = case_when(Logger == "Compass_GWI_W_411" ~ "GWI",
                            .default = Site)) -> design_table

write_csv(design_table, "pipeline/metadata/design_table.csv", na = "")
