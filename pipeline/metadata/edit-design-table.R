
# Quick script to edit design table
# Stephanie Pennington | Created 07 Jan 2025

pacman::p_load(dplyr, tidyr, readr)

read_csv("pipeline/metadata/design_table.csv", na = "") %>%
    mutate(Plot = case_when(Table == "ExoTable" ~ "OW",
                            .default = Plot)) -> design_table

write_csv(design_table, "pipeline/metadata/design_table.csv", na = "")
