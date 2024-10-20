# memory-profile.R
# hacky profile of the CPU and memory needed by the processing pipeline
# BBL 10/2024

library(ggplot2)
theme_set(theme_bw())

library(readr)

# Step 1: run `top -i 5 >parallel.txt`, run pipeline, stop with ctrl-C

# Step 2: process resulting file (this script)

x <- readLines("~/Desktop/parallel.txt")

Processes <- grep("^Processes:", x)

results_list <- list()
for(i in seq_along(Processes)) {
    start <- Processes[i]
    if(i < length(Processes)) {
        nextstart <- Processes[i+1]
    } else {
        nextstart <- length(Processes) + 1
    }

    tablestart <- start + 11
    tableend <- nextstart - 1

    dat <- read_table(I(x[tablestart:tableend]), col_types = "cccccccc")
    dat$TIMESTAMP <- lubridate::ymd_hms(x[Processes[i]+1])
    results_list[[i]] <- dat
}

library(dplyr)
library(tidyr)

results <- bind_rows(results_list) %>% filter(COMMAND == "R")

results %>%
    mutate(Seconds = as.numeric(TIMESTAMP - min(TIMESTAMP)),
           MEM = as.numeric(gsub("M.*$", "", MEM))) %>%
    group_by(Seconds) %>%
    summarise(MEM = sum(MEM), CPU = sum(as.numeric(`%CPU`)) / 100) ->
    out

out %>%
    filter(Seconds < 60) %>%
    pivot_longer(-Seconds) %>%
    ggplot(aes(Seconds, value)) +
    geom_point() +
    facet_grid(name~., scales = "free") +
    geom_line()

# L0 runs 0-10 seconds
# L1_normalize 15-20
# L1 20-60

