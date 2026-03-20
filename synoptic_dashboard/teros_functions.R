

fileread <- function(data, total_files) {

  # If we're running in a Shiny session, update progress bar
  if(!is.null(getDefaultReactiveDomain())) {
    incProgress(1 / total_files)
  }

  # Download files to local (don't worry, we'll delete em later)
  drive_download(data, overwrite = T, path = "tempfile.dat")

  # Lines 1, 3, and 4 of the TEROS data files contain sensor metadata that we want to remove
  # Read the data files into a string vector, remove those lines, and then pass to read.csv()
  teros_primitive <- read_lines("tempfile.dat") #temp file goes here
  teros_primitive <- teros_primitive[-3:-4]

  unlink("tempfile.dat")

  #textConnection(teros_primitive) %>%
    read_csv(I(teros_primitive),
             skip = 1,
             #check.names = FALSE,
             na = "NAN",
             #na.strings = "NAN",
             #stringsAsFactors = FALSE,
             col_types = "Tddcddddddddddddddddddd") %>%
    as_tibble() %>%
    # Reshape the data frame to one observation per row
    gather(channel, value, -TIMESTAMP, -RECORD, -Statname) %>%
    filter(!is.na(value)) %>%
    # Pull data logger ID out of statname
    separate(Statname, into = c("Inst", "Logger"), sep = "_" ) %>%
    # Parse the data logger number, channel number, and variable number out of the
    # Statname and Channel columns
    mutate(Logger = as.integer(Logger, fixed = TRUE),
           TIMESTAMP = ymd_hms(TIMESTAMP, tz = "EST")) %>%
    select(-Inst) %>%  # unneeded
    # Next, parse channel into the data logger channel and variable number
    separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
    mutate(Data_Table_ID = as.integer(gsub("Teros(", "", Data_Table_ID, fixed = TRUE)),
           variable = as.integer(gsub(")", "", variable, fixed = TRUE)),
           # Give them sensible names
           variable = case_when(variable == 1 ~ "VWC",
                                variable == 2 ~ "TSOIL",
                                variable == 3 ~ "EC"))
}


process_teros <- function() {

  # Create a list of files
  cat("Accessing drive..")
  teros_files <- gdrive_files %>%
    filter(grepl("TerosTable", name))

  #NEED TO ADD INVENTORY LATER

teros_files$name %>%
    map(fileread, nrow(teros_files)) %>%
    bind_rows()

  # teros_primitive %>%
  #   left_join(teros_inventory, by = c("Logger" = "Data Logger ID",
  #                                     "Data_Table_ID" = "Terosdata table channel")) %>%
  #   select(- `Date of Last Field Check`) %>%
  #   rename("Active_Date" = "Date Online (2020)",
  #          "Grid_Square" = "Grid Square") ->
  #   teros
  #
  # nomatch <- anti_join(teros, teros_inventory, by = c("Logger" = "Data Logger ID",
  #                                                     "Data_Table_ID" = "Terosdata table channel"))
  # if(nrow(nomatch) > 0) {
  #   warning("There were logger/channel combinations that I couldn't find in teros_inventory.csv:")
  #   nomatch %>%
  #     distinct(Logger, Data_Table_ID) %>%
  #     kable()
  # }

}
