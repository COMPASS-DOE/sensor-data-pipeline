# out-of-service

This folder holds COMPASS-FME instrument maintenance records that are
read by `L1_normalize.qmd` and used to add out-of-service flags to data.

In both cases,
- copy new rows from google sheet into the CSV
- check times -- technicians sometime use 12hr times, sometimes 24hr

`troll_maintenance.csv` tracks the "Aquatroll Calibration/Removal Log"
spreadsheet on the COMPASS-FME Google Drive.

`exo_log.csv` tracks the "EXO calibration/deployment log" sheet on the
COMPASS-FME Google Drive.
