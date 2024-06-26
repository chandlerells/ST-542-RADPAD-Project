library(tidyverse)
library(readxl)

radpad <- read_excel("RADPAD DATA Stats team 5_28_24_FINAL.xlsx", skip = 1)

long <- radpad |>
  select(-Patient...20,
         -Breed,
         -`Sex (FI, FS, MI, MN)`) |>
  mutate(ID = 1:200) |>
  mutate(Weight = `Weight (kg)`) |>
  mutate(Procedure = as.factor(`Procedure  type (BPV, PDA PMI, PV Stent)`)) |>
  mutate(Time = `Total fluoro time (min)`) |>
  mutate(RADPAD = as.factor(`RADPAD Drape Used? Y/N`)) |>
  mutate(Patient_DAP = `DAP Total (Gycm2)`) |>
  select(-`Weight (kg)`,
         -`Procedure  type (BPV, PDA PMI, PV Stent)`,
         -`Total fluoro time (min)`,
         -`RADPAD Drape Used? Y/N`,
         -`DAP Total (Gycm2)`)|>
  mutate(`Resident 1` = as.character(`Resident 1`)) |>
  pivot_longer(Faculty:Anesthesia,
               names_to = "Occupation",
               values_to = "Dose") |>
  mutate(across(where(is.character), ~na_if(., "n/a"))) |>
  mutate(across(where(is.character), ~na_if(., "n /a"))) |>
  mutate(across(where(is.character), ~na_if(., "NB"))) |>
  drop_na() |>
  mutate(Dose = as.numeric(Dose)) |>
  mutate(ID = as.factor(ID)) |>
  mutate(Occupation = as.factor(Occupation)) |>
  mutate(Procedure = replace(Procedure, Procedure == "PV stent", "PV Stent"))

write_csv(long, "radpad.csv")




