library(dplyr)

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))  # Carga el objeto elsoc_long_2016_2023

elsoc_2023 <- elsoc_long_2016_2023 |>
  filter(ola == 7)

save(elsoc_2023, file="elsoc2023.rdata")
