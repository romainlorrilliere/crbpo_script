

library(data.table)
library(openxlsx)
library(lubridate)

file_OD <- "Bague_Espece_OD.xlsx"

file <- paste0("data/",nom,".xlsx")
d <- read.xlsx(file)

file_RP <- "TAILLE_BAGUE_RP.xlsx"
