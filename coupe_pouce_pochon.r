
library(data.table)
library(ggplot2)
library(lubridate)
library(openxlsx)

setwd("crbpo/")


d_m <- fread("data/Extrait_alldata_SPOL_Mangeoire_20231121.txt")
setnames(d_m,colnames(d_m),gsub(" ","_",colnames(d_m)))
d_m <- d_m[THEME_SESSION %in% c("MANGEOIRE"),.(BAGUE,BAGUEUR,ACTION,DATE,THEME_SESSION)]
d_m[,date := dmy(DATE)]
d_m[,year := year(date)]
d_m[,month := month(date)]
d_m_22 <- d_m[year == 2022|(year == 2021 & month > 10),.(BAGUE,BAGUEUR,ACTION,DATE,THEME_SESSION,date,year)]
dim(d_m_22)




d_p <- fread("data/Extrait_pheno_202302.txt")
setnames(d_p,colnames(d_p),gsub(" ","_",colnames(d_p)))


d_p <- d_p[THEME_SESSION %in% c("PHENO","HALTE","SEJOUR"),.(BAGUE,BAGUEUR,ACTION,DATE,THEME_SESSION)]
d_p[,date := dmy(DATE)]
d_p[,year := year(date)]
d_p_22 <- d_p[year == 2022]
dim(d_p_22)





d_s <- fread("data/extrait_sejour_coup_pouce_2024-02-14_2.txt")
setnames(d_s,colnames(d_s),gsub(" ","_",colnames(d_s)))

d_s <- d_s[THEME_SESSION %in% c("PHENO","HALTE","SEJOUR"),.(BAGUE,BAGUEUR,ACTION,DATE,THEME_SESSION)]
d_s[,date := dmy(DATE)]
d_s[,year := year(date)]
d_s_22 <- d_s[year == 2022,]
dim(d_s_22)



d_22 <- rbind(rbind(d_m_22,d_p_22),d_s_22)

d_bj <- d_22[,.(nb = .N),by = .(date,BAGUEUR,THEME_SESSION)]
d2 <- d_bj[,.(median = round(median(nb))),by = .(BAGUEUR,THEME_SESSION)]
d1 <- dcast(d2,BAGUEUR ~THEME_SESSION)
d2[,isMax := median == max(median),by = BAGUEUR]
d2 <- d2[isMax == TRUE,]
d2[,nbTheme := .N, by= BAGUEUR]
d2[nbTheme > 1, THEME_SESSION := nbTheme]
d2 <- unique(d2[,.(BAGUEUR,median,THEME_SESSION)])





d <- merge(d2,d1,by="BAGUEUR")

setorder(d,-median)

d[,BAGUEUR := gsub(","," ",BAGUEUR)]
d <- d[BAGUEUR != "",]
head(d)


d[1:20,]



write.xlsx(d,"output/coup_pouce_2023.xlsx")









