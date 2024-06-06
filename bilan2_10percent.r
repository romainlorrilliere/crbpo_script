

library(data.table)
library(openxlsx)
library(lubridate)
### setwd("crbpo")
source("functions/fun_open_gestbag.r")

nom <- "extrait_STRALU"
file <- paste0("data/",nom,".xlsx")
d <- read.xlsx(file)

setDT(d)

head(d)
d[grep(" X ",Nom.vernaculaire),Nom.vernaculaire := ""]
d <- d[Nom.vernaculaire != "",]

dd <- d[,.(nb = sum(Nombre)),by=.(BAGUEUR,Nom.vernaculaire,Prive)]

dd[, tot := sum(nb),by=.(Nom.vernaculaire)]
dd[,prop := round(nb/tot,2)]

ddd <- dd[BAGUEUR != "" & Nom.vernaculaire != "" ,.(BAGUEUR, Nom.vernaculaire, prop,Prive)]
setnames(ddd,"prop","prop_baguage_control")

d_b <- d[ACTION == "B              ",]
dd1 <- d_b[,.(nb = sum(Nombre)),by=.(BAGUEUR,Nom.vernaculaire)]

dd1[, tot := sum(nb),by=.(Nom.vernaculaire)]
dd1[,prop_baguage := round(nb/tot,2)]

ddd2 <- dd[BAGUEUR != "" & Nom.vernaculaire != "" ,.(BAGUEUR, Nom.vernaculaire, prop)]
setnames(ddd2,"prop","prop_baguage")



ddd <- merge(ddd,ddd2,by=c("BAGUEUR","Nom.vernaculaire"),all = TRUE)
ddd[,prop_max := ifelse(prop_baguage_control>prop_baguage,prop_baguage_control,prop_baguage)]
setorder(ddd,-prop_max)

ddd


dbag <- read.xlsx("data/liste_bagueur.xlsx")
setDT(dbag)
dbag <- dbag[,.(NOM,mail)]
setnames(dbag,"NOM","BAGUEUR")

ddd <- merge(ddd,dbag,by="BAGUEUR")
setorder(ddd,BAGUEUR)
ddd

fwrite(ddd,"output/bilan_habitrack_euring_2024-03-11_10percent.csv")





# figure privé publique

vecsp <- "Pie-grièche grise"

dprive <- d[Nom.vernaculaire %in% vecsp,.(nb = sum(Nombre)),by=.(Prive,Annee,Nom.vernaculaire)]
dprive[,Annee := as.numeric(Annee)]
dprive[,Prive := as.logical(Prive)]

library(ggplot2)

gg <- ggplot(dprive,aes(x=Annee,y=nb,group=Prive,colour=Prive)) + facet_grid(Nom.vernaculaire~.,scales="free_y")
gg <- gg + geom_point()+geom_line()
gg <- gg  + labs(y = "Nombre de données",x= "Année",colour = "Données privées")
ggsave(paste0("output/fig_prive_public_",nom,".png"),gg)


dprive2 <- dprive[Prive == TRUE,.(firstY_prive = min(Annee)),by = .(Nom.vernaculaire)]
dprive <- merge(dprive,dprive2)
dprive[,avdebutPrive := Annee < firstY_prive]

sum_dprive <- dprive[,.(tot = sum(nb)),by = .(Nom.vernaculaire,avdebutPrive,Prive)]

sum_dprive







nom <- "extrait_STRALU_2024-04-05"
file <- paste0("data/",nom,".xlsx")
d <- read.xlsx(file)
file <- paste0("data/",nom,".csv")
d<-fread(file)

setDT(d)

head(d)
d[grep(" X ",Nom.vernaculaire),Nom.vernaculaire := ""]
d <- d[Nom.vernaculaire != "",]
d[,DATE := dmy(DATE)]
d[,year := year(DATE)]

dd <- d[AGE == "PUL" & ACTION == "B" & !is.na(BAGUEUR) & BAGUEUR != "UFCS" & BAGUEUR != "",]

dd[,BAGUEUR := gsub(","," ",BAGUEUR)]

ddd <- dd[,.(nb = .N),by = .(BAGUEUR,year)]
ddd[,tot_bagueur := sum(nb),by = BAGUEUR]
setorder(ddd,-tot_bagueur)


print(ddd)


fwrite(ddd,"output/baguage_pul_STRALU.csv")

d_j <- d[grep("JO",BAGUEUR),]
d_j

d_b <- d[grep("BOST",BAGUEUR),]
d_b


unique(ddd[tot_bagueur > 50,BAGUEUR])






## seabird bezil

name ="extrait_seabird_euring_2024-04-09"
file <- paste0("data/",name,".xlsx")

d <- f_open_extrait_xlsx(file,selected_col =c("ACTION","CENTRE","PAYS","Lieux","BAGUE","ESPECE","Nom_vernaculaire","BAGUEUR"))
bagues_BA <- d[grep("BRESIL",Lieux),BAGUE]

dd <- d[BAGUE %in% bagues_BA,]
dim(dd)

dd





## ALCTOR_URIAAL

name ="extrait_euring_ALCTOR_URIAAL"
file <- paste0("data/",name,".xlsx")

d <- f_open_extrait_xlsx(file,selected_col =c("ACTION","CENTRE","PAYS","Lieux","BAGUE","ESPECE","Nom_vernaculaire","BAGUEUR"))

d <- d[PAYS == "FR" & ACTION %in% c("C","R"),]
head(d)

dd <- d[,.(nb = .N) , by = BAGUEUR]
dd[, tot := sum(nb)]
dd[,prop := round(nb/tot,2)]
setorder(dd,-prop)
print(dd)


dd
