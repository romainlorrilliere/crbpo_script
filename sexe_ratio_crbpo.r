library(data.table)
library(stringr)


dnom <- fread("data/nat2021.csv")

setnames(dnom,"preusuel","prenom")
dnom <- dnom[,.(sexe_cat = mean(sexe)),by = .(prenom)]
dnom[,sexe_cat := ifelse(sexe_cat == 1.5 , "mixte",ifelse(sexe_cat < 1.5, "homme","femme"))]
print(dnom[,(.N),by = sexe_cat])

dbag <- fread("data/liste_bagueur.csv",encoding = "Latin-1")
head(dbag)
setnames(dbag,colnames(dbag),gsub(" ","_",colnames(dbag)))
dbag <- dbag[,.(NOM,Status_bagueur)]
dbag[,NOM := toupper(iconv(NOM, to='ASCII//TRANSLIT'))]
dbag[,prenom := sapply(strsplit(sapply(strsplit(NOM, ", "), tail, 1)," "),head,1)]

head(dbag)

dbag <- merge(dbag,dnom,by="prenom",all.x = TRUE)
dbag[is.na(sexe_cat), sexe_cat := "inconnu"]

dbag_unique <- dbag[,.(nb=.N),by=.(prenom,sexe_cat)]

print(dbag_unique[,.(nombre = .N),by=sexe_cat])
cat(dbag_unique[sexe_cat == "inconnu",prenom])

sum_bag_nom <- dbag[,.(nombre = .N),by = .(Status_bagueur,sexe_cat)]
sum_bag_nom_w <- dcast(sum_bag_nom,Status_bagueur ~ sexe_cat)
sum_bag_nom_w[is.na(sum_bag_nom_w)] <- 0

fwrite(sum_bag_nom_w,"sexe_ratio_crbpo.csv")
