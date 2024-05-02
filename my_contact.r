library(data.table)
library(ggplot2)

dbag <- fread("data/liste_bagueur.csv",encoding = "Latin-1")
setnames(dbag,colnames(dbag),gsub(" ","_",colnames(dbag)))
head(dbag)


dbag <- dbag[,.(terrain,rencontre,NOM,Status_bagueur)]

dbag[is.na(terrain),terrain := 0]
dbag[is.na(rencontre),rencontre := 0]
dbag[terrain == 1, rencontre := 1]

dbag[,`:=`(generaliste_specialiste = ifelse (Status_bagueur == "Bagueur spécialiste","spécialiste",ifelse(Status_bagueur %in% c("Bagueur généraliste métro","Délégué régional"),"généraliste","")),delegue  = ifelse(Status_bagueur == "Délégué régional","délégué","non"))]

cat("\n")
head(dbag)
dbag1 <- dbag[,.(nombre_bagueur = .N),by = generaliste_specialiste]
dbag2 <- dbag[terrain== 1,.(nb_terrain = .N),by = generaliste_specialiste]
dbag3 <- dbag[rencontre == 1,.(nb_connaissance = .N),by = generaliste_specialiste]

dbag_sum <- merge(merge(dbag1,dbag2,by="generaliste_specialiste"),dbag3,by="generaliste_specialiste")
cat("\n")
dbag_sum


dbag_sum[,`:=`(terrain = round(nb_terrain/nombre_bagueur,3),connaissance = round(nb_connaissance/nombre_bagueur,2))]

cat("\n")
dbag_sum

fwrite(dbag_sum,"my_contact.csv")

dbag4 <- dbag[delegue == "délégué",.(nombre_delegue = .N),by = delegue]
dbag5 <- dbag[delegue == "délégué" & rencontre == 1 ,.(nb_connaissance = .N),by = delegue]
dbag_dele <- merge(dbag4,dbag5,by="delegue")
dbag_dele[,`:=`(proportion = nb_connaissance/nombre_delegue,generaliste_specialiste = "délégué",variable = "connaissance")]
dbag_dele <- dbag_dele[,.(generaliste_specialiste,variable,proportion)]
dbag_dele


ggbag_1 <- melt(dbag_sum[,.(generaliste_specialiste,terrain,connaissance)],id.vars = "generaliste_specialiste",value.name="proportion")
ggbag_1

ggbag_1 <- rbind(ggbag_1,dbag_dele)

fwrite(dbag_sum,"my_contact_2.csv")

gg <- ggplot(data= ggbag_1,aes(x=variable,y=proportion)) + geom_bar(stat = "identity") + facet_wrap(.~generaliste_specialiste)
gg <- gg + labs(x="Interactions",y="Proportion")
gg
ggsave("my_contacts.png",gg)
