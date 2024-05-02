require(ggplot2)
require(data.table)
require(xlsx)
require(readr)
require(lubridate)
require(stringdist)
require(stringr)
require(lares)


setwd("crbpo")

##importer les inscriptions


file <- "data_stage_theo/inscription_stage_theorique.xlsx"
dinscrit <- read.xlsx(file,1)
colnames(dinscrit)
setDT(dinscrit)
newcol <- c("id","date","user","exam","last_name","first_name","email","region","type","pp","identif","titre","cadre","convoc","attestation","destinataire","session","memo","raison","imperatif","confirmation","na","na1","na2","na3")
colnames(dinscrit) <- newcol
dinscrit[,type := ifelse (type == "Permis généraliste","generalist","specialist")]

dinscrit[, `:=`(last_name = toupper(last_name), first_name = str_to_title(first_name))]
dinscrit <- dinscrit[session == "202404",.(id,last_name,first_name,email,type,attestation)]
dinscrit[,`:=`(name = paste(last_name,first_name),name_up_1 = toupper(paste(last_name,first_name)),name_up_2 = toupper(paste(first_name,last_name)),first_name_up = toupper(first_name))]

diname <- dinscrit[,.(name,name_up_1,name_up_2,last_name,first_name_up)]
setorder(diname,name)


## importer les data de connexion

file <- "data_stage_theo/Stage_theorique_CRBPO_Rapport_de_presence_4-04-24.csv"
d <- read_delim(file,delim = "|",skip_empty_rows = FALSE)
d <- as.data.frame(d)

nb_skip <- grep("3. Activités en réunion", d[,1]) + 1
nb_skip
d <- read.csv(file,sep = "\t",skip = nb_skip)
setDT(d)
colnames(d) <- c("name_up","type_salle","nom_salle","time_in","time_out","duration","email","role")
head(d)

d[,`:=`(time_in = mdy_hms(time_in),time_out = mdy_hms(time_out))]
d[,duration := as.vector(difftime(time_out, time_in,units = "hours"))]
head(d)
d[,date := date(time_in)]
head(d)

d[,name_up := toupper(name_up)]

d <- d[,.(name_up,date,time_in,time_out,duration,email,role)]

###########################

dtname <- unique(d[,name_up])


dd <- data.frame(name_up_t = dtname,
                 name_up_i_1 = diname[amatch(dtname,diname[,name_up_1], maxDist = 5),name],
                 name_up_i_2 = diname[amatch(dtname,diname[,name_up_2], maxDist = 5),name])

setDT(dd)
dd[,name := ifelse(is.na(name_up_i_1),name_up_i_2, name_up_i_1)]
dd[name_up_i_1 != name_up_i_2, name := NA]
print(dd)


diname <- merge(diname,dd[,.(name_up_t,name)],by= "name",all.x = TRUE)
diname1 <- diname[!is.na(name_up_t),.(name,name_up_t)]

diname_na <- diname[is.na(name_up_t),.(name,last_name,first_name_up)]
diname_na[,name_up_t := NA]


dd <- NULL

for(n in 1:nrow(diname_na)) {
    ln_n <- diname_na[n,last_name]
    i <- grep(ln_n,dtname)
    if(length(i)>0) {
        ddi <- data.frame(name = diname_na[n,name],name_up_t = dtname[i])
        dd <- rbind(dd,ddi)
    }
}

setDT(dd)
dd <- unique(dd)
diname2 <- rbind(diname1,dd)
diname2 <- unique(diname2)
dim(diname2)

diname2 <- merge(diname2,diname[,.(name)],by="name",all.y = TRUE)
diname2 <- unique(diname2)
dim(diname2)
diname2[is.na(name_up_t),]
dim(diname2[is.na(name_up_t),])

dtname <- unique(d[,.(name_up)])
colnames(dtname) <- "name_up_t"
diname2 <- merge(diname2,dtname,by="name_up_t",all = TRUE)
diname2 <- unique(diname2)
dim(diname2)
diname2[is.na(name),]
dim(diname2[is.na(name),])
diname2 <- unique(diname2)
dim(diname2)
diname2[,valid := 1]
diname2[is.na(name) | is.na(name_up_t), valid := 0]

setorder(diname2,name)

diname2
write.xlsx(diname2,"data_stage_theo/synonyme.xlsx",sheetName="Sheet1")




################



dname_t <- read.xlsx("data_stage_theo/synonyme.xlsx",sheetName="Sheet1")
setDT(dname_t)
dim(dname_t)
head(dname_t)

dname_t <- unique(dname_t)
dim(dname_t)
head(dname_t)


dname_t <- na.omit(dname_t[,.(name_up_t,name)])
dim(dname_t)



###############


setnames(dname_t,"name_up_t","name_up")
dim(d)
d <- merge(d,dname_t,by="name_up",all.x=TRUE)
dim(d)

d <- d[!is.na(name),]
dim(d)

###################

dd <- d[,.(duration = sum(duration)),by = .(name,date)]


dinscrit2 <- unique(dinscrit[,.(name,email,type,attestation)])
dtype <- unique(dinscrit[,.(name,type)])
dd <- merge(dd,dtype,by = "name",all.x = TRUE)
dd[is.na(type), type := "generalist"]

dd[,`:=`(conforme = !outlier_turkey(duration,k=3),median = median(duration)),by = .(date,type)]
dd[duration > median, conforme := TRUE]
dd[,conforme := ifelse(conforme == TRUE, "OUI","NON")]
dd_dure <- dd[,.(name,date,duration)]
dd_out <- dd[,.(name,date,conforme)]


dd_dure <- dcast(data= dd_dure,name~date,value.var="duration",sep = "_",fill=0)
setnames(dd_dure, colnames(dd_dure)[-1], paste(colnames(dd_dure)[-1],"durée",sep="_"))

dd_out <- dcast(dd_out,name~date,value.var="conforme",sep = "_",fill="NON")
setnames(dd_out, colnames(dd_out)[-1], paste(colnames(dd_out)[-1],"conforme",sep="_"))

dd_dure <- merge(dd_dure, dd_out, by = "name")

dinscrit2 <- merge(dinscrit2,dd_dure,by="name",all.y = TRUE)

write.xlsx(dinscrit2,"data_stage_theo/inscription_stage_theorique.xlsx",sheetName="présence",append=TRUE)


