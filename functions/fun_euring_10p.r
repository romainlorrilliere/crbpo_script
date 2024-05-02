require(data.table)
require(openxlsx)
require(lubridate)

source("functions/fun_open_gestbag.r")

f_euring_10p <- function(name ="extrait_seabird_euring_2024-04-09" ) {
    name ="extrait_seabird_euring_2024-04-09"
    file <- paste0("data/",name,".xlsx")

    d <- f_open_extrait_xlsx(file)

    d[grep(" X | x ",Nom_vernaculaire),Nom_vernaculaire := ""]
    d <- d[Nom_vernaculaire != "",]

    dd <- d[,.(nb = .N),by=.(BAGUEUR,Nom_vernaculaire)]

    dd[, tot := sum(nb),by=.(Nom_vernaculaire)]
    dd[,prop := round(nb/tot,2)]

    ddd <- dd[BAGUEUR != "" & Nom_vernaculaire != "" ,.(BAGUEUR, Nom_vernaculaire, prop)]
    setnames(ddd,"prop","prop_baguage_control")

    d_b <- d[ACTION == "B",]
    dd1 <- d_b[,.(nb = .N),by=.(BAGUEUR,Nom_vernaculaire)]

    dd1[, tot := sum(nb),by=.(Nom_vernaculaire)]
    dd1[,prop_baguage := round(nb/tot,2)]

    ddd2 <- dd1[BAGUEUR != "" & Nom_vernaculaire != "" ,.(BAGUEUR, Nom_vernaculaire, prop_baguage)]

    ddd <- merge(ddd,ddd2,by=c("BAGUEUR","Nom_vernaculaire"),all = TRUE)
ddd[,prop_max := ifelse(prop_baguage_control>prop_baguage,prop_baguage_control,prop_baguage)]
    setorder(ddd,-prop_max)

    ddd
    return(ddd)

}
