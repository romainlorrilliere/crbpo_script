require(ggplot2)
require(data.table)
require(xlsx)



f_bilan_pp(1212,last_y = 2023) {



    dd <- melt(d,id.vars = c("source","sp","annee","classe"),variable.name = "action",value.name = "nombre")
    setDT(dd)
    dd[,nombre := as.numeric(gsub(" ","",nombre))]
    ## dd[annee < 2020, annee := "1978-2019"]

    ## dd

    ddd <- dd[,.(nombre = sum(nombre)),by = .(source,sp,annee,action)]
    d <- dcast(ddd, sp + annee + action ~ source)
    setDT(d)
    d[is.na(d)] <- 0
    d[,`:=`(difference = GestBag - Bilan, pourcent = round(GestBag/Bilan * 100))]
    d <- d[GestBag != 0 | Bilan != 0, ]

    ## d
    file <- paste0("output/bilan_gestbag_pp_",numpp,".xlsx")
    write.xlsx(d, file)

    ddiff <- d[difference != 0, ]

    file <- paste0("output/bilan_gestbag_pp_",numpp,"_diff.xlsx")
    write.xlsx(ddiff, file)
    print(d)
    return(d)
}
