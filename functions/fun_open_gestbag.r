require(data.table)
require(openxlsx)
require(lubridate)

f_open_extrait_xlsx <- function(file,using_case =NULL,selected_col = c("ACTION","CENTRE","BAGUE","DATE","HEURE", "ESPECE","SEXE","AGE","MEMO SESSION","MEMO","FS","HS","DS","NF","CS","PI","PC","LP","LT","MA","ES","MU","COND_REPR","CIRC_REPR","PAYS","DEPT","LOCALITE","LIEUDIT","LAT","LON","THEME SESSION","BAGUEUR","BG") ) {


    d <- read.xlsx(file)
    setDT(d)
    setnames(d,colnames(d),gsub(" |\\.","_",colnames(d)))

    if(!is.null(using_case)) {
        if(using_case == "10p") {
            selected_col =c("ACTION","CENTRE","BAGUE","ESPECE","Nom_vernaculaire","BAGUEUR")
        }
    }

    if(!is.null(selected_col)) {
        d <- d[,selected_col,with = FALSE]
    }


    if("DATE" %in% colnames(d)) {
        d[,DATE := as_date(DATE)]
        d[,YEAR := year(d$DATE)]
    }

    if("HEURE" %in% colnames(d)) d[,HEURE := format(as.POSIXct(HEURE),"%H:%M")]
    if("HS" %in% colnames(d)) d[,HS := format(as.POSIXct(HS),"%H:%M")]
    if("DS" %in% colnames(d)) d[,DS := format(as.POSIXct(DS),"%H:%M")]

    if("BAGUE" %in% colnames(d)) d[,BAGUE := gsub("\\.","",BAGUE)]

    return(d)
}



f_clean_colnames <- function(DT)  setnames(DT,colnames(DT),iconv(tolower(gsub(" |\\.","_",colnames(DT))),from = 'UTF-8', to = 'ASCII//TRANSLIT'))


trim <- function (x) gsub("^\\s+|\\s+$", "", x)
