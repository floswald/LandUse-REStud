# Functions to build the historical land price database







#  DEPARTEMENT
# ============

#' Read Departement Classification from disk
#'
#' https://www.insee.fr/fr/information/4316069
read_depts <- function(){
    d = fread(file.path(datadir(),"cog_ensemble_2020_csv","departement2020.csv"))
    # change codgeo of cheflieu for paris
    # d[dep == "75", cheflieu := "75060"]  # we don't care - we just use dep anyway
    d
}

#' Department level land prices 1892
#'
#' Read 1892 Table 1
#'
read_prices_1892 <- function() {
    z = fread(file.path(OCRdir(), "1892","1892-cleaning","valeur_labourable-Table 1.csv"))
    n <- z[, lapply(.SD, function(x) as.numeric(gsub(",","",x)) ), .SDcols = labourable_1:labourable_5]
    d = cbind(z[,.(V1,V2,share_1,share_2,share_3,share_4,share_5)],n)
    setnames(d,c("V1","V2"),c("departement","dep_clean"))
    shares = as.matrix(d[,.SD,.SDcols = share_1:share_5]) / 100
    prices = as.matrix(d[,.SD,.SDcols = labourable_1:labourable_5])
    d[, francs_per_hectare := rowSums(shares * prices)]

    # merge DEP classification
    r <- read_depts()

    dr <- merge(d, r, by.x = "dep_clean", by.y = "ncc",all.x = TRUE)
    dr <- dr[dep_clean != ""]
    del = c("departement", paste0("share_",1:5), paste0("labourable_",1:5), "tncc", "nccenr", "libelle")
    dr[,(del) := NULL]
    dr[, year := 1876]

    # fix paris: cannot be only SEINE, but has to include Seine et Marne, Yvelines
    dr[dep_clean == "PARIS", francs_per_hectare := dr[dep_clean %in% c("PARIS","YVELINES","SEINE ET MARNE"), mean(francs_per_hectare)]]

    fwrite(dr, file.path(OCRdir(),"..", "cleaned-output","table1-1892.csv"))

    dr
}


#' Department level land prices 2018
#'
#' from tableaux_VVT 2020
#'
read_prices_2018 <- function(){

    # better
    l = readODS::list_ods_sheets(file.path(OCRdir(), "2018","cd2021-10_tableaux_VVT 2020.ods"))
    w = grep("Corse",grep("(Libres)",l,value = TRUE),invert= TRUE,value = TRUE)

    L = list()
    for (iw in w){
        z = data.table(readODS::read_ods(file.path(OCRdir(), "2018","cd2021-10_tableaux_VVT 2020.ods"), sheet = iw,skip = 2,na = "NS"))
        setnames(z, c("dept",paste(2018:2020), "2020/2019","minimum","maximum"))
        L[[iw]] <- z[is.na(maximum),.(dept,`2018`,`2019`,`2020`)][!is.na(`2019`)]
        L[[iw]][, c("2018","2019","2020") := lapply(.SD, as.numeric), .SDcols = 2:4]
    }
    z = rbindlist(L)

    # throw out weirdos
    z[dept %like% "NOUVELLE-AQUITAINE", dept := gsub("NOUVELLE-AQUITAINE - ","",dept)]
    z[dept %like% "GRAND-EST - MEUSE", dept := gsub("GRAND-EST - ","",dept)]



    # z = fread(file.path(OCRdir(), "2018","p8-23.csv"), drop = c(1:6,11), na.strings = "NS",skip = 1, header = TRUE)
    # setnames(z, c("departement",paste0("terres_pres_",2018:2020), "minimum", "maximum" ))
    # z[, isdept := (minimum == "") & (maximum == "")]
    #
    # # fix NA
    # z[, c("terres_pres_2018","terres_pres_2019","terres_pres_2020") := lapply(.SD, function(x) {x[x=="NS"] <- NA; x}), .SDcols = terres_pres_2018:terres_pres_2020]
    #
    # n <- z[, lapply(.SD, function(x) as.numeric(gsub(" r|r| |\u00a0","",x))), .SDcols = terres_pres_2018:maximum]
    #
    # z = cbind(z[,.(departement, isdept)],n)
    #
    # subdept = z[(!isdept)][]
    # dept = z[(isdept)]
    #
    # subdept[, isdept := NULL]
    # dept[, c("isdept","minimum","maximum") := NULL]

    # get a clean ASCII departement name
    z[, dep_clean := stringi::stri_trans_general(dept, "Latin-ASCII")]

    # manual overrides
    # z[ dep_clean == "SEINE-ET-MARNE", dep_clean := "VAL-DE-MARNE"]

    # get dep classifier
    r = read_depts()
    r[, nccenr2 := stringi::stri_trans_general(toupper(nccenr), "Latin-ASCII")]

    # do a partial string match
    # pm = pmatch(z$dep_clean, r$nccenr2, duplicates.ok = FALSE)
    # list(z,r,pm)
    zr = merge(z, r, by.x = "dep_clean", by.y = "nccenr2", all = TRUE)
    zr[,year := 2015]  # assign to 2015 for now

    fwrite(zr, file.path(OCRdir(),"..", "cleaned-output","from-ods-file-2018.csv"))

    list(zr,z,r)
}


#' Department level land prices 1990
#'
#' Read 1990 Table 1
#'
#' Francs per hectare
read_prices_1990 <- function() {
    z = fread(file.path(OCRdir(), "1990","table1.csv"), skip = 2, drop = c(1:6,9,11,14), header = TRUE, na.strings = "III")
    setnames(z, c("departement", "labourable_1990", "prairie_1990", "ensemble_1989", "ensemble_1990" ))

    n <- z[, lapply(.SD, function(x) as.numeric(gsub(" r|r| |\u00a0","",x))), .SDcols = labourable_1990:ensemble_1990]
    z = cbind(z[,.(departement)],n)

    regions <- z[(departement == toupper(departement)) | (departement == "REGION d'ILE de FRANCE")]
    setnames(regions, "departement", "region")

    depts <- z[(departement != toupper(departement)) & (departement != "REGION d'ILE de FRANCE")]
    depts[,DEP := substr(departement,1,2)]
    depts[, departement := substr(departement,start = 6, stop = nchar(departement) )]
    setcolorder(depts, c("departement","DEP"))
    depts[, year := 1990]

    fwrite(depts, file.path(OCRdir(),"..", "cleaned-output","table1-1990.csv"))

    list(regions = regions, depts = depts)
}


#' Department level land prices 1975
#'
#' Read 1975 Table 1
#'
#' Francs per hectare
read_prices_1975 <- function() {
    z = fread(file.path(OCRdir(), "1975","table1-cleaned.csv"), skip = 4, drop = 1:6, header = TRUE)
    setnames(z, c("departement", "labourable_1975", "labourable_1975_1974", "prairies_1975", "prairies_1975_1974" , "ensemble_1974","ensemble_1975","ensemble_1975_1974"))

    n <- z[, lapply(.SD, function(x) as.numeric(gsub("\u00a0","",x)) ), .SDcols = labourable_1975:ensemble_1975_1974]
    z = cbind(z[,.(departement)],n)

    regions <- z[(departement == toupper(departement)) | (departement == "REGION d'ILE de FRANCE")]
    setnames(regions, "departement", "region")

    depts <- z[(departement != toupper(departement)) & (departement != "REGION d'ILE de FRANCE")]
    depts[,DEP := substr(departement,1,2)]
    depts[, departement := substr(departement,start = 6, stop = nchar(departement) )]
    setcolorder(depts, neworder = c(1,9,2:8))
    depts[, year := 1975]
    fwrite(depts, file.path(OCRdir(),"..", "cleaned-output","table1-1975.csv"))

    list(regions = regions, depts = depts)
}


#' Department level land prices 1950
#'
#'
#' Francs per hectare
read_prices_1950 <- function(){

    indices = seq(1,9,by = 2)
    skips = c(2,2,2,2,2)
    drops = list(c(1:9,11),1:7,1:7,c(1:7,9),c(1:7,9))
    L = list()
    for (i in 1:length(indices)){
        ix = indices[i]
        fi = paste0(ix,"-",ix+1,"_COMPI-0-png.csv")
        z = fread(file.path(OCRdir(), "1950-1968","splitting",fi), skip = skips[i], drop = drops[[i]], header = TRUE)
        # clean names
        setnames(z, c("departement",gsub(" \\*| ","",names(z)[-1])))

        # clean departement column
        z[ , departement := trimws(gsub(" |Т.L.|T.L.|TL.|T.L|P.N.|PN.|T L.| P L.","",departement))]
        # add PN TL column indicator
        z[ , kind := rep(c("labourable","prairie"), times = nrow(z)/2)]
        # fillin missing department entries
        z[ , departement := rep(departement[seq(2,nrow(z),by = 2)],each = 2)]
        setcolorder(z,c("departement","kind"))
        z[,V30 := NULL]

        # clean I for 1x[]
        # get rid of stars and weird zeros
        n <- z[, lapply(.SD, function(x) as.numeric(gsub("\\!","1",gsub("I","1",gsub("\\*| |\u00a0|\\.","",x)))) ), .SDcols = `1950`:`1969`]

        z = cbind(z[,.(departement,kind)],n)

        # save
        L[[i]] <- z
    }
    LL = rbindlist(L)
    regs <- LL[departement == toupper(departement)]
    setnames(regs, "departement", "region")
    d <- LL[departement != toupper(departement)]
    d[departement == "A1lier" , departement := "Allier"]
    # get departement code
    r <- read_depts()

    dr <- merge(d, r, by.x = "departement", by.y = "nccenr",all.x = TRUE)

    # fix paris: cannot be only SEINE, but has to include Seine et Marne, Yvelines
    # append paris
    pl = dr[reg == 11 & kind == "labourable", lapply(.SD, mean, na.rm = TRUE), .SDcols = 3:22]
    pp = dr[reg == 11 & kind == "prairie", lapply(.SD, mean, na.rm = TRUE), .SDcols = 3:22]
    dr = rbind(dr,
               rbind(
               cbind(data.table(departement = "Paris", kind = "labourable", dep = "75", reg = "11", cheflieu = "75060" , ncc = "PARIS"), pl),
               cbind(data.table(departement = "Paris", kind = "prairie", dep = "75", reg = "11", cheflieu = "75060" , ncc = "PARIS"),pp)), fill = TRUE
    )

    del = c("tncc", "libelle")
    dr[,(del) := NULL]
    dr[,year := 1950]
    #
    fwrite(dr, file.path(OCRdir(), "..","cleaned-output","1950-1968-p55-p121.csv"))

    list(regs = regs,deps = dr)
}


# #' Department Level Land Prices:
# #'
# #' Combines All Waves
# #'
# combine_prices <- function(overwrite = FALSE){
#     if (overwrite){
#         p1892 = read_prices_1892()
#         p1950 = read_prices_1950()
#         p1975 = read_prices_1975()
#         p1990 = read_prices_1999()
#         p2018 = read_prices_2018()

#         # read output
#         o = read_output()
#         o[,dep := stringr::str_pad(DEP,width = 2, side = "left", pad = "0")]
#         setcolorder(o,"dep")
#         o <- o[,-c(11:23)]

#         setkey(o, dep, year)

#         L = list()
#         L[[1]] = merge(o, p1892[,.(departement = dep_clean,francs_per_hectare,dep,year)], by = key(o))
#         L[[2]] = merge(o, p1950$deps[kind == "labourable",.(departement = ncc,francs_per_hectare = `1950`,dep,year)], by = key(o))
#         L[[3]] = merge(o, p1975$depts[,.(departement,francs_per_hectare = labourable_1975,dep=DEP,year)], by = key(o))
#         L[[4]] = merge(o, p1990$depts[,.(departement,francs_per_hectare = labourable_1990,dep=DEP,year)], by = key(o))
#         L[[5]] = merge(o, p2018[[1]][,.(departement = dep_clean,francs_per_hectare = (`2018` * 6.55957),dep,year)], by = key(o))

#         y = rbindlist(L)

#         p1 = ggplot(y, aes(x = francs_per_hectare,y = area, color = factor(year)) )+ geom_point() + scale_x_log10(name = "log francs per hectare", labels = scales::label_number(big.mark = ",") ) +scale_y_log10(name = "log area (km2)") + geom_smooth(method = "lm") + theme_bw() + ggtitle("Cross Section by Year", subtitle = "1 Euro (2015) = 6.55 Francs. No Inflation adjustment.")

#         # cities with greater growth in prices have smaller growth in area.
#         # growth
#         yg = y[year != 1950,.(departement,year,
#                               pctarea = 100 * (area - data.table::shift(area)) / data.table::shift(area),
#                               pctfrancs = 100 * (francs_per_hectare - data.table::shift(francs_per_hectare))/data.table::shift(francs_per_hectare),
#                               logdarea = log(area) - log(shift(area)),
#                               logdfrancs = log(francs_per_hectare) - log(shift(francs_per_hectare))), by = CODGEO]
#         # yg[is.na(darea), c("darea","dfrancs") := 0]

#         fwrite(y[,!"extent"], file.path(OCRdir(),"..", "cleaned-output","area-pop-prices.csv"))
#         list(y,yg,p1)


#     } else {
#         y = data.table::fread(file.path(OCRdir(),"..", "cleaned-output","area-pop-prices.csv"))
#         p1 = ggplot(y, aes(x = francs_per_hectare,y = area, color = factor(year)) )+ geom_point() + scale_x_log10(name = "log francs per hectare", labels = scales::label_number(big.mark = ",") ) +scale_y_log10(name = "log area (km2)") + geom_smooth(method = "lm") + theme_bw() + ggtitle("Cross Section by Year", subtitle = "1 Euro (2015) = 6.55 Francs. No Inflation adjustment.")

#         # cities with greater growth in prices have smaller growth in area.
#         # growth
#         yg = y[year != 1950,.(departement,year,
#                               pctarea = 100 * (area - data.table::shift(area)) / data.table::shift(area),
#                               pctfrancs = 100 * (francs_per_hectare - data.table::shift(francs_per_hectare))/data.table::shift(francs_per_hectare),
#                               logdarea = log(area) - log(shift(area)),
#                               logdfrancs = log(francs_per_hectare) - log(shift(francs_per_hectare))), by = CODGEO]
#         # yg[is.na(darea), c("darea","dfrancs") := 0]

#         list(y,yg,p1)
#     }
# }

