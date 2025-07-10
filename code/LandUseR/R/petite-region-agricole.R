# PETITE REGION AGRICOLE

isparis <- function() {
    x = pra_specials()$Paris
    x[,unique(PRA_Code)]
}

#' Read Hand-Cleaned PRA Data
#'
#' this is the hand cleaned version generated from
#' [pra_manual_check()]
pra_cleaned <- function(){
    d = data.table(readxl::read_excel(file.path(PRAdir(),"PRA_cleaned_20221201.xlsx"),sheet = "checking_PRA"))
    d[,CODGEO := stringr::str_pad(codgeo, width = 5, side = "left", pad = 0)]
    d[,PRA_Code := stringr::str_pad(pra_code_corr, width = 5, side = "left", pad = 0)]
    d[,year := as.integer(as.character(year))]
    d[year == 2015, price_corrected := euros_2_francs(price_corrected)]
    d = d[,.(CODGEO,LIBGEO = libgeo,year,price = price_corrected, PRA_Code, PRA_Lib = pra_name_corr)]
    # add paris
    pc = pra_specials()$Paris
    pc = pc[,.(CODGEO = "75060",LIBGEO = "Paris",price = mean(price), PRA_Code = "75000",PRA_Lib = "Paris"), by = year]
    o = rbind(d, pc, use.names = TRUE)

    p2000 = PRA_2015_VVTDiffusion(yr = 2000)
    merge(o, p2000$merge[,.(PRA_Code,new_price_2000 = price,year = 2000)],
          by = c("PRA_Code","year"), all.x = TRUE)
}

#' PRA special cases
#'
#' This returns a list of special cases cities where we make
#' manual adjustments to the list of surrouding PRAs.
#' 
#' This is for the most part cities where no unique PRA encloses the entire city, which happens for
#' four cities in total.
pra_specials <- function(overwrite = FALSE) {
    if (overwrite){
        L = list()
        # Paris
        d = readxl::read_excel(file.path(PRAdir(),"PRA_Paris.xlsx"),sheet = 1,range = "A1:K15",col_types = c(rep("text",2),rep("numeric",9)))
        d = data.table(d[-1 , c(1:5,7,9)])
        setnames(d,c("PRA_Code","PRA_Lib","1950","1975","1990","2000","2015"))
        d[,`2015` := euros_2_francs(`2015`)]

        L$Paris = melt.data.table(d, id.vars = c("PRA_Code","PRA_Lib"),variable.name = "year",value.name = "price",variable.factor = FALSE)
        L$Paris[, year := as.integer(year)]

        # Lyon
        d = data.table(readxl::read_excel(file.path(PRAdir(),"PRA_Paris.xlsx"),sheet = "Pra_lyon",range = "A1:G6",col_types = c(rep("text",2),rep("numeric",5))))
        setnames(d,c("PRA_Code","PRA_Lib","1950","1975","1990","2000","2015"))
        d[,`2015` := euros_2_francs(`2015`)]


        # do not use Bas Dauphine
        d = d[PRA_Code != "69199"]

        L$Lyon = melt.data.table(d, id.vars = c("PRA_Code","PRA_Lib"),variable.name = "year",value.name = "price",variable.factor = FALSE)
        L$Lyon[, year := as.integer(year)]

        # Nantes
        d = data.table(readxl::read_excel(file.path(PRAdir(),"PRA_Paris.xlsx"),sheet = "Pra_nantes",range = "A1:G6",col_types = c(rep("text",2),rep("numeric",5))))
        setnames(d,c("PRA_Code","PRA_Lib","1950","1975","1990","2000","2015"))
        d[,`2015` := euros_2_francs(`2015`)]

        L$Nantes = melt.data.table(d, id.vars = c("PRA_Code","PRA_Lib"),variable.name = "year",value.name = "price",variable.factor = FALSE)
        L$Nantes[, year := as.integer(year)]

        # Nice
        d = data.table(readxl::read_excel(file.path(PRAdir(),"PRA_Paris.xlsx"),sheet = "Pra_nice",range = "A1:H6"))
        d = d[ , c(1,4,8)]
        setnames(d,c("year","PRA_Code","price"))
        d[,price := as.numeric(price)]
        d[,year := as.integer(year)]
        d[year == 2015, price := euros_2_francs(price)]

        L$Nice = d

        saveRDS(L, file.path(PRAdir(),"pra-prices-specials.Rds"))

    } else {
        # L = readRDS(file.path(system.file(package = "LandUseR","data","pra-prices-specials.Rds")))
        L = readRDS(file.path(PRAdir(),"pra-prices-specials.Rds"))
    }
    L
}


pra_shape <- function(paris = TRUE,disk = FALSE){
    if (disk){
        if (paris){
            s = readRDS(file.path(datadir(),"paris-pra-shape.Rds"))
        } else {
            s = readRDS(file.path(datadir(),"pra-shape.Rds"))
        }
    } else {
        ref = load_PRA()
        ref = ref[,.(CODGEO,PRA_Code,PRA_Name)]
        if (paris){
            ref = ref[PRA_Code %in% isparis() ]
        }

        # load a communes shapefile
        sh = sf::read_sf(file.path(datadir(),"communes-20150101-100m-shp","communes-20150101-100m.shp"))

        # merge
        sf = sh %>%
            dplyr::right_join(ref ,
                              by = c("insee" = "CODGEO"))
        sp = sf %>%
            dplyr::group_by(PRA_Code,PRA_Name) %>%
            dplyr::summarise(geometry =
                                 sf::st_union(geometry, is_coverage = TRUE),
                             n = dplyr::n(),.groups = "drop")
        s = list(full = sf, pras = sp)
        if (paris){
            saveRDS(s, file.path(datadir(),"paris-pra-shape.Rds"))
        } else {
            saveRDS(s, file.path(datadir(),"pra-shape.Rds"))
        }
    }
    s
}

city_locations <- function(topn,readdisk = FALSE){

    if (readdisk){
        chef_lieux = readRDS(file = file.path(igndir(),paste0("city-",topn,"-centers.Rds")))
    } else {
        city_list = export_pop_area(topn = topn)[,unique(CODGEO)]
        chef_lieux = sf::st_read(file.path(igndir(),
                                           "ign_metropole_adminexpress_chefs_lieux_z.shp")) %>%
            filter(insee_com %in% city_list)
        saveRDS(chef_lieux,file = file.path(igndir(),paste0("city-",topn,"-centers.Rds")))
    }
    chef_lieux
}

pra_check <- function(paris = TRUE, disk = TRUE, topn = 0, alpha = 0.5){

    # all pra prices
    p = all_pra_prices(readdisk = TRUE)

    # paris pra shapes
    sh = pra_shape(paris = paris,disk = disk)

    # ggplot(data = sh$full) + geom_sf(aes(fill = PRA_Code))

    pramap = mapview::mapview(sh$full,layer.name = "PRA_Code", zcol = "PRA_Code", col.regions = rainbow(length(unique(sh$full$PRA_Code))))

    if (topn > 0){
        clocs = city_locations(topn = topn, readdisk = TRUE)
    }

    l = list()
    m = list()

    for (il in names(p)){
        l[[il]] <- sh$pras %>%
            left_join(p[[il]][,.(pra_code,price = round(price))], by = c("PRA_Code" = "pra_code"))
        atb = quantile(l[[il]]$price,na.rm = TRUE)
        if (il == "2015"){
            atb = atb[-1]
        }
        m[[il]] <- mapview::mapview(l[[il]], zcol = "price",
                           alpha.regions = alpha, at = atb)

        if (topn > 0){
            m[[il]] = m[[il]] + mapview::mapview(clocs, cex = 4, col.regions = "red")
        }
    }
    list(prices = m, pras = pramap)
}



# link 1950 price data to PRA =======================



clean_pra_value_1950 <- function(){
    z = fread(file.path(PRAdir(), "output","PRA-terres-1950-cleaned.csv"), skip = 2)

    # set names
    setnames(z, c("region","type","1950","1953","1958","1960",paste(1962:1968)))
    # drop empty rows
    z1 = z[`1950` != ""]
    # drop MOYENNE and INDICE rows
    moytype = z1[, grep("MOYENNE|INDICE|ENSEMBLE",type)]
    moyregi = z1[, grep("MOYENNE|INDICE|ENSEMBLE",region)]
    z2 = z1[-union(moyregi,moytype)]
    # grep numeric codes from region
    z2[, pra_code := gsub(" ",replacement = "", x = stringr::str_extract(region, "\\d{2} \\d{3}|\\d{2}\u00a0\\d{3}"))]
    z2[, dep := substr(pra_code, 1,2)]

    # grep alphanumeric label from region
    z2[,label := stringr::str_remove(region, "\\d{2} \\d{3}")]
    z2[,label := stringr::str_remove(label, "\\d{2} \\d{3}| \\- [A-Z][1-9] | \\- [A-Z]{1,2} |\\- [A-Z][1-9] |\\- [A-Z]{1,2} |\\-[A-Z][1-9] |\\-[A-Z]{1,2} | \\- [1-9]{1,2} |\\-[1-9]{1,2} |\\: |\\; ")]

    # check that each region is next to the DOM row
    isdom = union( z2[,grep("DOM|DOO|DOB",type)], z2[,grep("DOM|DOO|DOB",region)] )
    z2[isdom , sum(is.na(pra_code))] # 6

    # keep only dominant price
    z3 <- z2[isdom][!is.na(pra_code)]

    setcolorder(z3, c("pra_code","dep","label"))
    z3[,c("region","type") := NULL]

    # clean numeric values
    n = z3[, lapply(.SD, function(x) as.numeric(gsub("\\!","1",gsub("I","1",gsub("\\*| |\u00a0|\\.| ","",x)))) ), .SDcols = `1950`:`1968`]
    z3 <- cbind(z3[,.(pra_code,dep,region = label)], n)
    z4 = melt.data.table(z3, id.vars = c("pra_code","dep","region"),variable.name = "year", value.name = "price")
    z4[, year := as.integer(as.character(year))]
    z4
}

#' Paris land value 1950
#'
#' terre labourable only
#'
#' We take all PRA directly adjacent to paris as well as CEINTURE DE PARIS as
#' on this map: https://driaaf.ile-de-france.agriculture.gouv.fr/IMG/pdf/RA_R11_cle4ce9b1.pdf
paris_pra_1950s_labour <- function(tl0,year = 1950,keep_pras = FALSE){
    tl0 = tl0[year == year]

    x = tl0[pra_code %in% isparis()]

    x = x[,.(region,pra_code,dep,price,year = year)]
    # return(x)

    kableExtra::kbl(x, booktabs = TRUE, format = "latex", align = "rccc", longtable = FALSE,
                    label = "paris-pra-prices-1950",caption = "Paris Region PRA prices 1950") %>%
        readr::write_lines(file.path(outdatadir(),"tables",paste0("paris-pra-prices-",year,".tex")),append = FALSE)

    data.table::fwrite(x, file.path(outdatadir(),"tables",paste0("paris-pra-prices-",year,".csv")))

    if (keep_pras){
        x[,.(pra_code = "75000", region = paste(region, pra_code), dep = "75",price,year)]
    } else {
        x[,.(pra_code = "75000", region = "Paris",dep = "75",price = mean(price,na.rm = TRUE), year = year[1])]

    }
}

#' map pra 1950s
#'
map_pra_prices1950 <- function(year = 1950){
    p = clean_pra_value_1950()
    stopifnot(year %in% unique(p$year))

    p[ year == year]
    p = p[,.(pra_code,price)]

    m = mapdata_pra() %>%
        inner_join(p, by = c("PRA_Code" = "pra_code")) %>%
        mapview::mapview(zcol = "price", at= quantile(p$price,probs = c(seq(0,0.75,length.out = 6),0.9,0.95,0.99,0.995,1), na.rm = TRUE))
    m
}

#' Match Cities to Prices in 1950
#'
#' Similar to [pra_1975()], performs some manual assignment.
#' adds paris via [paris_pra_1950s_labour()].
#'
#' @param yr integer by default 1950. One can choose the relevant year
#' to consider for the PRA prices. For instance, we tried 1966 once.
#' @param paris_pra Boolean by default `FALSE` indicating that we do not
#' want to keep parisian PRAs disaggregated in the data.
#'
#' outputs to
#' `data/statistique-agricole/petite-region-agricole/area-pop-prices-1950.csv`
pra_1950 <- function(yr = 1950,paris_pras = FALSE){

    z = clean_pra_value_1950()
    z = z[year == yr]  # optionally don't take 1950 prices but another year
    par = paris_pra_1950s_labour(z,year = yr,keep_pras = paris_pras)
    z = rbind(z,par, use.names = TRUE, fill = TRUE)
    setnames(z,"region","PRA_Lib")

    r = merge_results_PRA()  # top 100 cities with PRA code
    r <- r[year == 1954]
    setkey(r , CODGEO)

    r[,year := 1950]  # but model runs on 1950
    setkey(r , CODGEO)
    rz = merge(r[,!"PRA_Lib"],z[,!"year"], by.x = c("PRA_Code"),by.y = c("pra_code"))

    # aoc = readAOC()
    # rz[, AOC := CODGEO %in% aoc$insee]
    fwrite(rz[,.(CODGEO,LIBGEO,PRA_Code, PRA_Lib,PRA_Name,price)], file.path(PRAdir(),"pra-prices-1950.csv"))
    rz
}




# year 1975 ===========================


#' Read 1975 PRA data from extracted file
#'
clean_pra_value_1975 <- function(){
    z = fread(file.path(PRAdir(), "output","PRA-terres-1975-cleaned.csv"), skip = 2)
    cats = c("dominante","min","max","growth")
    setnames(z, c("region","ismultireg","lab_1974",
                  paste(paste("lab",cats,sep = "_"),"1975",sep = "_"),"prarie_1974",
                  paste(paste("prarie",cats,sep = "_"),"1975",sep = "_")))

    # drop intermittent header rows
    z <- z[!(region == "" | region == "Régions de programme\nDépartements" | region == "Départements\nRégions agricoles")]

    # get numeric codes from region field
    z[, pra_code := gsub(x = stringr::str_extract(region, "\\d{2}-\\d{3}"), pattern = "-",replacement = "")]
    z[, dep_code := substr(stringr::str_extract(region, "^\\d{2}"), 1,2)]

    # get clean libelle
    z[,label := stringr::str_extract(region, "\\b[a-zA-Z-\\- ]{2,1000}\\b")]

    # drop growth
    z[, grep("growth",names(z)) := NULL ]

    # clean numeric values
    n = z[, lapply(.SD, function(x) as.numeric(gsub("\\!","1",gsub("I","1",gsub("\\*| |\u00a0|\\.| ","",x)))) ), .SDcols = lab_dominante_1975:prarie_max_1975]
    z <- cbind(z[,.(region,ismultireg,pra_code,dep_code,label)], n)
    z
}

#' map pra 1975
#'
map_pra_prices1975 <- function(){
    yr = 1975
    p = clean_pra_value_1975()
    p = p[,.(pra_code,price = lab_dominante_1975)]

    m = mapdata_pra() %>%
        inner_join(p, by = c("PRA_Code" = "pra_code")) %>%
        mapview::mapview(zcol = "price", at= quantile(p$price,probs = c(seq(0,0.99,length.out = 6),1), na.rm = TRUE))
    m
}

plot_ts_pra_cities <- function(){
    # z = rbindlist(all_pra_prices(), use.names = TRUE,fill = TRUE) # prices for all PRA units. the mapping in 2015 from petitID and petitName to PRA_Code is incomplete.

    # cw = get_codgeo_pra_xwalk()
    # zxw = merge(z, cw, by.x = "pra_code", by.y = "PRA_Code")
    # r = unique(read_output()[, .(CODGEO,rank)]) # relevant cities

    # # need to back-fill 2015 where many citiets are missing
    #  # get name correspondance
    # xx = get_PRA_result_name_1999(r,p)

    # # for each year in results, add prices
    # mx = merge(xx, p[year == yr], by = c("dep","petitID"),all.x = TRUE )

    # get 1870
    r = read_output()
    r = r[year == 1876,.(CODGEO,rank, DEP, LIBGEO)]

    p1870 = data.table::fread(file.path(datadir(),"statistique-agricole","departement","cleaned-output","table1-1892.csv"))
    p1870_ = merge(p1870, r, by.x = "dep", by.y = "DEP",all.y = FALSE,all.x = FALSE)
    setnames(p1870_,"francs_per_hectare", "price")
    p1870[, year := 1892]

    x = pra_stack(topn = 100)  # to get 1950s prices!
    x = x[,.(rank,CODGEO,price,year,LIBGEO)]
    x_ = rbind(x,p1870_[,.(rank,price,year,CODGEO,LIBGEO)],use.names=TRUE,fill = TRUE)

    pal = tmaptools::get_brewer_pal("Accent",n = 21)

    zzr = x_[ , list(rank,CODGEO,LIBGEO,rprice = price / .SD[rank == 1,price ]), by = year]
    # zzr = zzr[LIBGEO != "Strasbourg"]

    pl = ggplot(zzr[rank < 22], aes(x = year, y = rprice,color = LIBGEO)) +
        geom_line(linewidth = 1) + geom_point(size =2) + theme_bw() + theme(panel.grid.minor = element_blank()) +
        scale_color_manual(values = pal) +scale_x_continuous(limits = c(1850,2022), breaks = c(1892,1950,1975,1990,2000,2015)) +
      directlabels::geom_dl(aes(label = LIBGEO), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
      directlabels::geom_dl(aes(label = LIBGEO), method = list(directlabels::dl.trans(x = x - 0.2), "first.points", cex = 0.8))
    ggsave(plot = pl, file.path(dataplots(),"city-prices.pdf"),width = 15, height = 9)

    pal = tmaptools::get_brewer_pal("Accent",n = 100)

    zzr = x_[ , list(rank,CODGEO,LIBGEO,rprice = price / .SD[rank == 1,price ]), by = year]
    # zzr = zzr[LIBGEO != "Strasbourg"]

    pl = ggplot(zzr, aes(x = year, y = rprice,color = LIBGEO)) +
        geom_line(linewidth = 1) + geom_point(size =2) + theme_bw() + theme(panel.grid.minor = element_blank()) +
        scale_color_manual(values = pal) +scale_x_continuous(limits = c(1850,2022), breaks = c(1892,1950,1975,1990,2000,2015)) +
        directlabels::geom_dl(aes(label = LIBGEO), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
        directlabels::geom_dl(aes(label = LIBGEO), method = list(directlabels::dl.trans(x = x - 0.2), "first.points", cex = 0.8))
    ggsave(plot = pl, file.path(dataplots(),"city-prices-all.pdf"),width = 15, height = 9)

}

tmapper <- function(sp,iy){
    sp %>%
        tmap::tm_shape() +
        tmap::tm_borders(col = "white", lwd = 0.3) +
        tmap::tm_fill(
            style = "quantile",
            palette = "YlOrRd",
            col = "price",   # variable to be mapped
            title = "Francs/ha",   # legend title
            id = "price_francs",   # information to display when mouse hovers over a departement
            popup.vars = c("region" = "region", "dep" = "dep","pra_code" = "pra_code")) +
        tmap::tm_layout(legend.position = c("left", "bottom"), title= paste(iy), title.position = c('left', 'top'))
}

map_pra_all <- function(){
    z = all_pra_prices()
    pra = load_PRA()

    # load a communes shapefile
    sh = sf::read_sf(file.path(datadir(),"communes-20150101-100m-shp","communes-20150101-100m.shp"))

    l = list()
    iy = 1892
    futile.logger::flog.info("doing year %d",iy)
    l[[paste(iy)]] <- tmapper(DEP_1892_prices(),"1892")


    for (iy in c(1950,1975,1990,2000)){
    # for (iy in c(2015)){
        futile.logger::flog.info("doing year %d",iy)
        a = z[[paste(iy)]]
        ms = a[pra_code %in% a[,.N,by=pra_code][N>1, pra_code]]
        ok = a[!(pra_code %in% a[,.N,by=pra_code][N>1, pra_code])]
        ms = ms[,.(PRA_Lib = region[1], dep = dep[1], price = mean(price,na.rm = TRUE), year = year[1]), by = pra_code]
        a = rbind(ok,ms,fill = TRUE)

        pra = load_PRA()
        pa = merge(pra,a[,!"PRA_Lib"], by.x = "PRA_Code", by.y = "pra_code" )
        pa = pa[!(DEP %in% c("2A","2B"))]

        # merge
        sp = sh %>%
            dplyr::right_join(pa ,
                              by = c("insee" = "CODGEO")) %>%
            dplyr::group_by(PRA_Code,PRA_Lib) %>%
            dplyr::summarise(geometry =
                                 sf::st_union(geometry, is_coverage = TRUE),
                             n = dplyr::n(),
                             surf_m2 = sum(surf_m2, na.rm = TRUE),
                             DEP = DEP[1],
                             price = mean(price, na.rm = TRUE))

        aj = sh %>%
            dplyr::anti_join(pa,by = c("insee" = "CODGEO"))

        if (iy == 1990){
            sm = get_seine_marne_1990() # prices of seine et marne
            sm77 = aj %>%
                dplyr::filter(grepl("^77", insee)) %>%
                dplyr::summarise(geometry = sf::st_union(geometry, is_coverage = TRUE),
                                 n = dplyr::n(),
                                 surf_m2 = sum(surf_m2, na.rm = TRUE),
                                 DEP = "77",
                                 price = sm[,mean(price,na.rm = TRUE)],
                                 PRA_Code = "77061",
                                 PRA_Lib = "Seine et Marne")
            sp = rbind(sp,sm77)

        } else if (iy == 2000){
            sm = get_seine_marne_2000() # prices of seine et marne

            sm77 = aj %>%
                dplyr::filter(grepl("^77", insee)) %>%
                dplyr::summarise(geometry = sf::st_union(geometry, is_coverage = TRUE),
                                 n = dplyr::n(),
                                 surf_m2 = sum(surf_m2, na.rm = TRUE),
                                 DEP = "77",
                                 price = sm[,mean(price,na.rm = TRUE)],
                                 PRA_Code = "77061",
                                 PRA_Lib = "Seine et Marne")
            sp = rbind(sp,sm77)
        }

        sp = sp[!sf::st_is_empty(sp), , drop = FALSE]

        l[[paste(iy)]] <- tmapper(sp,iy)
    }
    futile.logger::flog.info("doing year 2015 new")

    dd = PRA_2015_VVT()
    missings <- list(VVT = dd$miss)
    l[["20151"]] <- tmapper(PRA_2015_map_merge(dd$merge),"2015")

    dd = PRA_2015_VVTDiffusion()
    missings$VVTDiffusion = dd$miss

    l[["20152"]] <- tmapper(PRA_2015_map_merge(dd$merge),"2015")

    mm = tmap::tmap_arrange(l[1:6], ncol = 3, nrow = 2)
    tmap::tmap_save(tm = mm, filename = file.path(dataplots(),"figureA20.pdf"),width = 14,height=9)
    tmap::tmap_save(tm = tmap::tmap_arrange(l[6:7], ncol = 2, nrow = 1), filename = file.path(dataplots(),"map-pra-all-2015.pdf"),width = 14,height=9)
    return(list(mm,missings))
}


#' Paris land value 1975
#'
#' terre labourable only
#'
#' We take all PRA directly adjacent to paris as well as CEINTURE DE PARIS as
#' on this map: https://driaaf.ile-de-france.agriculture.gouv.fr/IMG/pdf/RA_R11_cle4ce9b1.pdf
paris_pra_1975_labour <- function(tl0,keep_pras = FALSE){

    x = tl0[pra_code %in% isparis()]

    x = x[,.(label,pra_code,dep_code,price = lab_dominante_1975)]

    kableExtra::kbl(x, booktabs = TRUE, format = "latex", align = "rccc") %>%
        readr::write_lines(file.path(outdatadir(),"tables","paris-pra-prices-1975.tex"))

    data.table::fwrite(x,file.path(outdatadir(),"tables","paris-pra-prices-1975.csv"))

    if (keep_pras){
        x[,.(pra_code = "75000" , region = paste(label,pra_code),dep_code = "75",lab_dominante_1975 = price)]
    } else {
        x[,.(pra_code = "75000", region = "Paris",dep_code = "75",lab_dominante_1975 = mean(price,na.rm = TRUE))]

    }
}





#' Match Cities to Prices in 1975 for 200 city case
#'
#' only do ranks 101-200
pra_1975 <- function(overwrite = FALSE,paris_pras = FALSE){
    z = clean_pra_value_1975()
    r = merge_results_PRA(topn = 200)
    r = r[rank >= 101]
    r <- r[year == 1975]
    r[CODGEO == "25031", c("PRA_Code") := list("25450")]  # Audincourt
    # r[CODGEO == "25388", c("PRA_Code") := list("25450")]  # Montbeliard is the same as Audincourt!
    r[CODGEO == "57606", c("PRA_Code") := list("57004")]  # saint avold
    r[CODGEO == "57240", c("PRA_Code") := list("57004")]  # Warndt

    setkey(r , CODGEO)

    zr = merge(r[,!"PRA_Lib"],z[,.(pra_code,PRA_Lib = region,price = lab_dominante_1975)],by.x = "PRA_Code", by.y = "pra_code", all.x = TRUE)

    # Auch has two PRAs, take average:
    # 32-147 - A- Haut Armagnac Zone N., 32-147 - Zone S.
    mp = zr[CODGEO == "32013", mean(price)]
    zr = zr[!(CODGEO == "32013" & price == 10500.0)] # get rid of duplicate row
    zr = zr[CODGEO == "32013", price := mp] # fill back

    # same for saintes
    # TODO nico
    #17-375 - S- Saintonge agricole ( + )
    #17-375 - T Double\nsaintongeaise ( + )
    mp = zr[CODGEO == "17415", mean(price)]
    zr = zr[!(CODGEO == "17415" & price == 10000.0)] # get rid of duplicate row
    zr = zr[CODGEO == "17415", price := mp] # fill back

    # montbeliard: 10000
    zr[CODGEO == "25388", price := 10000]

    zr

}

pra_1975_plots <- function(){
    zr = pra_1975()
    zr[,lab2 := ""]
    zr[LIBGEO == "Paris", lab2 := "Paris"]
    zr[LIBGEO == "Marseille", lab2 := "Marseille"]
    zr[LIBGEO == "Lyon", lab2 := "Lyon"]
    zr[sample(1:nrow(zr),30,replace = FALSE), lab2 := LIBGEO]
    zr[price == min(price), lab2 := LIBGEO]
    zr[price == max(price), lab2 := LIBGEO]
    zr[area == min(area), lab2 := LIBGEO]
    zr[lab2 == "", lab2 := NA_character_]
    df = as.data.frame(zr)

    p1 = ggplot(df, aes(x = log(price) , y = log(pop/area), label = lab2)) + geom_point() + geom_smooth(method = "lm") + ggrepel::geom_text_repel(max.overlaps = 10, box.padding = 0.5)+ ggtitle("1975") + theme_bw() + scale_y_continuous("log density")
    p2 = ggplot(df, aes(x = log(price) , y = log(area), label = lab2)) + geom_point() + geom_smooth(method = "lm") + ggrepel::geom_text_repel(max.overlaps = 10, box.padding = 0.5)+ ggtitle("1975") + theme_bw() + scale_y_continuous("log area")
    list(p1,p2)
}

pra_1975_qreg <- function(){
    z = pra_1975()
    m = rq(log(area) ~ log(price), tau = c(0.05,0.1,0.25,0.5,0.9), z)
    m2 = rq(log(area) ~ log(price) + log(pop), tau = c(0.05,0.1,0.25,0.5,0.9), z)

    c1 = broom::tidy(m)
    c1 = tidyr::pivot_wider(c1[,c(1,2,5)],id_cols = "tau", values_from = "estimate", names_from = "term")
    c1$quantile = factor(c1$tau)

    p = ggplot(z, aes(x = log(price), y = log(area))) + geom_point()
    p = p + geom_abline(data = c1 , mapping = aes(intercept = `(Intercept)`, slope = `log(price)`, color = quantile), linewidth = 1) +
        ggtitle("1975: quantile regression") + theme_bw()
    p
}


# year 1990 ==============


#' terre labourable 1990 - libre a la vente
#'
#' read raw data from pdf at `statistique-agricole/petite-region-agricole/input/1990-labourables.pdf`
read_terre_labourable_1990 <- function(){
    z = fread(file.path(PRAdir(), "output","1990-labourables-cleaned.csv"), skip = 4, header = TRUE, select = c(1:3,7,8))
    setnames(z, c("region", "labou_1989","labou_1990","prairie_1989","prairie_1990"))

    # split region across two columns
    ispra <- z[ , grep("^\\d{3} - |^\\d{3}|\\d{3}|89 - VALLEE", region)]
    isdep <- z[ , grep("^\\d{2} - |^\\d{2} ", region)]
    z[ ispra , pra_code0 := stringr::str_extract(pattern = "[0-9]{3}", region)]
    z[ isdep , dep_code0 := stringr::str_extract(pattern = "[0-9]{2}", region)]

    # go down over each row adding the dep code to the pra code
    cur = z[1,dep_code0]
    for (i in 1:nrow(z)){
        if (is.na(z[i,dep_code0])){
            # do nothing and keep current value to fill in
        } else {
            cur = z[i,dep_code0]
        }
        z[i,dep := cur]
    }
    z[isdep, dep := dep_code0]

    z[!is.na(pra_code0),pra_code := paste0(dep,pra_code0)]
    # fix that one
    z[grep("89 - VALLEE",region),pra_code := "72089"]
    z[, price := as.numeric(gsub("\\!","1",gsub("I","1",gsub("\\*| |\u00a0|\\.| ","",labou_1990)))) ]
    # 2 values have a - sign because of visual issue in pdf
    z[, price := abs(price)]

    # vallee de la seine missing
    z[pra_code == "27332", price := labou_1989]

    z[,.(region,dep,pra_code, price)]
}


#' terre labourable 1999 - louee
#'
#' read raw data from pdf at `statistique-agricole/petite-region-agricole/input/1990-labourables-louee.pdf`
read_terre_labourable_louee_1990 <- function(){
    z = fread(file.path(PRAdir(), "output","1990-labourables-louees-cleaned.csv"), skip = 4, header = TRUE, select = c(1:3,7,8))
    setnames(z, c("region", "labou_1989","labou_1990","prairie_1989","prairie_1990"))

    # drop header rows
    z <- z[!(region == "" | region == "Régions\ndépartements" | region == "( petites ) régions agricoles")]

    # drop region headers
    z <- z[labou_1989 != ""]

    # split region across two columns
    ispra <- z[ , grep("^\\d{3} - |^\\d{3}|\\d{3}", region)]
    isdep <- z[ , grep("^\\d{2} - |^\\d{2} |^'\\d{2} - |^\\d{2}-", region)]
    z[ ispra , pra_code0 := stringr::str_extract(pattern = "[0-9]{3}", region)]
    z[ isdep , dep_code0 := stringr::str_extract(pattern = "[0-9]{2}", region)]



    # go down over each row adding the dep code to the pra code
    cur = z[1,dep_code0]
    for (i in 1:nrow(z)){
        if (is.na(z[i,dep_code0])){
            # do nothing and keep current value to fill in
        } else {
            cur = z[i,dep_code0]
        }
        z[i,dep := cur]
    }
    z[isdep, dep := dep_code0]

    z[!is.na(pra_code0),pra_code := paste0(dep,pra_code0)]
    z[, price := as.numeric(gsub("\\!","1",gsub("I","1",gsub("\\*| |\u00a0|\\.| ","",labou_1990)))) ]


    z[,.(region,dep,pra_code, price)]
}

#' Match Cities to Prices in 1990
#'
#' Similar to [pra_2000()], performs some manual assignment.
#' Adds Paris via [paris_pra_labour()].
pra_1990 <- function(){
    r = merge_results_PRA(topn = 200)
    r = r[rank >= 101]
    r <- r[year == 1990]
    tl = read_terre_labourable_1990()

    # first clean some entries in r or tl0
    r[CODGEO == "06083", c("PRA_Code", "PRA_Lib") := list("06250", "ALPES NICOISES")]
    r[CODGEO == "06088", c("PRA_Code", "PRA_Lib") := list("06250", "ALPES NICOISES")]
    r[CODGEO == "13103", c("PRA_Code", "PRA_Lib") := list("13246", "CRAU")]
    r[CODGEO == "13055", c("PRA_Code", "PRA_Lib") := list("13468", "COTEAUX DE PROVENCE")]

    r[CODGEO == "41018", c("PRA_Code", "PRA_Lib") := list("41068", "VALLEE ET COTEAUX DE LA LOIRE")] # Blois
    r[CODGEO == "41194", c("PRA_Code", "PRA_Lib") := list("41343", "GRANDE SOLOGNE")] # Romorantin-Lanthenay
    r[CODGEO == "44184", c("PRA_Code", "PRA_Lib") := list("44106", "ESTUAIRE DE LA LOIRE")]  # saint nazaire. https://draaf.pays-de-la-loire.agriculture.gouv.fr/valeur-venale-des-terres-en-pays-de-la-loire-r102.html
    r[CODGEO == "64102", c("PRA_Code", "PRA_Lib") := list("64138", "COTE BASQUE")]  # Bayonne
    r[CODGEO == "64445", c("PRA_Code", "PRA_Lib") := list("64380", "COTE BASQUE")]  # Pau
    r[CODGEO == "69123", c("PRA_Code", "PRA_Lib") := list("69194", "PLATEAUX DU LYONNAIS")] # "Lyon"

    r[CODGEO == "83050", c("PRA_Code", "PRA_Lib") := list("83468", "468 COTEAUX DE PROVENCE")]  # Draguignan
    r[CODGEO == "83061", c("PRA_Code", "PRA_Lib") := list("83468", "468 COTEAUX DE PROVENCE")]  # Frejus
    r[CODGEO == "83137", c("PRA_Code", "PRA_Lib") := list("83468", "468 COTEAUX DE PROVENCE")]  # Toulon

    r[CODGEO == "86194", c("PRA_Code", "PRA_Lib") := list("86338", "338 - RÉGION DES BRANDES")]
    r[CODGEO == "25462", c("PRA_Code", "PRA_Lib") := list("25450", "450 - PLATEAUX MOYENS DU JURA")]
    r[CODGEO == "27375", c("PRA_Code", "PRA_Lib") := list("27044", "044 - VEXIN NORMAND")]
    r[CODGEO == "27681", c("PRA_Code", "PRA_Lib") := list("27044", "044 - VEXIN NORMAND")]
    r[CODGEO == "57606", c("PRA_Code", "PRA_Lib") := list("57473", "473 - PLATEAU LORRAIN NORD")]
    r[CODGEO == "77305", c("PRA_Code", "PRA_Lib") := list("77335", "335 - BRIE CHAMPENOISE")]
    r[CODGEO == "04112", c("PRA_Code", "PRA_Lib") := list("84466", "466 - VAL DE DURANCE")]
    r[CODGEO == "57240", c("PRA_Code", "PRA_Lib") := list("57004", "004 - VALLEE DE LA MOSELLE")]

    z = merge(r[,!"PRA_Lib"],tl[,.(PRA_Lib = region,dep = as.numeric(dep), pra_code,price)], by.x = c("PRA_Code"), by.y = c("pra_code") ,all.x = TRUE, all.y = FALSE)
    ms = z[CODGEO %in% z[,.N ,by  = CODGEO][N>1,CODGEO]] #multiple pras per code
    ok = z[!(CODGEO %in% z[,.N ,by  = CODGEO][N>1,CODGEO])]

    z = rbind(ok, ms[,.(price = mean(price, na.rm = TRUE),pop_data = pop_data[1], area_data = area_data[1],rank = rank[1],PRA_Lib = PRA_Lib[1]),by = list(DEP,PRA_Code, CODGEO, LIBGEO, REG,year )], fill = TRUE)

    # fill Fontainebleau
    sm = get_seine_marne_1990()
    z[CODGEO == "77186" , price := sm[pra_code == "77064", price]]
    # fill Meaux
    z[CODGEO == "77284" , price := sm[pra_code == "77061", price]]


    z
}

#' For Seine et Marne area 1990
#'
#' For the Seine et Marne PRAs disaggregated sales data are missing in 1990
#' and we only observe rental values (*valeur louee*). We impute sales prices
#' by using the price to rent ratio at the departmental level. We get the
#' rental values from [read_terre_labourable_louee_1990()].
#'
#' @seealso [get_seine_marne_2000()]
get_seine_marne_1990 <- function(){
    # 1. compute ratio of each rented pra in seine and marne region to it's average
    t77 <- read_terre_labourable_louee_1990()[dep == "77"]
    t0 = t77[pra_code %in% c("77061","77064","77335","77336","77337")]

    # get ratio of each region to mean
    to_mean <- t0[,.(region,pra_code,dep,rent = price, factor = price / t77[is.na(pra_code), price])]

    # apply that ratio to known sales price
    s77 <- read_terre_labourable_1990()[dep == "77"]

    to_mean[, price := factor * s77[,price]]

    to_mean
}



# year 2000 ==================

# 2000 prices from PDF - labourable only

combine_pra_pdf_csvseries <- function(){
    x2 = pra_2000()
    x  = add_prices_PRA_2000()
    x3 = merge(x, x2[,.(price_francs = price, CODGEO)])

    fwrite(x3, file.path(PRAdir(),"area-pop-prices-2000-both.csv"))
    x3

}

#' PRA Arable Land prices for 2000
#'
#' Adds Paris via [paris_pra_labour()].
read_terre_labourable_2000 <- function(overwrite = FALSE,paris_pras = FALSE){
    if (overwrite) {
        tl = read_terre_labourable_1999_2000()

        # keep only pras
        tl0 = tl[!is.na(pra_code)]
        tl0[pra_code == "14353", price := tl0[dep == "14",mean(price,na.rm = TRUE)]] # pays d'auge missing

        # paris
        paris <- paris_pra_labour(tl0,year = 2000,keep_pras = paris_pras)
        tl0 = rbind(tl0, paris)

        # clean tl0 first
        tx = tl0[pra_code %in% c("12411" ,"22358", "26240", "32147", "35358" ,"67301", "68301" ,"72089")]
        tl0 <- tl0[!(pra_code %in% c("12411" ,"22358", "26240", "32147", "35358" ,"67301", "68301" ,"72089"))]
        tx <- tx[, .(region = region[1], dep = dep[1], price = mean(price,na.rm = TRUE)), by = pra_code]
        tl0 <- rbind(tl0,tx)
        data.table::fwrite(tl0, file = file.path(PRAdir(),"prix-labourables-2000.csv"))
        tl0

    } else {
        tl0 = data.table::fread(file = file.path(PRAdir(),"prix-labourables-2000.csv"), colClasses = list("character" = c("dep","pra_code")))
    }
    tl0

}

#' Match PRA Prices to Cities in 2000
#'
#' Manually associates PRAs to cities, where the mapping is incomplete or ambiguous.
#' Also takes care of multiple PRAs being associated to a single city via CODGEO,
#' in which case we take a simple average over all concerned PRA prices.
#'
#' Input data are from [read_terre_labourable_2000()]
#'
#' Writes output data to
#' `data/statistique-agricole/petite-region-agricole/area-pop-prices-2000-labourable.csv`
#'
#' Writes the list of cities with multiple PRAs to
#' `data/statistique-agricole/petite-region-agricole/area-pop-prices-2000-labourable-fixlist.csv`
pra_2000 <- function(){
    r = merge_results_PRA(topn = 200)
    r = r[rank >= 101]
    r <- r[year == 2000]

    # manually assign missing cities
    r[CODGEO == "06083", c("PRA_Code", "PRA_Lib") := list("06250", "ALPES NICOISES")]
    r[CODGEO == "06088", c("PRA_Code", "PRA_Lib") := list("06250", "ALPES NICOISES")]
    r[CODGEO == "13103", c("PRA_Code", "PRA_Lib") := list("13246", "CRAU")]
    r[CODGEO == "13055", c("PRA_Code", "PRA_Lib") := list("13468", "COTEAUX DE PROVENCE")]
    #
    r[CODGEO == "41018", c("PRA_Code", "PRA_Lib") := list("41068", "VALLEE ET COTEAUX DE LA LOIRE")] # Blois
    r[CODGEO == "41194", c("PRA_Code", "PRA_Lib") := list("41343", "GRANDE SOLOGNE")] # Romorantin-Lanthenay
    r[CODGEO == "44184", c("PRA_Code", "PRA_Lib") := list("44106", "ESTUAIRE DE LA LOIRE")]  # saint nazaire. https://draaf.pays-de-la-loire.agriculture.gouv.fr/valeur-venale-des-terres-en-pays-de-la-loire-r102.html
    r[CODGEO == "64445", c("PRA_Code", "PRA_Lib") := list("64380", "COTE BASQUE")]  # Pau
    r[CODGEO == "69123", c("PRA_Code", "PRA_Lib") := list("69194", "PLATEAUX DU LYONNAIS")] # "Lyon"

    r[CODGEO == "83050", c("PRA_Code", "PRA_Lib") := list("83468", "468 COTEAUX DE PROVENCE")]  # Draguignan
    r[CODGEO == "83061", c("PRA_Code", "PRA_Lib") := list("83468", "468 COTEAUX DE PROVENCE")]  # Frejus
    r[CODGEO == "83137", c("PRA_Code", "PRA_Lib") := list("83468", "468 COTEAUX DE PROVENCE")]  # Toulon

    r[CODGEO == "25462", c("PRA_Code", "PRA_Lib") := list("25450", "450 - PLATEAUX MOYENS DU JURA")]
    r[CODGEO == "27375", c("PRA_Code", "PRA_Lib") := list("27044", "044 - VEXIN NORMAND")]
    r[CODGEO == "27681", c("PRA_Code", "PRA_Lib") := list("27044", "044 - VEXIN NORMAND")]
    r[CODGEO == "57606", c("PRA_Code", "PRA_Lib") := list("57473", "473 - PLATEAU LORRAIN NORD")]
    r[CODGEO == "77305", c("PRA_Code", "PRA_Lib") := list("77335", "335 - BRIE CHAMPENOISE")]
    r[CODGEO == "04112", c("PRA_Code", "PRA_Lib") := list("84466", "466 - VAL DE DURANCE")]
    r[CODGEO == "86194", c("PRA_Code", "PRA_Lib") := list("86438", "438 REGION DES BRANDES")]
    r[CODGEO == "57240", c("PRA_Code", "PRA_Lib") := list("57004", "004 VALLEE DE LA MOSELLE")]

    tl = read_terre_labourable_1999_2000()
    z = merge(r[,!"PRA_Lib"],tl[,.(PRA_Lib = region,dep = as.numeric(dep), pra_code,price)], by.x = c("PRA_Code"), by.y = c("pra_code") ,all.x = TRUE)

    fixlist = data.table(CODGEO = c("69194",
                                    "13004",
                                    "41269",
                                    "44109",
                                    "62119",
                                    "62178",
                                    "64102"),
                         city = c("Lyon",
                                  "Arles",
                                  "Vendome",
                                  "Nantes",
                                  "Bethune",
                                  "Bruay-la-Buissière",
                                  "Bayonne"),
                         pras = c("69194|69199",
                                  "30471|13244|13468",
                                  "41074|41350|41341",
                                  "44373|44108|44105|44356",
                                  "62023", # taking Isbergues, Aire-sur-la-Lys
                                  "62023", # taking Isbergues, Aire-sur-la-Lys
                                  "40144|64143|64139"))
    fwrite(fixlist, file.path(PRAdir(),"area-pop-prices-2000-labourable-fixlist.csv"))
    fixprices = fixlist[ , .(pricef = tl[grep(pras,pra_code),mean(price)]), by = CODGEO]
    z = merge(z,fixprices, by = "CODGEO",all.x = TRUE)
    z[CODGEO %in% fixprices[,CODGEO], price := pricef]
    z[, pricef := NULL]
    z[,year := 2000]

    # there are a few with multiple PRAs per CODGEO
    ms = z[CODGEO %in% z[,.N ,by  = CODGEO][N>1,CODGEO]] #multiple pras per code
    ok = z[!(CODGEO %in% z[,.N ,by  = CODGEO][N>1,CODGEO])]

    z = rbind(ok, ms[,.(price = mean(price, na.rm = TRUE),pop_data = pop_data[1], area_data = area_data[1],rank = rank[1],PRA_Lib = PRA_Lib[1]),by = list(DEP,PRA_Code, CODGEO, LIBGEO, REG,year )], fill = TRUE)

    # fill Fontainebleau
    sm = get_seine_marne_2000()
    z[CODGEO == "77186" , price := sm[pra_code == "77064", price]]
    # fill Meaux
    z[CODGEO == "77284" , price := sm[pra_code == "77061", price]]

    # TODO nico
    # fill Bethune and Bruay-la-Buissière
    # price 32000

    # bring in new version of year 2000 price
    p2000 = PRA_2015_VVTDiffusion(yr = 2000)
    merge(z, p2000$merge[,.(PRA_Code,new_price_2000 = price,year = 2000)],
          by = c("PRA_Code","year"), all.x = TRUE)

}



#' terre labourable 2000 - libre a la vente
#'
#' this is year 2000 Sales prices for *terre labourable*.
#' read raw data from pdf at `statistique-agricole/petite-region-agricole/input/1999-2000-terre-labourables.pdf`
read_terre_labourable_1999_2000 <- function(){
    z = fread(file.path(PRAdir(), "output","1999-2000-terre-labourables-cleaned.csv"), skip = 4, header = TRUE, select = c(1:4,8,9))
    setnames(z, c("region", "region2", "labou_1999","labou_2000","prairie_1999","prairie_2000"))

    # split region across two columns
    ispra <- z[ , grep("[0-9]{3} .*|[0-9]{3}", region)]
    isdep <- z[ , grep("\\b[0-9]{2} - \\b|\\b[0-9]{2} \\b", region)]
    ispra2 <- z[ , grep("^[0-9]{1,2}$", region)]

    # zero pad
    z[ispra2, region := stringr::str_pad(region, pad = "0",side = "left", width = 3)]
    # tease out dep and pra codes from region string
    z[ union(ispra,ispra2) , pra_code0 := stringr::str_extract(pattern = "[0-9]{3}", region)]
    z[ isdep , dep_code0 := stringr::str_extract(pattern = "[0-9]{2}", region)]

    # if region2 string is full, overwrite region string with its content
    z[union(ispra,ispra2)][region2 != "", region := region2]

    # go down over each row adding the dep code to the pra code
    cur = z[1,dep_code0]
    for (i in 1:nrow(z)){
        if (is.na(z[i,dep_code0])){
            # do nothing and keep current value to fill in
        } else {
            cur = z[i,dep_code0]
        }
        z[i,dep := cur]
    }
    z[isdep, dep := dep_code0]

    z[!is.na(pra_code0),pra_code := paste0(dep,pra_code0)]
    z[, price := as.numeric(gsub("\\!","1",gsub("I","1",gsub("\\*| |\u00a0|\\.| ","",labou_2000)))) ]

    z[,.(region,dep,pra_code, price)]

}



#' terre labourable 2000 - louee
#'
#' from pdf at `statistique-agricole/petite-region-agricole/input/1999-2000-terre-labourables-louees.pdf`
read_terre_labourable_louee_1999_2000 <- function(){
    z = fread(file.path(PRAdir(), "output","1999-2000-terre-labourables-loues.csv"), skip = 3, header = TRUE, select = c(1:4,8,9))
    setnames(z, c("region1", "region2", "labou_1999","labou_2000","prairie_1999","prairie_2000"))

    # split region across two columns
    ispra <- z[ , grep("[0-9]{3} .*|[0-9]{3}", region1)]
    isdep <- z[ , grep("[0-9]{2} - ", region1)]
    z[ ispra , pra_code0 := stringr::str_extract(pattern = "[0-9]{3}", region1)]
    z[ isdep , dep_code0 := stringr::str_extract(pattern = "[0-9]{2}", region1)]

    # go down over each row adding the dep code to the pra code
    cur = z[1,dep_code0]
    for (i in 1:nrow(z)){
        if (is.na(z[i,dep_code0])){
            # do nothing and keep current value to fill in
        } else {
            cur = z[i,dep_code0]
        }
        z[i,dep := cur]
    }

    z[!is.na(pra_code0),pra_code := paste0(dep,pra_code0)]
    z[, price := as.numeric(gsub("\\!","1",gsub("I","1",gsub("\\*| |\u00a0|\\.| ","",labou_2000)) ))]

    # fill in region empty
    z[region2 == "" , region2 := region1]
    z[,region1 := NULL]
    setnames(z,"region2","region")

    z[,.(region,dep,pra_code, price)]
}

#' Seine et Marne PRA Prices from Departmental Price 2000
#'
#' For the Seine et Marne PRAs disaggregated sales data are missing in 2000
#' and we only observe rental values (*valeur louee*). We impute sales prices
#' by using the price to rent ratio at the departmental level. We get the
#' rental values from [read_terre_labourable_louee_1999_2000()].
get_seine_marne_2000 <- function(){
    # 1. compute ratio of each rented pra in seine and marne region to it's average
    t77 <- read_terre_labourable_louee_1999_2000()[dep == "77"]
    t0 = t77[pra_code %in% c(NA,"77061","77064","77335","77336","77337")]

    # get ratio of each region to mean
    to_mean <- t0[,.(region,pra_code,dep,rent = price, factor = price / t77[is.na(pra_code), price])]

    # apply that ratio to known sales price
    s77 <- read_terre_labourable_1999_2000()[dep == "77"]

    to_mean[, price := factor * s77[,price]]

    to_mean
}

#' Paris land value 1990 and 2000
#'
#' terre labourable only.
#'
#' This function produces a csv and tex output table in dropbox at location
#' `"~output/data/tables/paris-pra-prices-2000.csv"`, for example
#' We take all PRA directly adjacent to paris as well as CEINTURE DE PARIS as
#' on this map: https://driaaf.ile-de-france.agriculture.gouv.fr/IMG/pdf/RA_R11_cle4ce9b1.pdf
paris_pra_labour <- function(tl0,year = 2000,keep_pras = FALSE){

    if (year == 1990){
       sm =  get_seine_marne_1990()
    } else {
      sm = get_seine_marne_2000()
    }

    x = tl0[pra_code %in% isparis()]
    x = rbind(x, sm, fill = TRUE, use.names = TRUE)
    data.table::fwrite(x, file.path(outdatadir(),"tables",paste0("paris-pra-prices-",year,".csv")))

    x[,c("rent","factor") := NULL]

    kableExtra::kbl(x, booktabs = TRUE, format = "latex", align = "rccc") %>%
        readr::write_lines(file.path(outdatadir(),"tables",paste0("paris-pra-prices-",year,".tex")))

    if (keep_pras) {
        x[,.(pra_code = "75000", region = paste(region, pra_code) ,dep = "75",price)]
    } else {
        x[,.(pra_code = "75000", region = "Paris",dep = "75",price = mean(price))]

    }
}







# 1999+ from web interface : year 2015==============================

# note:
# the csv downloaded from the itneractive tableau is an average of land values
# with no distinction to what type of land is valued
# around paris we need a finer aggregation and for that we only have terre labourable - not the average mentioned above.


#' Add Prices to PRA+Results 1999+
#'
#' We use this function to assign 2015 PRA prices to cities.
#'
#' This uses data from interactive tableau
#' hence, it's an AVERAGE PRICE over all land types. Regarding the mapping,
#' this could be improved by relying on [PRA_xw()] instead of [get_PRA_result_name_1999()].
pra_2015 <- function() {
    r = merge_results_PRA(topn = 200)
    r = r[rank >= 101]
    r <- r[year == 2015]

    vvt = data.table(PRA_2015_VVT()$merge)
    z = merge(r,vvt[,.(PRA_Code,PRA_Libelle,price = euros_2_francs(price_euros))],all.y = FALSE, by = "PRA_Code")

    z[CODGEO == "46042", price := euros_2_francs(6590)]  # Cahors
    z[CODGEO == "51230", price := euros_2_francs(12035)]  # Epernay, avg of pays remoise and Champagne Crayeuse
    # out[CODGEO == "77186", price := 6380] # https://driaaf.ile-de-france.agriculture.gouv.fr/IMG/pdf/PRIX_GBP_2015_REGROUP_PRA_cle046232.pdf
    # out[CODGEO == "77284", price := 9140] # Meaux
    # out[CODGEO == "77305", price := 6380] # Montereau-Fault-Yonne
    z
}


#' Link PRA names to CODGEO in 1999+
#'
#' This function attemps to allocate cities to relevant PRA IDs.
#'
#' Should probably rely on [PRA_xw()] instead here.
#'
#' Auvergne Alpes Rhones map: https://draaf.auvergne-rhone-alpes.agriculture.gouv.fr/IMG/pdf/no9_Analyses_VVT_2015_enligne_cle87d384.pdf
get_PRA_result_name_1999 <- function(result,pra){
    # get name correspondance
    pn = unique(pra[,.(petitName,petitID), by = dep])
    rn = unique(result[,.(PRA_Lib   = PRA_Lib, LIBGEO, CODGEO),by = DEP])
    out = list()
    for (i in 1:nrow(rn)){
        id = rn[i]
        cand = pn[dep == id[,as.numeric(DEP)]]
        xn = apply(id[,.(PRA_Lib)], 1, function(x) grepl(x, cand[,petitName],ignore.case = TRUE))
        xi = which(xn)
        xx = cbind(id, cand[xi])

        out[[i]] = xx
    }
    o = rbindlist(out)

    # manual fixes
    o[CODGEO == "01053", c("dep", "petitID") := list("1","1")]
    o[CODGEO == "01283", c("dep", "petitID") := list("1","2")]   # Oyonnax. me: HAUT BUGEY. you: PAYS DE GEX HAUT-BUGEY
    o[CODGEO == "02408", c("dep", "petitID") := list("2","1")]
    o[CODGEO == "02691", c("dep", "petitID") := list("2","1")]
    o[CODGEO == "02722", c("dep", "petitID") := list("2","3")]
    o[CODGEO == "03185", c("dep", "petitID") := list("3","1")]
    o[CODGEO == "03190", c("dep", "petitID") := list("3","4")]
    o[CODGEO == "03310", c("dep", "petitID") := list("3","2")]
    o[CODGEO == "04112", c("dep", "petitID") := list("3","2")]

    o[CODGEO == "05061", c("dep", "petitID") := list("5","1")]
    o[CODGEO == "07010", c("dep", "petitID") := list("7","1")]
    o[CODGEO == "12145", c("dep", "petitID") := list("12","3")]
    o[CODGEO == "15014", c("dep", "petitID") := list("15","3")]
    o[CODGEO == "24037", c("dep", "petitID") := list("24","2")]
    o[CODGEO == "27229", c("dep", "petitID") := list("27","2")]
    o[CODGEO == "27375", c("dep", "petitID") := list("27","4")]
    o[CODGEO == "39198", c("dep", "petitID") := list("39","2")]
    o[CODGEO == "41269", c("dep", "petitID") := list("41","1")]
    o[CODGEO == "46042", c("dep", "petitID") := list("46","1")]  # zero value in id 3 (2015)04112
    o[CODGEO == "47323", c("dep", "petitID") := list("47","3")]
    o[CODGEO == "49023", c("dep", "petitID") := list("49","3")]
    o[CODGEO == "49099", c("dep", "petitID") := list("49","4")]
    o[CODGEO == "49301", c("dep", "petitID") := list("49","3")]
    o[CODGEO == "50502", c("dep", "petitID") := list("50","4")]
    o[CODGEO == "52121", c("dep", "petitID") := list("52","1")]
    o[CODGEO == "52448", c("dep", "petitID") := list("52","3")]
    o[CODGEO == "54323", c("dep", "petitID") := list("54","2")]
    o[CODGEO == "54329", c("dep", "petitID") := list("54","1")]
    o[CODGEO == "54329", c("dep", "petitID") := list("54","1")]

    o[CODGEO == "59392", c("dep", "petitID") := list("59","4")]
    o[CODGEO == "61001", c("dep", "petitID") := list("61","4")]
    o[CODGEO == "61006", c("dep", "petitID") := list("61","4")]
    o[CODGEO == "65286", c("dep", "petitID") := list("65","2")]
    o[CODGEO == "73008", c("dep", "petitID") := list("73","1")]
    o[CODGEO == "73011", c("dep", "petitID") := list("73","1")]
    o[CODGEO == "74010", c("dep", "petitID") := list("74","1")]
    o[CODGEO == "74012", c("dep", "petitID") := list("74","1")]
    o[CODGEO == "88160", c("dep", "petitID") := list("88","1")]
    o[CODGEO == "89387", c("dep", "petitID") := list("89","5")]
    o[CODGEO == "90010", c("dep", "petitID") := list("90","1")]
    o[CODGEO == "91223", c("dep", "petitID") := list("91","1")]
    o[CODGEO == "27681", c("dep", "petitID") := list("27","4")]
    o[CODGEO == "74281", c("dep", "petitID") := list("74","1")]
    o[CODGEO == "51230", c("dep", "petitID") := list("51","5")]


    o[CODGEO == "02691", c("dep", "petitID") := list("2","1")]
    o[CODGEO == "08105", c("dep", "petitID") := list("8","2")]
    o[CODGEO == "08409", c("dep", "petitID") := list("8","2")]
    o[CODGEO == "14118", c("dep", "petitID") := list("14","6")]
    o[CODGEO == "14366", c("dep", "petitID") := list("14","7")]
    o[CODGEO == "16015", c("dep", "petitID") := list("16","2")]
    o[CODGEO == "17299", c("dep", "petitID") := list("17","1")]
    o[CODGEO == "31555", c("dep", "petitID") := list("31","3")]
    o[CODGEO == "31555", c("dep", "petitID") := list("31","3")]
    o[CODGEO == "33063", c("dep", "petitID") := list("33","6")]
    o[CODGEO == "35288", c("dep", "petitID") := list("35","3")]
    o[CODGEO == "42187", c("dep", "petitID") := list("42","1")]
    o[CODGEO == "42207", c("dep", "petitID") := list("42","2")]
    o[CODGEO == "42218", c("dep", "petitID") := list("42","2")]
    o[CODGEO == "44184", c("dep", "petitID") := list("44","2")]
    o[CODGEO == "47001", c("dep", "petitID") := list("47","3")]
    o[CODGEO == "53130", c("dep", "petitID") := list("53","2")]
    o[CODGEO == "54395", c("dep", "petitID") := list("54","1")]
    o[CODGEO == "64445", c("dep", "petitID") := list("64","2")]
    o[CODGEO == "64445", c("dep", "petitID") := list("65","1")]
    o[CODGEO == "65440", c("dep", "petitID") := list("65","1")]
    o[CODGEO == "66136", c("dep", "petitID") := list("66","1")]
    o[CODGEO == "69123", c("dep", "petitID") := list("69","3")]
    o[CODGEO == "72181", c("dep", "petitID") := list("72","7")]
    o[CODGEO == "72181", c("dep", "petitID") := list("72","3")]
    o[CODGEO == "73065", c("dep", "petitID") := list("73","2")]

    # paris
    o[CODGEO == "75060", c("dep", "petitID") := list("78","1")]
    o[CODGEO == "75060", c("dep", "petitID") := list("95","1")]
    # o[CODGEO == "75060", c("dep", "petitID") := list("51","4")]
    o[CODGEO == "75060", c("dep", "petitID") := list("91","1")]
    o[CODGEO == "75060", c("dep", "petitID") := list("77","1")]

    # nice
    o[CODGEO == "06088", c("dep", "petitID") := list("6","3")]  # zero value

    # marseille: assigned COTEAUX DE PROVENCE
    o[CODGEO == "13055", c("dep", "petitID") := list("13","4")] # petitName == "LITTORAL DE PROVENCE" has a zero price

    o[CODGEO == "81004", c("dep", "petitID") := list("81","3")]
    o[CODGEO == "81065", c("dep", "petitID") := list("81","2")]
    o[CODGEO == "82121", c("dep", "petitID") := list("82","3")]
    o[CODGEO == "86066", c("dep", "petitID") := list("86","1")]
    o[CODGEO == "64102", c("dep", "petitID") := list("64","1")]   # assumption
    o[CODGEO == "83137", c("dep", "petitID") := list("83","4")]   # toulon: coteau de provence
    o[CODGEO == "47157", c("dep", "petitID") := list("47","3")]   #  Marmande
    o[CODGEO == "49092", c("dep", "petitID") := list("49","5")]   # Chemillé-en-Anjou
    o

}


namecomb <- function(s){
    n = length(s)
    if (n == 1){
        return(s)
    } else {
        paste(s[1],
              s[2],
              paste(s[3:n],collapse = " "),
              sep = ":")
    }
}

colsplit <- function(z){
    nr = strsplit(z, split = " - ")
    x = lapply(nr, function(x) {grepl(pattern = "[[:digit:]]",x)})
    regid = unlist(lapply(x, function(x) !any(x)))
    depr = nr[unlist(regid)]
    depregr = data.frame(reg = unlist(lapply(depr, function(c) c[[1]])), dep =  unlist(lapply(depr, function(c) trimws(c[[2]]))))

    petitid = unlist(lapply(x, any))
    petitnames = lapply(nr[petitid], function(x) paste(x, collapse = " "))
    petitnames = lapply(petitnames, function(x) namecomb(unlist(strsplit(x, split = " "))))

    newnames = rep("", times = length(nr))
    newnames[regid] <- depregr$dep
    newnames[petitid] <- unlist(petitnames)

    # get numbers out of names string
    # s = strsplit(newnames[-1],split = " ")
    #
    #
    # r = regmatches(newnames,gregexpr("[[:digit:]]+", newnames))
    # rid = unlist(lapply(r, function(x) length(x)>0))
    # notrid = unlist(lapply(r, function(x) length(x)==0))
    # r[rid] <- lapply(r[rid], function(x) pastepaste(c("dep","petitID"), x, sep = "_"))
    newnames
}


# prices by petite region agricole

#' petite region agricole 1999-2020
#'
#' Refernce: https://agreste.agriculture.gouv.fr/agreste-web/methodon/Z.1/!searchurl/listeTypeMethodon/
#'
#' function is deprecated as we use [PRA_xw()] now.
#'
#' data is from https://agreste.agriculture.gouv.fr/agreste-web/disaron/V_0003/detail/
#' same data in nicer format here `data/statistique-agricole/petite-region-agricole/input/DIFFUSION_VVT_2020_site_DRIAAF_cle0d516d.xlsx`
read_pra_agreste <- function(overwrite = FALSE){
    if (overwrite){
        x = readxl::read_xlsx(file.path(PRAdir(), "input","DIFFUSION_VVT_2020_site_DRIAAF_cle0d516d.xlsx"),
                                sheet = 4, skip = 4,na = "NS")

        # filter columns
        x = x[ , -((ncol(x)-2):ncol(x))]

        # rename column 1
        names(x)[1] <- "region"

        setDT(x)

        # region starting with a digit is a PRA, else it's a Departement
        pras = x[grepl(x$region, pattern = "^\\d"), ]
        rsp = strsplit(pras$region, split = " ")
        pras[, DEP := unlist(lapply(rsp, function(x) stringr::str_pad(x[1], width = 2, side = "left",pad = "0"))) ]
        pras[, PRAid := unlist(lapply(rsp, function(x) x[2])) ]
        pras[, PRAname := unlist(lapply(rsp, function(x) paste(x[3:length(x)], collapse = " "))) ]
        pras[, region := NULL]
        mp = melt.data.table(pras)
        setnames(mp, "variable", "year")
        mp[, year := as.integer(as.character(year))]
        data.table::fwrite(mp, file = file.path(PRAdir(),"pra-agreste-1999-2020.csv"))
    } else {
        mp = data.table::fread(file = file.path(PRAdir(),"pra-agreste-1999-2020.csv"),colClasses = c("DEP" = "character"))
    }
    mp
}

#' Merge PRA Referentiel with Agreste PRA Prices
#'
#' Want to associate a unique PRA_code to each row in Agreste Prices
#'
#' Attempt to automatically perform match. function deprecated, as we use the manual
#' matching provided in `PRA_xw`.
merge_agreste_pra_ref <- function(){
    agr = read_pra_agreste()
    agr[,PRAname_match := gsub(pattern = " DE LA | DE | DU | SUR | EN | LE | DES | ET ",replacement = " ", PRAname)]
    agr[,PRAname_match := gsub(pattern = " - |-| ",replacement = "|", PRAname_match)]

    iagr = unique(agr[,.(DEP,PRAname_match)])


    ref = load_PRA()
    ref = ref[!(DEP %in% c("2A","2B"))]
    iref = unique(ref[,.(PRA_Code,DEP,PRA_Name)])   # keymap

    iref[, PRA_Name2 := NA_character_]

    iref[ , PRA_Name2 := pra_matcher(PRA_Name,iagr[DEP == .SD[,DEP],PRAname_match]), by = PRA_Code]

    nomatches = list()
    idx = iref[,sum(is.na(PRA_Name2))>0,by = DEP]
    for (id in idx[(V1),DEP]){
        nomatches[[id]] <- list(ref = iref[is.na(PRA_Name2)][DEP == id],
                                prices = iagr[DEP==id]
        )
        pa = file.path(PRAdir(),"input","missing-matches",id)
        dir.create(pa, showWarnings = FALSE)
        data.table::fwrite(nomatches[[id]]$ref, file = file.path(pa,"pra-put.csv"))
        data.table::fwrite(nomatches[[id]]$prices, file = file.path(pa,"pra-take.csv"))
    }

    # merge back into price data
    ma = merge(agr, iref[,.(DEP,PRA_Code,PRA_Name2)], by.x = c("DEP","PRAname_match"), by.y = c("DEP","PRA_Name2"), allow.cartesian = TRUE)

    list(ma,iref, nomatches)

}

pra_matcher <- function(pname, candidates){
    if (length(candidates) > 0) {
        for (ix in 1:length(candidates)) {
            o = grep(x = pname, pattern = candidates[ix])
            if (length(o) > 0){
               return(candidates[ix])
            }
        }
    } else {
        NA
    }
}

#' petite region agricole 1990+
#'
#' this price data covers 1999 onwards. It has an incomplete mapping to PRA_Code,
#' which complicates the merge to city data. We first attempt to merge the majority
#' by department and name, and then fix manually for each city and year the missing
#' prices (not in this function).
#'
#' data is from https://agreste.agriculture.gouv.fr/agreste-web/disaron/V_0003/detail/
#' same data in nicer format here `data/statistique-agricole/petite-region-agricole/input/DIFFUSION_VVT_2020_site_DRIAAF_cle0d516d.xlsx`
read_petite_1999 <- function(){
    z = fread(file.path(PRAdir(), "output","PRA-terres-1999-2018.csv"), skip = 5, sep = ";")
    for_rentn = grepl(pattern = "Prix moyen des terres et prés loués", names(z))
    for_rent = z[, .SD, .SDcols = c(1,(1:ncol(z))[for_rentn])]
    for_sale = z[, .SD, .SDcols = (1:ncol(z))[!for_rentn]]

    setnames(for_rent, gsub(pattern = "Prix moyen des terres et prés loués \\(euro courant \\/ ha\\)\\/","", names(for_rent)))
    setnames(for_rent, 1, "year")

    setnames(for_sale, gsub(pattern = "Prix moyen des terres et prés libres de plus de 70 ares \\(euro courant \\/ ha\\)\\/","", names(for_sale)))
    setnames(for_sale, 1, "year")

    # split colnames

    newr <- colsplit(names(for_rent)[-1])
    news <- colsplit(names(for_sale)[-1])

    setnames(for_rent, -1, newr)
    setnames(for_sale, -1, news)

    # now long format
    for_rent = melt(for_rent, id.vars = "year")
    for_sale = melt(for_sale, id.vars = "year")

    # names
    for_rent[ ,c("dep","petitID","petitName") := tstrsplit(variable,split = ":")]
    for_sale[ ,c("dep","petitID","petitName") := tstrsplit(variable,split = ":")]
    for_rent[ , variable := NULL]
    for_sale[ , variable := NULL]

    list(sale = for_sale, rent = for_rent)

    # ggplot(x[[1]][!is.na(petitID), list(mean(value),sd(value)),by = year], aes(x = year, y = V1)) + geom_line() + geom_ribbon(aes(y = V1, ymin = V1-V2, ymax = V1+V2),alpha = 0.4)+ggtitle("Prix des terres en Euros courants",subtitle = "mean +- standard deviation")
}


#' Paris Land Values 2015
#'
#' values are an AVERAGE over all land types - not labourable vs vignes etc
#'
#' we collect those from the map at https://driaaf.ile-de-france.agriculture.gouv.fr/IMG/pdf/PRIX_GBP_2015_REGROUP_PRA_cle046232.pdf
paris_prices_2015 <- function(keep_pras = FALSE) {
    p = data.table(price = c(7980,11030,13890, 10890, 9140,6380,10640),
                   LIBGEO = c("9:00","0:00","1:00","3:00","3:05","5:30","6:30"),
                   PRA_Lib = paste("paris_",1:7),
                   DEP = "75",
                   PRA_Code = "75000",
                   rank = 1,
                   year = 2015,
                   CODGEO = "75060")
    data.table::fwrite(p, file.path(outdatadir(),"tables","paris-pra-prices-2015.csv"))
    if (keep_pras){
        p
    } else {
        data.table(CODGEO = "75060", LIBGEO = "PARIS", DEP = "75",year = 2015,price = p[,mean(price)], PRA_Lib = "PARIS")
    }

}












# Mapping Facilities ==================

#' map data of PRA
#'
#' produce a mapview map of all PRAs in France
mapdata_pra <- function(overwrite = FALSE) {
    if (overwrite){
        pra = load_PRA()

        # load a communes shapefile
        sh = sf::read_sf(file.path(datadir(),"communes-20150101-100m-shp","communes-20150101-100m.shp"))

        # merge
        sp = sh %>%
            dplyr::right_join(pra %>%
                                  dplyr::filter(!(DEP %in% c("2A","2B"))),
                              by = c("insee" = "CODGEO")) %>%
            dplyr::group_by(PRA_Code,PRA_Lib) %>%
            dplyr::summarise(geometry = sf::st_union(geometry, is_coverage = TRUE),n = dplyr::n(), surf_m2 = sum(surf_m2, na.rm = TRUE), DEP = DEP[1])

        saveRDS(sp,file = file.path(PRAdir(), "PRA-mapdata.Rds"))
        sp

    } else {
        readRDS(file.path(PRAdir(), "PRA-mapdata.Rds"))
    }
}


# Neighboring Area Prices Correspondance ====================

#' Get PRA adjancency correspondance
#'
#' we want a list of neighbors for each PRA
pra_neighbors <- function(){
    m = mapdata_pra() %>% dplyr::ungroup()
    d = data.table(n = nrow(m),step = "All PRAs")
    m = m[!sf::st_is_empty(m), ]
    d = rbindlist(list(d,
                  data.table(n = nrow(m),step = "PRAs with valid geometry")))

    ids = unique(m$PRA_Code)
    touches = m[,1] %>% sf::st_touches(., m[,1])

    list(data = m %>% mutate(touched_by = lapply(lapply(touches, unlist), function(x) ids[x])),
         obs = d)
}

pra_get_neighbor_prices <- function(){
    p = rbindlist(all_pra_prices(), use.names = TRUE)     # all years
    p = p[!is.na(price)]


    # neighbor correspondance
    nx = pra_neighbors()
    n = nx$data %>% data.table
    n = n[,.(PRA_Code,touched_by)]

    pn = merge(p,n, by.x = "pra_code",by.y = "PRA_Code")


    nx$obs <- rbindlist(list(nx$obs,
                        data.table(n = nrow(pn),step = paste0("Merge with Price-Year Data (",nrow(p)," prices)"))))
    pnn = pn[, .(price_neighbors =
                    pn[pra_code %in% .SD[,touched_by[[1]]] * year == .BY[["year"]],
                       mean(price,na.rm = TRUE)]),by = .(pra_code,year)]

    z = merge(pn,pnn,by = c("pra_code","year"))
    nx$obs <- rbindlist(list(nx$obs,
                        data.table(n = nrow(z),step = "With Valid Neighboring PRAs")))
    nx$obs <- rbindlist(list(nx$obs,
                        data.table(n = sum(complete.cases(z[,.(price_neighbors)])),
                                   step = "With Nonmissing Neighbor Prices")))


    list(data = z, obs = nx$obs)
}


#' PRA Prices: Neighbor IV Regression
#'
#' this function performs 2SLS with neighboring PRA prices
#' as an IV for own PRA price.
pra_IV_neighbors <- function(){
    x = pra_stack()


    gn = pra_get_neighbor_prices()
    gn$obs <- rbindlist(list(gn$obs,
                             data.table(n = nrow(x),
                                        step = "City-Year Observations")))
    xn = merge(x,gn$data[,.(price_neighbors,year,pra_code)], by.x = c("year","PRA_Code"), by.y = c("year","pra_code"))
    gn$obs <- rbindlist(list(gn$obs,
                             data.table(n = sum(complete.cases(xn[,.(density,logmean_netsalaire, price, price_neighbors)])),
                                        step = "With Nonmissing Data")))
    o1 = xn[,sum(is.na(logmean_netsalaire)),by = year]
    setnames(o1, c("year","V1"), c("step", "n"))
    o1[, step := paste("netsalaire missing in",step)]
    gn$obs <- rbindlist(list(gn$obs,o1),use.names = TRUE)

    # take care of some cities with multiple obs per year
    # xn[,.N,by = CODGEO][N > 4]
    xn = unique(xn)

    l = list()
    l[["IV0"]] <- fixest::feols(log(density) ~ 0 | REG + year | log(price) ~ log(price_neighbors) , data = xn)
    l[["IV1"]] <- fixest::feols(log(density) ~ logmean_netsalaire | REG + year | log(price) ~ log(price_neighbors) , data = xn)
    # fixest::etable(l[["IV"]],stage = 1:2, file = file.path(outdatadir(),"tables", paste0("IV-density-price.tex")))

    l[["IV2"]] <- fixest::feols(log(density) ~ logmean_netsalaire | year | log(price) ~ log(price_neighbors) , data = xn)

    fixest::etable(l,stage = 1:2,
                   file = file.path(outdatadir(),"tables", paste0("IV-density-neighbors.tex")),
                   replace = TRUE,
                   label = "tab:IV-regression-neighbors",
                   title = "Three 2SLS Models",
                   digits.stats = "r3")


    # options("modelsummary_format_numeric_latex" = "plain")
    #
    # modelsummary::modelsummary(summary(l[["IV"]], stage = 1:2),
    #                            gof_omit = "R2 \\w*|AIC|BIC|",output = "latex",
    #                            stars = TRUE) %>%
    #     readr::write_lines(file.path(outdatadir(),"tables", paste0("IV-density-price.tex")))
    l[["obs"]] <- gn$obs
    l[["data"]] <- xn
    ta = knitr::kable(gn$obs, format = "latex",
                      booktabs = TRUE,
                      caption = "PRA Neighbors Sample Construction",
                      label = "pra-neighbor-sample"
    ) %>%
        readr::write_lines(file.path(datatables(), "IV-neighbors-sample.tex"))

    # cat(kableExtra::landscape(ta), file = file.path(datatables(), "france-pop-1950.tex"))
    l
}


make_AU2010_shapefile <- function(){
    sh = sf::read_sf(file.path(datadir(),"communes-20150101-100m-shp","communes-20150101-100m.shp"))
    au = getAU()

    as = sh %>%
        inner_join(au, by = c("insee" = "CODGEO")) %>%
        rename(CODGEO = insee)
    sh2 = as %>%
        filter(!(grepl("997|998|000|\\d[A-Z]\\d", AU2010 ))) %>%
        group_by(AU2010) %>%
        summarise(surf_m2 = sum(surf_m2), LIBAU2010 = LIBAU2010[1], DEP = DEP[1],REG = REG[1], geometry = sf::st_union(geometry, is_coverage = TRUE))
    sf::st_write(sh2, file.path(dboxdir(),"..","housing-fertility","empirics","France","uu2020_2022","AU2010.shp"))
    sh2



}


notheme <- function(){
    theme(
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    # text = element_blank(),
    plot.background = element_blank())
}



map_pra_prices2000_data <- function(overwrite = FALSE){

    if (overwrite){
        a = read_terre_labourable_2000(overwrite = overwrite)
        ms = a[pra_code %in% a[,.N,by=pra_code][N>1, pra_code]]
        ok = a[!(pra_code %in% a[,.N,by=pra_code][N>1, pra_code])]
        ms = ms[,.(region = region[1], dep = dep[1], price = mean(price,na.rm = TRUE)), by = pra_code]
        a = rbind(ok,ms)

        pra = load_PRA()
        pa = merge(pra,a, by.x = "PRA_Code", by.y = "pra_code")
        pa = pa[!(DEP %in% c("2A","2B"))]

        # load a communes shapefile
        sh = sf::read_sf(file.path(datadir(),"communes-20150101-100m-shp","communes-20150101-100m.shp"))

        # merge
        sp = sh %>%
            dplyr::right_join(pa ,
                              by = c("insee" = "CODGEO")) %>%
            dplyr::group_by(PRA_Code,PRA_Lib) %>%
            dplyr::summarise(geometry = sf::st_union(geometry, is_coverage = TRUE),
                             n = dplyr::n(),
                             surf_m2 = sum(surf_m2, na.rm = TRUE),
                             DEP = DEP[1],
                             price = mean(price, na.rm = TRUE))

        aj = sh %>%
            dplyr::anti_join(pa,by = c("insee" = "CODGEO"))

        sm = get_seine_marne_2000() # prices of seine et marne
            sm77 = aj %>%
                dplyr::filter(grepl("^77", insee)) %>%
                dplyr::summarise(geometry = sf::st_union(geometry, is_coverage = TRUE),
                                 n = dplyr::n(),
                                 surf_m2 = sum(surf_m2, na.rm = TRUE),
                                 DEP = "77",
                                 price = sm[,mean(price)],
                                 PRA_Code = "77061",
                                 PRA_Lib = "Seine et Marne")
        sp = rbind(sp,sm77)



        # load a dept shapefile
        cbreaks = quantile(sp$price, probs = c(0,0.1,0.5,0.7,0.9,0.99,0.999), na.rm = T)
        cuts = classInt::classIntervals(sp$price, style = "fixed", n = length(cbreaks), fixedBreaks = cbreaks)
        sp = sp %>% dplyr::mutate(price_cat = cut(price,cuts$brks,dig.lab = 6))

        sp = sp[!sf::st_is_empty(sp), , drop = FALSE]
        saveRDS(sp, file = file.path(PRAdir(),"pra-shapes-2000.Rds"))
        sf::st_write(sp, file = file.path(PRAdir(),"pra-shapes-2000.shp"))

    } else {
        sp = readRDS(file.path(PRAdir(),"pra-shapes-2000.Rds"))
    }
    sp
}

#' Map PRA prices and City locations
#'
#' for year 2000
map_pra_prices2000 <- function(wi = 10, he = 7){
    sp = map_pra_prices2000_data()
    # ggplot map
    basemap = ggplot() + geom_sf(data = sp, aes(fill = price_cat), lwd = 0.1) + scale_fill_brewer(name = "Francs per hectare",palette = "YlOrRd")

    cities = map_cities(TLS = TRUE, sf::st_crs(sp))

    ti = ggtitle("Price per Hectare of Arrable Land (`Terre Labourable`) in 2000")

    #

    TLSmap = basemap + geom_sf(data = cities[[1]]) + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data =cities[[2]],arrow = arrow(), lineend = "round", size = 2) + notheme() + ti

    map = basemap + geom_sf(data = cities[[1]])  + notheme() + ti
    map_noti = basemap + geom_sf(data = cities[[1]])  + notheme() + theme(legend.position = c(0.15,0.32))

    ggsave(plot = TLSmap, filename = file.path(outdatadir(),"plots", "map-prices-TLS.pdf"),width = wi, height =he)
    ggsave(plot = map_noti, filename = file.path(outdatadir(),"plots", "figureA23.pdf"),width = wi, height =he)
    # ggsave(plot = map, filename = file.path(outdatadir(),"plots", "map-prices.pdf"),width = 10, height =7)
    ggsave(plot = map, filename = file.path(outdatadir(),"plots", "map-prices-square.pdf"),width = wi, height = wi + 2)

    mv = mapview::mapview(sp, zcol = "price_cat") + mapview::mapview(cities[[1]])

    list(TLSmap,map,mv,map_noti)

}

map_cities <- function(TLS = FALSE,crs = NULL){
    bb = bboxes_top200()
    cities = sf::st_read(file.path(datadir(), "IGN-chef-lieux", "ign_metropole_adminexpress_chefs_lieux_z.shp")) %>%
        filter(insee_com %in% bb[,CODGEO])

    paris = sf::st_as_sf(data.frame(x = 2.349020, y = 48.853481, gid = NA, id = NA, nom_chf = "Notre Dame", statut = NA, insee_com = "75056"),coords = c("x","y"), crs = 4326) %>%
        sf::st_transform(sf::st_crs(cities))
    cities = rbind(cities,paris)
    offmap = c("06083","06088","10029","83137","13055","10015")
    offcities = cities %>%
        filter(insee_com %in% offmap)
    dirs = c(rep(sf::st_point(c(0, 10000)),2), sf::st_point(c(-10000, 10000)), rep(sf::st_point(c(0, 10000)),3))
    for (i in 1:nrow(offcities)){
        offcities$geometry[[i]] <- offcities$geometry[[i]] + dirs[i]
    }
    cities[cities$insee_com %in% offmap,]$geometry <- offcities$geometry

    # TLS <- cities %>% filter(insee_com == "31555")
    # endp = TLS$geometry[[1]] - sf::st_point(c(12000,0))
    # startp = endp - sf::st_point(c(300000,40000))
    # arrowd= data.frame(x1 = startp[[1]], x2 = endp[[1]], y1 = startp[[2]], y2 = endp[[2]])

    if (TLS){
        stopifnot(!is.null(crs))
        TLS <- cities %>% filter(insee_com == "31555") %>% sf::st_transform(crs)
        endp = TLS$geometry[[1]] - sf::st_point(c(0.1,0.1))
        startp = endp - sf::st_point(c(1,1))
        arrowd= data.frame(x1 = startp[[1]], x2 = endp[[1]], y1 = startp[[2]], y2 = endp[[2]])
        list(cities,arrowd)
    } else {
        cities
    }
}



export_citylists <- function(){
    x = read_output()
    fl = get_flat_list(200)
    fwrite(x[1:100, .(CODGEO, LIBGEO)], "~/Downloads/CASD100.csv")
    fwrite(fl[year == 1975, .(CODGEO, LIBGEO)], "~/Downloads/CASD200.csv")
}


#' Collect All PRA prices in All years
#'
#' we use this function to get prices for all available PRAs
#' in the country, i.e. not matching to our 200/100 city samples.
#' we use those prices for the neighborhood IV exercise and for plotting
#' purposes.
all_pra_prices <- function(yr1950 = 1950,readdisk = FALSE){

    if (readdisk){
        o = readRDS(file = file.path(datadir(),"pra-all-prices.Rds"))
    } else {
        o = list()

        # 1950s
        p = clean_pra_value_1950()
        par = paris_pra_1950s_labour(p,year = yr1950)
        z = rbind(p,par, use.names = TRUE, fill = TRUE)
        o[["1950"]] <- z[year == yr1950]
        o[["1950"]][,price_euros := francs_2_euros(price)]


        # 1975
        z = clean_pra_value_1975()
        paris = paris_pra_1975_labour(z)
        z = rbind(z,paris, fill = TRUE)
        z[,year := 1975]
        z = z[,.(region,pra_code,year,dep = dep_code,price = lab_dominante_1975)]
        o[["1975"]] <- z
        o[["1975"]][,price_euros := francs_2_euros(price)]

        # 1990
        tl = read_terre_labourable_1990()
        # sm = get_seine_marne_1990()
        # tl = rbind(tl, sm[,.(region,dep,pra_code,price)])
        paris <- paris_pra_labour(tl,year = 1990)
        tl0 = rbind(tl, paris)
        # keep only pras
        tl0[,year := 1990]
        o[["1990"]] = tl0[!is.na(pra_code)]
        o[["1990"]][,price_euros := francs_2_euros(price)]

        # 2000
        o[["2000"]] <- read_terre_labourable_2000()
        o[["2000"]][,year := 2000]
        o[["2000"]][,price_euros := francs_2_euros(price)]



        # 2015
        z = PRA_2015_VVT()
        z = z$merge
        data.table::setDT(z)
        z = z[,.(pra_code = PRA_Code,region = PRA_Lib,dep = Département, year = 2015, price_euros , price = euros_2_francs(price_euros))]

        o[["2015"]] <- z

        saveRDS(o,file = file.path(datadir(),"pra-all-prices.Rds"))
        o


    }



}


# Regressions, Stacking Data and Scatter Plots ==========

getAU <- function(){
    x = data.table(readxl::read_xlsx(file.path(datadir(),"AU2010_au_01-01-2020.xlsx"),skip = 5, sheet = "Composition_communale"))
    x[,.(CODGEO,AU2010, LIBAU2010)]
}

#' Stack City and PRA Data for All Years
#'
#' Main PRA output function. Builds dataset with different row count
#' based on how many cities and whether one wants Paris PRAs disaggregated or not.
#'
#' @param paris_pras Boolean TRUE/FALSE to disaggregate parisian PRAs or not
#' @param topn integer 100 or 200 largest cities to consider
#' @param overwrite Boolean TRUE/FALSE whether to overwrite or read from disk
#'
#' @details
#' How does the mapping from PRA prices to cities work? For years up to and
#' and including 2000, we can rely on a 1:1 mapping of `PRA_Code` which maps
#' city `CODGEO` to `PRA_Code`. Year 2015 does not have this mapping,
#' so we first match by department and name and then manually fix the missing data
#' for each city in function [get_PRA_result_name_1999()].
#'
#' ## Function Contents
#'
#' * Load PRA Data for each year
#' * stack all years
#' * return from function if we are checking with `paris_pra = TRUE`
#' * otherwise continue:
#' * keep only ranks `topn`
#' * merge AOC classification by `CODGEO`
#' * price in Euros
#' * merge AU classification by `CODGEO`
#' * merge Urban wage data by `AU2010` (from CASD)
#' * compute rank by population of 1975 to get popweights
#'
#' ### Load PRA Data for each year
#'
#' The function has 5 functions that create data (4 in the `topn=200` sample).
#' Additionally the user can choose to output Paris PRAs in a disaggregated way
#' for checking with argument `paris_pra`.
#'
#' 1. [pra_1950()]
#' 2. [pra_1975()]
#' 3. [pra_1990()]
#' 4. [pra_2000()]
#' 5. [add_prices_PRA()] with `yr = 2015` argument.
#'
#' Please refer to the documentation of each for details.
#'
#' @export
pra_stack <- function(topn = 100, overwrite = FALSE) {
    if (overwrite) {
        if (topn == 100){
            r = export_pop_area(topn = 100)  # area and population data
            # fix r years
            r[year == 1954, year := 1950]
            r[year == 1999, year := 2000]
            setkey(r,CODGEO,year)
            l = merge(r, pra_cleaned()[,!"LIBGEO"], by = c("CODGEO","year"))

        } else if (topn == 200) {
            L = list()
            L[[1]] <- pra_1975()
            L[[2]] <- pra_1990()
            L[[3]] <- pra_2000()
            L[[4]] <- pra_2015()
            l = rbindlist(L, fill = TRUE)
            l[,c("lab2","pra_lib","dep") := NULL]
            l[is.na(density_data), density_data := pop_data / area_data]
        } else {
            error("topn is either 100 or 200.")
        }

        l[, CODGEO := stringr::str_pad(CODGEO, width = 5, side = "left", pad = "0")]
        l[, PRA_Code := stringr::str_pad(PRA_Code, width = 5, side = "left", pad = "0")]

        # drop Frejus because it's full of holiday homes
        l = l[CODGEO != "83061"]

        # enforce max rank
        l = l[rank <= topn]

        # merge AU classification
        l = merge(l, getAU(), all.x = TRUE, by = "CODGEO")
        l[CODGEO == "75060", AU2010 := "001"]

        # merge urban productivity by AU data
        casd = data.table::fread(file.path(CASDdir(),"2022-07-08","tables","panelEDP2019-avgsalary-au.csv"), colClasses = list("character" = "AU2010"))
        # fix years
        casd[year == 1976, year := 1975]
        casd[year == 1991, year := 1990]

        l = merge(l, casd, all.x = TRUE, all.y = FALSE, by = c("AU2010", "year"))

        if (topn == 200){
            t100 = pra_stack()
            l = rbind(l, t100[year > 1950], fill = TRUE, use.names = TRUE)  # add the top 100
        }

        fwrite(l[,!"PRA_Lib"], file.path(PRAdir(),paste0("pra-output-",topn,".csv")))
        # differences
    } else {
        l = fread(file.path(
                PRAdir(),paste0("pra-output-",topn,".csv")),
                colClasses = list("character" = c("CODGEO","AU2010","PRA_Code","DEP","REG")))
    }
    setkey(l, CODGEO, year)
    l
}


#' Generate PRA-City mapping for manual check
#' 
#' this function generates a csv file which we manually check for the correct mapping between old and new PRA code. 
#' 
#' Our mapping is correct in the vast majority of cases. Some cities have multiple PRAs assigned to them, in particular Paris.
#' For more info please see [`pra_specials()`] for a precise list.
pra_manual_check <- function() {
    topn = 100
    L = list()
    L[[1]] <- pra_1950()
    L[[2]] <- pra_1975()
    L[[3]] <- pra_1990()
    L[[4]] <- pra_2000()
    L[[5]] <- pra_2015()
    l = rbindlist(L, fill = TRUE)
    data.table::fwrite(l, file = file.path(PRAdir(),paste0("PRA_cleaned_INPUT.csv")))
    l
}

nafinder <- function(){
    x = pra_stack(topn = 200)
    l = list(
        nop = x[is.na(price),.(CODGEO,LIBGEO,year,PRA_Code,PRA_Lib,price)],
        now = x[is.na(logmean_netsalaire),.(CODGEO,LIBGEO,year,PRA_Code,PRA_Lib,price)]
    )
    data.table::fwrite(l$nop,
                       file = file.path(PRAdir(),paste0("pra-output-200-noprice.csv")))
    data.table::fwrite(l$now,
                       file = file.path(PRAdir(),paste0("pra-output-200-nowage.csv")))
    l
}


starfun <- function(){
    c('*'=.1, '**'=.05, '***'=.01)
}

run_all_tables <- function(writedisk = FALSE){
    o = list()
    o$t2 = table2(save = writedisk)
    o$t3 = tableA3(writedisk = writedisk)
    o$t4 = tableA4(save = writedisk)
    o$t5t6 = tableA5_A6(writedisk = writedisk)
    o
}

#' Data Builder for tables 2, A3, A4, A5
#'
data_tables2_A3 <- function(){

    # load model output from disk
    model = data.table::fread(file.path(outmodeldir(),"tables","hetr-density-price-1975s.csv"))
    # add pop share by year
    model[, pop_share := pop_data / sum(pop_data), by = year]
    # add 1975 pop share
    w1975 = model[year == 1975, .(CODGEO,Lu_share1975 = Lu / sum(Lu))]
    model = merge(model, w1975, by = "CODGEO", all.x = TRUE)
    model[, log_thetau := log(thetau)]

    # get land price data
    p = pra_stack(topn = 200, overwrite = FALSE)
    setnames(p, c("density_data","logmean_netsalaire","price"),c("citydensity","log_thetau","pricer"))
    p[, pop_share := pop_data / sum(pop_data), by = year]
    # fix DEP code
    p[, DEP := stringr::str_pad(DEP, 2, side = "left",pad = "0")]

    # get IV data
    iv_yields = haven::read_dta(file.path(IVdir(),"IV_yields.dta")) %>%
        dplyr::mutate(DEP = stringr::str_pad(dep, 2, "left", pad = "0")) %>%
        data.table

    # get land use data to select the correct departments (the wheat producers)
    wheat_shares = haven::read_dta(file.path(IVdir(),"AgriLandUse.dta")) %>%
        dplyr::mutate(DEP = stringr::str_pad(dep, 2, "left", pad = "0"))

    piv = merge(p, iv_yields, by = c("year","DEP"), all.x = TRUE)
    piv = merge(piv, wheat_shares, by = c("DEP"), all.x = TRUE)

    piv[is.na(bledur2000), bledur2000 := 0.0]
    piv[is.na(bledur2010), bledur2010 := 0.0]
    piv[, shareble := ble2000 / sau2000]
    piv[, share_cereals := cereales2000/sau2000]

    # prepare shift share setup: total output for each crop per year. output = yield * area
    shift_data = iv_yields[, c("year","dep",paste0("yield_",cereals()),paste0("area_",cereals())), with= FALSE]
    yields_shift = shift_data[ , lapply(.SD, sum), by = year, .SDcols = patterns("^area_")]

    # aggregate yield per hectare for the entire country by year
    output_ha_year = shift_data[, .(
        sum_maize = sum(area_maize * yield_maize, na.rm = TRUE) / sum(area_maize, na.rm = TRUE),
        sum_wheat = sum(area_wheat * yield_wheat, na.rm = TRUE) / sum(area_wheat, na.rm = TRUE),
        sum_potatoes = sum(area_potatoes * yield_potatoes, na.rm = TRUE) / sum(area_potatoes, na.rm = TRUE),
        sum_oats   = sum(area_oats * yield_oats, na.rm = TRUE) / sum(area_oats, na.rm = TRUE),
        sum_barley = sum(area_barley * yield_barley, na.rm = TRUE) / sum(area_barley, na.rm = TRUE),
        mean_yield_potatoes = mean(yield_potatoes, na.rm = TRUE),
        mean_yield_oats     = mean(yield_oats    , na.rm = TRUE),
        mean_yield_barley   = mean(yield_barley  , na.rm = TRUE),
        mean_yield_wheat   = mean(yield_wheat  , na.rm = TRUE),
        mean_yield_maize    = mean(yield_maize   , na.rm = TRUE)
                   ) ,
               by = year]


    list(model = model, data = piv, shift = output_ha_year)
}


#' remove SE Clustered form a tex table
#'
strip_SE_table <- function(d){
    d %>% gsub(".*\\\\centering","",.) %>%
        gsub("\\\\multicolumn.*","\\\\end\\{tabular\\}",.) %>%
        gsub("\\\\begin\\{table\\}","",.) %>%
        gsub("\\\\end\\{table\\}","",.)
}


cereals <- function(){c("potatoes","oats","maize","barley","wheat")}

texstyle <- function(){
    fixest::style.tex(main = "aer",
                      depvar.title = "",
                      var.title = "",
                      fixef.title = "",
                      fixef.prefix = "FE: "
                      )
}

etab_style <- function(){
    fixest::style.df(
        depvar.title = "",
        fixef.title = "Fixed-Effects:",
        fixef.line = "-",
        fixef.prefix = "",
        fixef.suffix = "",
        slopes.title = "Varying Slopes:",
        slopes.line = "-",
        slopes.format = "__var__ (__slope__)",
        stats.title = "_",
        stats.line = "_",
        yesNo = c("Yes", "No"),
        headers.sep = TRUE,
        signif.code = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, . = 0.1),
        interaction.combine = " x ",
        i.equal = " = ",
        default = FALSE
    )
}

#' Tables A.5 and A.6: Urban Density and Land Values with Shift Share IV
#'
#' Same exercise as [`tableA3()`] but with shift share IV instead of wheat yields
#'
#' Produces one tables for first (A5) and second (A6) stages.
#'
#'
tableA5_A6 <- function(writedisk = FALSE){

    md = data_tables2_A3()

    # finish construction the `shift` part first
    md$shift
    x = merge(md$data, md$shift, by = "year", all.x = TRUE)

    x[, slogyield_wheat := s_area_wheat1960 * log(sum_wheat)]
    x[, slogyield_potatoes := s_area_potatoes1960 * log(sum_potatoes)]
    x[, slogyield_barley := s_area_barley1960 * log(sum_barley)]
    x[, slogyield_maize := s_area_maize1960 * log(sum_maize)]
    x[, slogyield_oats := s_area_oats1960 * log(sum_oats)]



    # 2SLS Regressions on Models
    # ====================

    tsls = list()
    tsls[["(1)"]] = summary(fixest::feols(log(citydensity) ~ 1 |
                                              year + DEP |
                                              log(pricer) ~ slogyield_wheat +
                                              slogyield_maize +
                                              slogyield_oats +
                                              slogyield_barley +
                                              slogyield_potatoes,data = x[share_cereals > 0.3]), cluster = c('DEP'))
    tsls[["(2)"]] = summary(fixest::feols(log(citydensity) ~ log_thetau |
                                              year + DEP |
                                              log(pricer) ~ slogyield_wheat +
                                              slogyield_maize +
                                              slogyield_oats +
                                              slogyield_barley +
                                              slogyield_potatoes,data = x[share_cereals > 0.3]), cluster = c('DEP'))
    tsls[["(3)"]] = summary(fixest::feols(log(citydensity) ~ log_thetau |
                                              year + CODGEO |
                                              log(pricer) ~ slogyield_wheat +
                                              slogyield_maize +
                                              slogyield_oats +
                                              slogyield_barley +
                                              slogyield_potatoes,data = x[share_cereals > 0.3]), cluster = c('DEP'))


    dict = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$",
             "slogyield_wheat" = "Shift-Share Wheat",
             "slogyield_maize" = "Shift-Share Maize",
             "slogyield_oats" = "Shift-Share Oats",
             "slogyield_barley" = "Shift-Share Barley",
             "slogyield_potatoes" = "Shift-Share Potatoes",
            #  "log_thetau" = "$\\log w_{u,k,t}$",
             "CODGEO" = "City",
             "year" = "Year",
             "DEP" = "Département")

    if (writedisk) {
        first_stage = fixest::etable(tsls, stage = 1, dict = dict,
            file = file.path(outdatadir(),"tables","tableA6.tex"),
            placement="h",
            depvar = FALSE,
            cluster = c("DEP","DEP", "DEP"),replace = TRUE,
            digits.stats = "r3",
            drop = "log_thetau",
            extralines = list("\\midrule Controls" = c("","$\\log w_{u,k,t}$","$\\log w_{u,k,t}$")),
            style.tex = texstyle())
        second_stage = fixest::etable(tsls, stage = 2, dict = dict, file = file.path(outdatadir(),"tables","tableA5.tex"),
             placement="h",
             depvar = FALSE,
             cluster = c("DEP","DEP", "DEP"),replace = TRUE,
             fitstat = c("n","r2"),
             drop = "log_thetau",
             extralines = list("\\midrule Controls" = c("","$\\log w_{u,k,t}$","$\\log w_{u,k,t}$")),
             digits.stats = "r3",
             style.tex = texstyle())
    } else {
        first_stage = fixest::etable(tsls, stage = 1, dict = dict,
                                     placement="h", label = "tab:shift-share-first", title = "First stage of shift-share exercise.",
                                     cluster = c("DEP","DEP", "DEP"),replace = TRUE,
                                     digits.stats = "r3",
                                     drop = "log_thetau",
                                    depvar = FALSE,
                                     extralines = list("Controls" = c("$\\log w_{u,k,t}$","$\\log w_{u,k,t}$","$\\log w_{u,k,t}$")),
                                     style.tex = texstyle(),tex = TRUE)
        second_stage = fixest::etable(tsls, stage = 2, dict = dict,
                                      placement="h", label = "tab:shift-share-second", title = "Second stage of shift-share exercise.",
                                      cluster = c("DEP","DEP", "DEP"),replace = TRUE,
                                      drop = "log_thetau",
                                      fitstat = c("n","r2"),
                                      depvar = FALSE,
                                      digits.stats = "r3",
                                      style.tex = texstyle(), tex = TRUE)
    }


    return(list(first_stage, second_stage))
#
#     rows <- tibble::tribble(~term, ~"OLS (1)",~"OLS (2)",~"OLS (3)",~"IV (1)",~"IV (2)",~"IV (3)",
#                             'Controls', '-', '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$', '-', '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$')
#     attr(rows, 'position') <- c(3, 1)
#
#     tab = modelsummary::modelsummary(l,
#                                      output = "latex_tabular",
#                                      stars = TRUE,
#                                      gof_omit = "R2 \\w*|AIC|BIC|Log|RMSE|Std.",
#                                      coef_map = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$","fit_log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$"),
#                                      escape = FALSE,
#                                      add_rows = rows)
#
#     # kableExtra::row_spec(c(8), hline_after = TRUE) %>%
#
#     tab = tab %>%
#         kableExtra::add_header_above(c(" " = 1, "OLS" = 3, "IV" = 3))
#     tab %>%
#         readr::write_lines(file.path(outdatadir(),"tables",paste0("tableA3.tex")))
#
#     tab


}



#' Table A.4: Urban Density and Land Values
#'
#' First Stage of [`tableA3()`].
#'
#'
tableA4 <- function(save = FALSE){

    md = data_tables2_A3()

    # Regressions on Model
    # ====================

    l = list()
    l[["(1)"]] = summary(fixest::feols( log(pricer)  ~ log(yield_wheat)             | year,data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l[["(2)"]] = summary(fixest::feols( log(pricer)  ~ log(yield_wheat) + log_thetau| year,data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l[["(3)"]] = summary(fixest::feols( log(pricer)  ~ log(yield_wheat) + log_thetau | year + REG, data = md$data[shareble > 0.2]), cluster = c('DEP'))

    options("modelsummary_format_numeric_latex" = "plain")

    rows <- tibble::tribble(~term, ~"(1)",~"(2)",~"(3)",
                            'Controls', '-', '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$')
    attr(rows, 'position') <- c(3, 1)

    gm <- tibble::tribble(
        ~raw,        ~clean,          ~fmt,
        "nobs",      "N",             0,
        "r.squared", "$R^2$", 3,
        "FE: year", "FE: Year", 0,
        "FE: REG", "FE: Région", 0
    )

    tab = modelsummary::modelsummary(l,
                                     output = "latex_tabular",
                                     stars = starfun(),
                                     gof_map = gm,
                                     coef_map = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$","log(yield_wheat)" = "$\\log \\text{Yield}_{k,t}$"),
                                     escape = FALSE,
                                     add_rows = rows)

    # kableExtra::row_spec(c(8), hline_after = TRUE) %>%

    # tab = tab %>%
        # kableExtra::add_header_above(c(" " = 1, "$\\log \\overline{\\rho}_{r,k,t}$" = 3))
    tab %>%
        readr::write_lines(file.path(outdatadir(),"tables",paste0("tableA4.tex")))

    tab


}

#' Table A.3: Urban Density and Land Values
#'
#' This is a more extensive version of [`table2()`]. OLS and IV results with varying controls.
#'
#' We add 2 columns for both model and data, one without any controls, and one with city FEs.
#' The function also writes a table to disk for R3 who wanted to see more specifications.
#'
tableA3 <- function(writedisk = FALSE){

    md = data_tables2_A3()

    # Regressions on Model
    # ====================

    l = list()
    l1 = list()
    l2 = list()



    l[["(1)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer)              | year,data = md$data), cluster = c('DEP'))
    l[["(2)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau| year,data = md$data), cluster = c('DEP'))
    l[["(3)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau | year + REG, data = md$data), cluster = c('DEP'))

    l[["(4)"]] = summary(fixest::feols(log(citydensity) ~      1       | year | log(pricer) ~ log(yield_wheat),data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l[["(5)"]] = summary(fixest::feols(log(citydensity) ~  log_thetau| year | log(pricer) ~ log(yield_wheat),data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l[["(6)"]] = summary(fixest::feols(log(citydensity) ~  log_thetau | year + REG | log(pricer) ~ log(yield_wheat), data = md$data[shareble > 0.2]), cluster = c('DEP'))

    # Options 1/ 8 columns
    l1[["(1)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer)              | year + REG   , data = md$data), cluster = c('DEP'))
    l1[["(2)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer)              | year + DEP   , data = md$data), cluster = c('DEP'))
    l1[["(3)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer)              | year + CODGEO, data = md$data), cluster = c('DEP'))
    l1[["(4)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau | year + CODGEO, data = md$data), cluster = c('DEP'))

    l1[["(5)"]] = summary(fixest::feols(log(citydensity) ~       1     | year + REG    | log(pricer) ~ log(yield_wheat),data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l1[["(6)"]] = summary(fixest::feols(log(citydensity) ~       1     | year + DEP    | log(pricer) ~ log(yield_wheat),data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l1[["(7)"]] = summary(fixest::feols(log(citydensity) ~       1     | year + CODGEO | log(pricer) ~ log(yield_wheat), data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l1[["(8)"]] = summary(fixest::feols(log(citydensity) ~  log_thetau | year + CODGEO | log(pricer) ~ log(yield_wheat), data = md$data[shareble > 0.2]), cluster = c('DEP'))

    # OLS
    # FE REG + wage
    # FE DEP + wage
    # FE CODGEO + wage
    # IV (the same)
    # FE REG + wage
    # FE DEP + wage
    # FE CODGEO + wage
    l2[["(1)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau | year + REG   , data = md$data), cluster = c('DEP'))
    l2[["(2)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau | year + DEP   , data = md$data), cluster = c('DEP'))
    l2[["(3)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau | year + CODGEO, data = md$data), cluster = c('DEP'))

    l2[["(4)"]] = summary(fixest::feols(log(citydensity) ~ log_thetau  | year + REG    | log(pricer) ~ log(yield_wheat),data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l2[["(5)"]] = summary(fixest::feols(log(citydensity) ~ log_thetau  | year + DEP    | log(pricer) ~ log(yield_wheat),data = md$data[shareble > 0.2]), cluster = c('DEP'))
    l2[["(6)"]] = summary(fixest::feols(log(citydensity) ~ log_thetau  | year + CODGEO | log(pricer) ~ log(yield_wheat), data = md$data[shareble > 0.2]), cluster = c('DEP'))


    options("modelsummary_format_numeric_latex" = "plain")

    rows <- tibble::tribble(~term, ~"OLS (1)",~"OLS (2)",~"OLS (3)",~"IV (1)",~"IV (2)",~"IV (3)",
                            'Controls', '-', '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$', '-', '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$')
    attr(rows, 'position') <- c(3, 1)

    rows1 <- tibble::tribble(~term, ~"OLS (1)",~"OLS (2)",~"OLS (3)",~"OLS (4)",~"IV (1)",~"IV (2)",~"IV (3)",~"IV (4)",
                            'Controls', '-', '-','-','$\\log w_{u,k,t}$','-', '-','-','$\\log w_{u,k,t}$')
    attr(rows1, 'position') <- c(3, 1)

    rows2 <- tibble::tribble(~term, ~"OLS (1)",~"OLS (2)",~"OLS (3)",~"IV (1)",~"IV (2)",~"IV (3)",
                            'Controls', '$\\log w_{u,k,t}$', '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$', '$\\log w_{u,k,t}$', '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$')
    attr(rows2, 'position') <- c(3, 1)

    # gof map
    # gm <- modelsummary::gof_map
    # gm$omit <- TRUE # omit all
    # gm[gm$raw == "r.squared", "omit"] = FALSE
    # gm[gm$raw == "nobs", "omit"] = FALSE

    gm <- tibble::tribble(
        ~raw,        ~clean,          ~fmt,
        "nobs",      "N",             0,
        "r.squared", "$R^2$", 3,
        "FE: year", "FE: Year", 0,
        "FE: REG", "FE: Région", 0
    )

    tab = modelsummary::modelsummary(l,
                                     output = "latex_tabular",
                                     stars = starfun(),
                                     gof_map = gm,
                                    #  gof_omit = "R2 \\w*|AIC|BIC|Log|RMSE|Std.",
                                     coef_map = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$","fit_log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$"),
                                     escape = FALSE,
                                     add_rows = rows)

    tab1 = modelsummary::modelsummary(l1,
                                     output = "latex_tabular",
                                     stars = starfun(),
                                     gof_map = gm,
                                     coef_map = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$","fit_log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$"),
                                     escape = FALSE,
                                     add_rows = rows1)

    tab2 = modelsummary::modelsummary(l2,
                                      output = "latex_tabular",
                                      stars = starfun(),
                                      gof_map = gm,
                                      coef_map = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$","fit_log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$"),
                                      escape = FALSE,
                                      add_rows = rows2)

    # kableExtra::row_spec(c(8), hline_after = TRUE) %>%

    if (writedisk){
        # tab = tab %>%
        #     kableExtra::add_header_above(c(" " = 1, "OLS" = 3, "IV" = 3))
        tab %>%
            readr::write_lines(file.path(outdatadir(),"tables",paste0("tableA3.tex")))

        # tab1 = tab1 %>%
        #     kableExtra::add_header_above(c(" " = 1, "OLS" = 4, "IV" = 4))
        tab1 %>%
            readr::write_lines(file.path(outdatadir(),"tables",paste0("tableA3-R3-option1.tex")))

        # tab2 = tab2 %>%
        #     kableExtra::add_header_above(c(" " = 1, "OLS" = 3, "IV" = 3))
        tab2 %>%
            readr::write_lines(file.path(outdatadir(),"tables",paste0("tableA3-R3-option2.tex")))


    }

    list(tab, tab1, tab2)


}


#' Produce Table 2 in Main Text
#'
#' table with 2 columns, model and data, for
#' regression of log city density on log price of surrounding agricultural
#' area. The data for the model regression is produced by the function
#' `plot_het` and is part of the corresponding julia package available
#' [here](https://github.com/floswald/LandUse.jl).
table2 <- function(save = FALSE){

    # list of models
    l = list()
    lw = list()  # weighted output
    lc = list()  # checking

    # load model and data tables
    md = data_tables2_A3()

    # Regressions on Model
    # ====================

    l[["Model"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau | year, data = md$model,se = "iid"))
    lc[["Model (1)"]] = summary(fixest::feols(log(citydensity) ~ 1 | year, data = md$model,se = "iid"))
    lc[["Model (2)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer)  | year, data = md$model,se = "iid"))
    lc[["Model (3)"]] = summary(fixest::feols(log(citydensity) ~ log_thetau | year, data = md$model,se = "iid"))
    lc[["Model (4)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau | year, data = md$model,se = "iid"))
    lw[["Model"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau | year,data = md$model,weights = ~ Lu_share1975), cluster = c('DEP'))

    # Regressions on Data
    # ===================

    l[["Data (OLS)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau| year,data = md$data), cluster = c('DEP'))
    l[["Data (IV)"]] = summary(fixest::feols(log(citydensity) ~                log_thetau| year | log(pricer) ~ log(yield_wheat),data = md$data[shareble > 0.2]), cluster = c('DEP'))
    # lr[["Data (OLS)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau| year,p), cluster = c('DEP'))

    lw[["Data (OLS)"]] = summary(fixest::feols(log(citydensity) ~ log(pricer) + log_thetau| year,data = md$data, weights= ~pop_1975), cluster = c('DEP'))
    lw[["Data (IV)"]] = summary(fixest::feols(log(citydensity) ~                log_thetau| year | log(pricer) ~ log(yield_wheat),data = md$data[shareble > 0.2], weights= ~pop_1975), cluster = c('DEP'))


    options("modelsummary_format_numeric_latex" = "plain")

    rows <- tibble::tribble(~term, ~"Model",~"Data (OLS)",~"Data (IV)",
                            '\\hline Controls',  '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$','$\\log w_{u,k,t}$')
    
    rows1 <- tibble::tribble(~term, ~"Model",~"Data (OLS)",~"Data (IV)",
                            'Controls',  '$\\log w_{u,k,t}$','$\\log w_{u,k,t}$','$\\log w_{u,k,t}$')
    

    attr(rows, 'position') <- c(3)
    attr(rows1, 'position') <- c(3)

    gm <- tibble::tribble(
        ~raw,        ~clean,          ~fmt,
        "nobs",      "N",             0,
        "r.squared", "$R^2$", 4,
        "FE: year", "FE: Year", 0
    )

    tab = modelsummary::modelsummary(l,
                                      output = "latex_tabular",
                                     stars = starfun(),
                                     gof_map = gm,
                                      coef_map = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$","fit_log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$"),
                                      escape = FALSE,
                                     add_rows = rows)
    tabw = modelsummary::modelsummary(lw,
                                     output = "latex_tabular",
                                     stars = starfun(),
                                     gof_map = gm,
                                     coef_map = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$","fit_log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$"),
                                     escape = FALSE,
                                     add_rows = rows)

    tab3 = modelsummary::modelsummary(l,
                                      output = "latex",
                                     stars = starfun(),
                                     gof_map = gm,
                                      coef_map = c("log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$","fit_log(pricer)" = "$\\log \\overline{\\rho}_{r,k,t}$"),
                                      escape = FALSE,
                                     add_rows = rows1,booktabs = TRUE,
                                    title = "Urban density and rural land values.\\label{tab:densityruralprice}")
    
    tab3_fixed <- gsub("\\\\begin\\{table\\}", "\\\\begin{table}[ht]", tab3)


    # tab3_fixed %>% readr::write_lines(file.path(outdatadir(),"tables",paste0("table2-test.tex")))


    if (save){
        fixest::etable(lc,
                       placement = "h",
                       title = "checking R2 in table 2",
                       file = file.path(outdatadir(),"tables",paste0("table2-check.tex")),
                       replace = TRUE,
                       digits.stats = "r3")
    }

    # tab = tab %>%
        # kableExtra::add_header_above(c(" " = 1, "log Urban Density" = 3))# %>%
        #add_stars_legend(starfun())
    # tabw = tabw %>%
        # kableExtra::add_header_above(c(" " = 1, "log Urban Density" = 3))
    if (save){
        tabw %>% readr::write_lines(file.path(outdatadir(),"tables",paste0("table2-weighted.tex")))
        tab %>% readr::write_lines(file.path(outdatadir(),"tables",paste0("table2.tex")))


    }
        # tab2 %>% readr::write_lines(file.path(outdatadir(),"tables",paste0("lm-model-data-cityFE.tex")))
        # tab2w %>% readr::write_lines(file.path(outdatadir(),"tables",paste0("lm-model-data-cityFE-weighted.tex")))

    # list(l,lw,lc)
    list(tab,tabw,tab3)

}

add_stars_legend <- function(s, leg){
    gsub("\\\\end\\{tabular\\}",paste0(leg,"\\","\\\\end\\{tabular\\}"),s)
}

#' Run Regression 2 for R1
#'
reg2_R1 <- function(save = FALSE){

    # list of models
    l = list()

    # load model output from disk
    model = data.table::fread(file.path(outmodeldir(),"tables","hetr-density-price-1975s.csv"))

    # get land price data
    p = pra_stack(topn = 200, overwrite = FALSE)

    d = merge(model[,.(thetau ,year, CODGEO = as.character(CODGEO))], p[,.(logmean_netsalaire, year, CODGEO = as.character(CODGEO))], by = c("year","CODGEO"), all.y = FALSE)

    l = list()
    l[["Log Urban Wage (Data)"]] = fixest::feols( logmean_netsalaire ~ log(thetau)  | year, data = d, cluster = "CODGEO")

    options("modelsummary_format_numeric_latex" = "plain")

    # options(modelsummary_factory_default = "latex")


    tab = modelsummary::modelsummary(l,
                                     output = "latex_tabular",
                                     stars = starfun(),
                                     gof_omit = "R2 \\w*|AIC|BIC|Log|RMSE|Std.",
                                     coef_map = c("log(thetau)" = "$\\log \\theta_{u,k,t}$ (Model)","logmean_netsalaire" = "$\\log \\w_{u,k,t}$"),
                                     escape = FALSE)


    # tab = tab %>%
        # kableExtra::add_header_above(c(" " = 1, "log Urban Wage" = 1))


    if (save){
        tab %>% readr::write_lines(file.path(R2dir(),paste0("R1-reg2.tex")))
    }
    tab

}

pra_stack_prices_plot <- function(topn){
    x = pra_stack(topn = topn)
    if (topn == 100){
        years <- c(1975,1990,2000,2015)
    } else {
        years <- c(1990,2000,2015)
    }

    ms = lapply(years, function(z) {lm(log(real_price_euros)~ log(real_price_euros_lag), data = x[year == z])})
    mr = lapply(years, function(z) {lm(p_rank ~ plag_rank, data = x[year == z])})

    coefs = lapply(ms, function(z) broom::tidy(z) %>% dplyr::filter(term != "(Intercept)"))
    names(coefs) <- paste(years)

    r_coefs = lapply(mr, function(z) broom::tidy(z) %>% dplyr::filter(term != "(Intercept)"))
    names(r_coefs) <- paste(years)


    R2s = lapply(ms, function(z) summary(z)$r.squared)
    names(R2s) <- paste(years)

    r_R2s = lapply(mr, function(z) summary(z)$r.squared)
    names(r_R2s) <- paste(years)

    # make a graph showing density vs price
    Ld = lapply(c(1975,1990,2000,2015), function(z) {ggplot(x[year == z], aes(x = log(price), y = log(density))) + geom_point() + ggtitle(paste(z,": Density vs Prices")) + geom_smooth(method = "lm") + theme_bw()})

    # make a graph showing area vs price
    La = lapply(c(1975,1990,2000,2015), function(z) {ggplot(x[year == z], aes(x = log(price), y = log(area))) + geom_point() + ggtitle(paste(z,": Density vs Prices")) + geom_smooth(method = "lm") + theme_bw()})

    # make a graph showing lagged prices
    L = lapply(years, function(z) {ggplot(x[year == z], aes(x = log(real_price_euros_lag), y = log(real_price_euros))) + geom_point() + ggtitle(paste(z,": Log Prices"), subtitle = paste("Slope =",round(coefs[[paste(z)]]$estimate,2),", R2 =", round(R2s[[paste(z)]],2))) + geom_smooth(method = "lm") + theme_bw()})

    # make a graph showing ranks of prices
    r_L = lapply(years, function(z) {ggplot(x[year == z], aes(x = plag_rank, y = p_rank)) + geom_point() + ggtitle(paste(z," Ranks of Prices"), subtitle = paste("Slope =",round(r_coefs[[paste(z)]]$estimate,2),", R2 =", round(r_R2s[[paste(z)]],2))) + geom_smooth(method = "lm") + scale_x_continuous("lag rank of price") + scale_y_continuous("rank of price") + theme_bw()})



    d_pg = cowplot::plot_grid(plotlist = Ld, nrow = 2, ncol = 2, rel_widths = c(1,1))
    a_pg = cowplot::plot_grid(plotlist = La, nrow = 2, ncol = 2, rel_widths = c(1,1))
    pg = cowplot::plot_grid(plotlist = L, nrow = 2, ncol = 2, rel_widths = c(1,1))
    r_pg = cowplot::plot_grid(plotlist = r_L, nrow = 2, ncol = 2, rel_widths = c(1,1))
    ggsave(filename = file.path(outdatadir(),"plots", paste0("prices",topn,".pdf")), plot = pg, width=9, height = 7)
    ggsave(filename = file.path(outdatadir(),"plots", paste0("prices-rank",topn,".pdf")), plot = r_pg, width=9, height = 7)
    ggsave(filename = file.path(outdatadir(),"plots", paste0("prices-density",topn,".pdf")), plot = d_pg, width=9, height = 7)
    ggsave(filename = file.path(outdatadir(),"plots", paste0("prices-area",topn,".pdf")), plot = a_pg, width=9, height = 7)
    list(pg,d_pg,r_pg,a_pg)


}

pra_regs <- function(p, save = FALSE){

    n = p[year == 1975, .N]
    if (n <= 100){
        topn <- 100
        years <- c(1950,1975,1990,2000,2015)
    } else {
        topn <- 200
        years <- c(1975,1990,2000,2015)
    }

    # year by year
    my = list()
    my2 = list()
    my3 = list()
    for (iy in years){
        if (iy == 1950){
            my[[paste0(iy)]]  <- lm(  log(density) ~ log(price) , data = p[year == iy] )

        } else {
            my[[paste0(iy)]]  <- lm(  log(density) ~ log(price) + logmean_netsalaire, data = p[year == iy] )

        }
        my2[[paste0(iy)]]  <- lm( log(density) ~ log(price) + log(pop) , data = p[year == iy] )
        my3[[paste0(iy)]]  <- lm( log(density) ~ log(price) , data = p[year == iy] )
    }

    if (save) {
        options("modelsummary_format_numeric_latex" = "plain")
        options(modelsummary_factory_html = 'kableExtra')

        options(modelsummary_factory_default = "latex")
        tby = modelsummary::modelsummary(my,
                                         output = "latex",
                                         stars = starfun(),
                                         gof_omit = "R2 \\w*|AIC|BIC|Log",
                                         coef_map = c("log(price)" = "log price","logmean_netsalaire" = "log Urban Productivity" ),
                                         escape = FALSE,title = paste0("Using ",topn," cities \\label{tab:reg-density-",topn,"}"))
        tby %>%
            kableExtra::add_header_above(c(" " = 1, "log density" = length(years))) %>%
            readr::write_lines(file.path(outdatadir(),"tables", paste0("lm-density-by-year-prod-",topn,".tex")))

        tby2 = modelsummary::modelsummary(my2,
                                          output = "latex",
                                          stars = starfun(),
                                          gof_omit = "R2 \\w*|AIC|BIC|Log",
                                          coef_map = c("log(price)" = "log price",
                                                       "log(pop)" = "log pop"),
                                          escape = FALSE,title = paste0("Using ",topn," cities \\label{tab:reg-density-pop-",topn,"}"))
        tby2 %>%
            kableExtra::add_header_above(c(" " = 1, "log density" = length(years))) %>%
            readr::write_lines(file.path(outdatadir(),"tables", paste0("lm-density-by-year-pop-",topn,".tex")))

        tby3 = modelsummary::modelsummary(my3,
                                          output = "latex",
                                          stars = starfun(),
                                          gof_omit = "R2 \\w*|AIC|BIC|Log",
                                          coef_map = c("log(price)" = "log price"),
                                          escape = FALSE,title = paste0("Using ",topn," cities \\label{tab:reg-density-",topn,"}"))
        tby3 %>%
            kableExtra::add_header_above(c(" " = 1, "log density" = length(years))) %>%
            readr::write_lines(file.path(outdatadir(),"tables", paste0("lm-density-by-year-",topn,".tex")))


    }

    # stacking data and doing FEs
    m = list()
    m[["(1)"]]  <- fixest::feols( fml = log(density) ~ log(price) | CODGEO, data = p )
    m[["(2)"]] <- fixest::feols( fml = log(density) ~ log(price)  + logmean_netsalaire| year, data = p )
    m[["(3)"]] <- fixest::feols( fml = log(density) ~ log(price) | year + REG, data = p )
    if (save) {
        tb = modelsummary::modelsummary(m,
                                        output = "latex",
                                        stars = starfun(),
                                        gof_omit = "R2 \\w*|AIC|BIC|Log",
                                        coef_map = c("log(price)" = "$\\log price$"),
                                        escape = FALSE,title = paste0("Using ",topn," cities"))
        tb %>%
            kableExtra::add_header_above(c(" " = 1, "log density" = 2)) %>%
            readr::write_lines(file.path(outdatadir(),"tables", paste0("FEregs",topn,".tex")))

    }
    list(my,my2,my3,m)
}

# Data I/O =========

#' write PRA classification
#'
write_PRA <- function() {
    x = data.table(readxl::read_xls(file.path(PRAdir(), "Referentiel_CommuneRA_PRA_2017.xls"), skip = 5), key = "CODGEO")
    x[, CODGEO := stringr::str_pad(as.character(CODGEO),width = 5, side = "left", pad = "0")]
    x[, c("PRA_Name","Pra_ID") := tstrsplit(PRA_Lib, split = " - ")]
    x[, PRA_Lib := PRA_Name]
    saveRDS(x, file.path(PRAdir(), "Referentiel_CommuneRA_PRA_2017.Rds"))
}

#' load official PRA-Commune classification
#'
load_PRA <- function() {
    readRDS(file.path(PRAdir(), "Referentiel_CommuneRA_PRA_2017.Rds"))
}

francs_2_euros <- function(francs) {francs / 6.56}
euros_2_francs <- function(euros) {euros * 6.56}

#' PRA new-old mapping Crosswalk
#'
#' We recovered this mapping manually, as there is no
#' official crosswalk from old to new PRA codes available.
#'
#' The mapping is provided as `PrixCode => PRA_Code`. `PrixCode` is column number 7.
PRA_xw <- function() {
    f = file.path(PRAdir(),"input","manual-mapping", "Referentiel_CommuneRA_PRA_2017_mapping.xls")
    d = readxl::read_xls(f, sheet = 2,col_types = "text",col_names = c("Département","RA_Code","RA_Libelle","PRA_Code","PRA_Libelle","RA_Code2","PrixCode","PrixCode2","PrixCode3","Deplib","missing","missing2"))
}

PRA_2015_VVT <- function(){
    # 1. load VVT price data
    f = file.path(PRAdir(),"input","manual-mapping", "cd2019-19_tableaux_VVT_mapping.xlsx")
    d = readxl::read_xlsx(f, col_types = "text",sheet = "2015", skip = 9, col_names = c("PrixCode","PRA_Lib","Dep","PRA_Lib2","price2015","min2015","max2015"))
    # keep only PRAs
    d = d[!is.na(d$PrixCode),c(1,2,5)]
    # price as numeric
    d$price2015 = as.numeric(d$price2015)

    # 2. merge with PRA xw
     o = d %>%
        dplyr::inner_join(PRA_xw(),by = c("PrixCode")) %>%
        dplyr::rename("price_euros" = "price2015") %>%
        dplyr::mutate(price = euros_2_francs(price_euros))

    a = d %>%
        dplyr::anti_join(PRA_xw(),by = c("PrixCode")) %>%
        select("PrixCode","PRA_Lib")
    list(merge = o, miss = a)
}

PRA_2015_map_merge <- function(d){
    sh = mapdata_pra()
    sh %>%
        dplyr::left_join(d, by = "PRA_Code")
}


#' petite region agricole 1999-2020 with PRA mapping
#'
PRA_2015_VVTDiffusion <- function(yr = 2015){

    x = readxl::read_xlsx(file.path(PRAdir(), "input","manual-mapping","DIFFUSION_VVT_2020_site_DRIAAF_cle0d516d_mapping.xlsx"),
                          sheet = 4, skip = 4,na = "NS", col_types = "text")
    # filter columns
    x = x[ , -(c(2,(ncol(x)-2):ncol(x)))]
    # rename column 1
    names(x)[2] <- "PRA_Lib"
    data.table::setDT(x)
    m = data.table::melt.data.table(x, id.vars = c("PrixCode","PRA_Lib"))
    data.table::setnames(m, c("variable","value"),c("year","price_euros"))
    m[,year := as.integer(as.character(year))]
    m[,price_euros := as.numeric(price_euros)]
    m[,price_euros := as.numeric(price_euros)]
    m[,price := euros_2_francs(price_euros)]
    m = m[year == yr]
    m = m[!is.na(PrixCode)]

    # 2. merge with PRA xw
    o = m %>%
        dplyr::inner_join(PRA_xw(),by = c("PrixCode"))

    a = m %>%
        dplyr::anti_join(PRA_xw(),by = c("PrixCode")) %>%
        select("PrixCode","PRA_Lib")
    list(merge = o, miss = a)

}

DEP_1892_prices <- function(){
    d = data.table::fread(file.path(OCRdir(),"..", "cleaned-output","table1-1892.csv"), colClasses = c("dep" = "character"))
    d[,price_euros := francs_2_euros(francs_per_hectare)]

    sh = sf::st_read(file.path(datadir(), "departements-shp","departements-20140306-100m.shp")) %>%
        dplyr::filter(code_insee %in% sprintf("%02d", (1:95))) %>%
        left_join(d[,.(dep,price_euros,price = francs_per_hectare, dep_clean)], by = c("code_insee" = "dep"))
}


#' Merge results with PRA classification
#'
merge_results_PRA <- function(topn = 100){
    p = load_PRA()
    p[, c("NATURE_EPCI","EPCI", "ARR","CV","RA_Code") := NULL]
    # p[, c("PRA_Lib","V2") := tstrsplit(PRA_Lib, split = " - ")]
    # p[, c("PRA_Lib","V2") := NULL]

    # fix paris
    p[CODGEO == "75056", CODGEO := "75060"]

    r = export_pop_area(topn = topn)
    setkey(r , CODGEO)
    # setnames(r, c("pop_data","area_data"), c("pop","area"))

    x = merge(p,r[,!"DEP"][,!"LIBGEO"],by = c("CODGEO"), all.y = TRUE)
    saveRDS(unique(x[,.(CODGEO,LIBGEO,PRA_Code,PRA_Lib)]), file = file.path(PRAdir(), "crosswalk-codgeo-PRA_Code.Rds"))
    x
}

get_codgeo_pra_xwalk <- function(){
    readRDS(file.path(PRAdir(), "crosswalk-codgeo-PRA_Code.Rds"))
}
