# code to get number of active workers in paris for 1975, 1990 and 2000
# we use the INSEE tables of active population by commune. then we need to aggregate to paris urban area

#' Get Paris Aire Urbaine 1975, 1990, 2000
#'
#' uses our method from \code{\link{measure_cities}} to determine
#' which INSEE communes are part of the parisian urban area. This
#' is an approximation only based on contiguous urban area (it does not
#' consider actual commuting patterns). The first official Urban Area classification
#' is available in 2010.
#'
#' @details We convert our raster data into an sf vector graphics object,
#' and intersect it with the communes shapefile of INSEE.
#'
commute_paris_AU <- function(overwrite = FALSE){
    p = lapply(c(1975,1990,2000), function(x) commute_paris_AU_(x,overwrite = overwrite))
    # export
    p[[1]] %>%
        sf::st_set_geometry(NULL) %>%
        readr::write_csv2(file = file.path(datadir(),"communes-20190101",paste0("paris-",1975,".csv")))
    p[[2]] %>%
        sf::st_set_geometry(NULL) %>%
        readr::write_csv2(file = file.path(datadir(),"communes-20190101",paste0("paris-",1990,".csv")))
    p[[3]] %>%
        sf::st_set_geometry(NULL) %>%
        readr::write_csv2(file = file.path(datadir(),"communes-20190101",paste0("paris-",2000,".csv")))
    p
}

load_paris_AU <- function(yr){
    fread(file = file.path(datadir(),"communes-20190101",paste0("paris-",yr,".csv")))
}

#' get list of communes in paris
#'
#' implementation for delineating paris urban area
commute_paris_AU_ <- function(yr, overwrite = FALSE){
    if (overwrite) {
        # compute
        # 1. get cut out of satellite data
        x = measure_cities()$cropped[[paste(yr)]][["75060"]]  # paris shape for that year
        # 2. cast as a spatialdataFrame
        spdf = as(x$built_cut, 'SpatialPolygonsDataFrame')
        # 3. as sf
        sfdf = sf::st_as_sf(spdf)
        # get communes shapefile and transform CRS
        a = readRDS(file.path(datadir(),"communes-20190101","communes-20190101.Rds")) %>%
            sf::st_transform(raster::crs(x$built_cut)@projargs)
        # intersect
        i = a %>% sf::st_intersects(sfdf)
        # save
        paris = a[unlist(lapply(i,any)), ]
        saveRDS(paris, file.path(datadir(),"communes-20190101",paste0("paris-",yr,".Rds")))
        paris
    } else {
        readRDS(file.path(datadir(),"communes-20190101",paste0("paris-",yr,".Rds")))
    }
}

write_communes <- function(){
    x = geojsonsf::geojson_sf(file.path(datadir(),"communes-20190101","communes-20190101.json"))
    saveRDS(x, file.path(datadir(),"communes-20190101","communes-20190101.Rds"))
}


#' Get Commuting Google Sheet
#'
commute_gsheet <- function(){
    "https://docs.google.com/spreadsheets/d/1xnlpL1J758Gyh0GuvzctOnbOuwR05G_XuB2hl_T4K1U/edit?usp=sharing"

}

commute_browse <- function(){googlesheets4::gs4_browse(commute_gsheet())}

commute_get_RP <- function(){

    x= data.table(readxl::read_xls(file.path(datadir(),"paris-commuting","RP-68-99.xls"),
                                    sheet = 3,
                                    skip = 6))
        n = data.table(long = names(x), code = as.character(x[1,]))
        list(x[-1,],n)

}

commute_write_RP <- function(){
    L = commute_get_RP()
    s = L[[1]]
    n = L[[2]] # names
    n[,c("sector", "sex","year") := lapply(data.table::tstrsplit(long, "\n", fixed = TRUE), function(z){
        gsub(x = stringr::word(z),pattern = "â",replacement = "a")
    })]
    n[ , g:=  paste(c(sector,sex,year),collapse = "_"), by = code]
    n[7:nrow(n) , code := g]
    setnames(s, n[,code])
    s[, CODGEO := paste0(DLTX, CLTX)]

    s.m1 = melt(s, id.vars = c(1:6,ncol(s)), value.name = "workers")
    s.m1[, workers := as.numeric(workers)]
    s.m1[, c("sector", "sex","year") := tstrsplit(variable, "_", fixed = TRUE)]
    s.m1[,year := as.integer(substr(year,3,6))]


    # aggregate
    a = s.m1[ , list(workers = sum(workers)),by = list(CODGEO, LIBELLE,year)]
    no_agri = s.m1[sector != "Agriculture" , list(workers = sum(workers)),by = list(CODGEO, LIBELLE,year)]
    L = list(all = a, noagri = no_agri)
    # save
    saveRDS(L, file.path(datadir(),"paris-commuting","workers-RP.Rds"))

    L

}

commute_read_RP <- function(){
    readRDS(file.path(datadir(),"paris-commuting","workers-RP.Rds"))
}



#' Compute Active Population for Paris AU
#'
#' @details Take our own delineation of the Parisian Urban area
#' from \code{\link{get_paris_AU}} and merge with INSEE population data for each commune.
#' For 1975 and 1990 we use our own delinations, for 2000 and 2010 we have offical classifications.
#'
#'
commute_get_active <- function(noagri = TRUE){


    # first 1975 and 1990
    L = commute_read_RP()
    if (noagri) {
        rp = L$noagri
    } else {
        rp = L$all
    }
    setkey(rp, CODGEO, year)

    ## get 2010 census data
    RP2010 <- data.table(readxl::read_xls(file.path(datadir(),"paris-commuting","base-cc-emploi-pop-active-2010.xls"),
                                         sheet = 1,
                                         skip = 5))
    RP2010 = RP2010[, .SD, .SDcols = c("P10_ACT1564","CODGEO","LIBGEO")]
    setnames(RP2010, 1, "workers")  # 15-64 years

    RP1999 <- data.table(readxl::read_xls(file.path(datadir(),"paris-commuting","base-cc-emploi-pop-active-2010.xls"),
                                          sheet = 3,
                                          skip = 5))
    RP1999 = RP1999[, .SD, .SDcols = c("P99_ACT1564","CODGEO","LIBGEO")]
    setnames(RP1999, 1, "workers")  # 15-64 years


        ## now use the official ones instead
    o1999 <- data.table(readxl::read_xls(file.path(datadir(),"paris-commuting","aires_urbaines-espaces_urbains-99.xls"),
                            sheet = 3,
                            skip = 1))
    o2010 <- data.table(readxl::read_xls(file.path(datadir(),"paris-commuting","AU2010_01-01-2019.xls"),
                              sheet = 2,
                              skip = 5))

    # old censuses dont have paris as 75056 but each arrondissement
    I = rp[,CODGEO > "75100" & CODGEO < "75121"]
    paris = rp[I,.(workers = sum(workers)),by = year]
    paris[, c("CODGEO","LIBELLE") := list("75056","PARIS")]

    rp <- rp[!I]
    rp <- rbind(rp,paris)

    # merge censes with AU classifications
    l = lapply(c(1975,1990,1999), function(x){
        z = merge(rp[year == x], o2010, by = "CODGEO", all.x = FALSE)
        z = z[AU2010 == "001"]
        z[, year := x]
        z
    })
    names(l) <- paste(c(1975,1990,1999))

    # RP 1999 and 2010
    tmp <- merge(RP1999, o2010, by = "CODGEO", all.x = FALSE)[AU2010 == "001"]
    tmp[,year := 1999]
    l$x1999 <- tmp
    tmp <- merge(RP2010, o2010, by = "CODGEO", all.x = FALSE)[AU2010 == "001"]
    tmp[,year := 2010]
    l$x2010 <- tmp

    l
}
