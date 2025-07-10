
# Functions to read manual data from google drive.


gsheet <- function(){"https://docs.google.com/spreadsheets/d/1IotLzprM5Os-06MpfU895VOgHieYkQfY3WOLBBiTKZA/edit#gid=0"}
browse <- function(){googlesheets4::gs4_browse(gsheet())}

export_sheet <- function(startdate = "1876-01-01", topn = 200){
    p = readpop()
    p = p %>% dplyr::filter(date == startdate) %>% dplyr::arrange(desc(population))
    # p = p %>% dplyr::filter(year == 2015) %>% dplyr::arrange(desc(population))
    paris = p %>%
        dplyr::filter(CODGEO > "75100" & CODGEO < "751210") %>%
        dplyr::summarise(CODGEO = "75060", REG = REG[1], DEP = DEP[1], LIBGEO = "Paris", year = year[1], population = sum(population), date = date[1])
    p0 = p %>% dplyr::filter(date == startdate,(CODGEO < "75101" | CODGEO > "75120")) %>% dplyr::arrange(desc(population)) %>% dplyr::top_n(topn,population)
    # p0 = p %>% dplyr::filter(year == 2015,(CODGEO < "75101" | CODGEO > "75120")) %>% dplyr::arrange(desc(population)) %>% dplyr::top_n(topn,population)
    r = dplyr::bind_rows(p0,paris) %>% dplyr::arrange(desc(population)) %>% dplyr::mutate(rank = 1:nrow(.), area_EM = 0.0, area_1950 = 0.0,area_2016 = 0.0, pop_1876 = population, pop_1950 = 0)
    r = r %>%
        dplyr::select(rank,CODGEO,REG,DEP,LIBGEO,pop_1876, pop_1950, area_EM, area_1950,area_2016)
        # dplyr::filter(!(CODGEO %in% c("59512","78646","59599","93066","92044","92012","92051")))

    # ss = googlesheets4::read_sheet(gsheet())
    googlesheets4::write_sheet(r,ss = gsheet(),sheet = paste0("manual-collection-",year(startdate),"-top-",topn))
}

city_list <- function(startyear = "1975", topn = 200, overwrite = FALSE){
    if (overwrite){
        startdate = paste0(startyear,"-01-01")
        p = readpop()
        p = p %>% dplyr::filter(date == startdate) %>% dplyr::arrange(desc(population))
        # p = p %>% dplyr::filter(year == 2015) %>% dplyr::arrange(desc(population))
        paris = p %>%
            dplyr::filter(CODGEO > "75100" & CODGEO < "751210") %>%
            dplyr::summarise(CODGEO = "75060", REG = REG[1], DEP = DEP[1], LIBGEO = "Paris", year = year[1], population = sum(population), date = date[1])
        p0 = p %>% dplyr::filter(date == startdate,(CODGEO < "75101" | CODGEO > "75120")) %>% dplyr::arrange(desc(population)) %>% dplyr::top_n(topn-1,population)
        # p0 = p %>% dplyr::filter(year == 2015,(CODGEO < "75101" | CODGEO > "75120")) %>% dplyr::arrange(desc(population)) %>% dplyr::top_n(topn,population)
        r = dplyr::bind_rows(p0,paris) %>% dplyr::arrange(desc(population)) %>% dplyr::mutate(rank = 1:nrow(.))
        r = r %>%
            dplyr::select(rank,CODGEO,REG,DEP,LIBGEO,population) %>%
            data.table
        # dplyr::filter(!(CODGEO %in% c("59512","78646","59599","93066","92044","92012","92051")))

        if (startyear >= 1975){
            r = check_urban_area(r)
        }
        futile.logger::flog.info("effective %d cities", nrow(r))

        # ss = googlesheets4::read_sheet(gsheet())
        saveRDS(r,file = file.path(datadir(),paste0("top",topn,"-",startyear,".Rds")))

    } else {
        r = readRDS(file = file.path(datadir(),paste0("top",topn,"-",startyear,".Rds")))
    }
    r
}

check_urban_area <- function(d) {
    AU = data.table(readxl::read_xlsx(file.path(datadir(),"UU2020_au_01-01-2022.xlsx")), skip = 5, sheet = 2)
    AU[CODGEO == "75056" , CODGEO := "75060"]
    dau =  merge(d,AU[, -which(names(AU) %in% c("REG","DEP","LIBGEO")),with = FALSE], by = c("CODGEO"), all.x = TRUE, all.y = FALSE)
    dau[STATUT_2017 != "B" & DEP < 96]
}


import_sheet <- function(sheet = NULL){
    googlesheets4::read_sheet(gsheet(),sheet = sheet)
}

#' Get Manual Measurements
#'
#' @param reload TRUE/FALSE whether to fetch from google drive or from disk
#'
#' Notice in google drive sheet area2016 = 0 or \code{is.na(which_city_2016)} means the city historic has been absorbed
#' by another city in modern times. Hence we do not consider such a city.
get_manuals <- function(reload = FALSE){
    if (reload){
        s = import_sheet(sheet = "master-list") %>% dplyr::filter(area_2016 != 0.0 & is.na(which_city_2016))
        sd = data.table(s)
        sd[,which_city_2016 := NULL]
        sd[ , rank := order(pop_1876,decreasing = TRUE)]
        saveRDS(sd,file = file.path(datadir(),"top100.Rds"))
        data.table::fwrite(sd, file = file.path(datadir(),"top100.csv"))
    } else {
        sd = readRDS(file = file.path(datadir(),"top100.Rds"))
    }
    sd
}

table_manuals <- function(){
    g = get_manuals()
    ta = knitr::kable(g[,!"pop_1950"], format = "latex",
                      booktabs = TRUE,
                      longtable = TRUE,
                      caption = "100-city sample",
                      label = "top-100-cities"
    ) #%>% kableExtra::column_spec(5, width = "5cm")
    # cat(kableExtra::landscape(ta), file = file.path(datatables(), "france-pop-1950.tex"))
    cat(kableExtra::kable_styling(ta,latex_options = c("repeat_header")), file = file.path(datatables(), "top-100-sample.tex"))
}

table_top200 <- function(){
    g = bboxes_top200()
    g = g[rank > 100,!"extent"]

    googlesheets4::write_sheet(g,ss = gsheet(),sheet = paste0("top200"))

    ta = knitr::kable(g, format = "latex",
                      booktabs = TRUE,
                      longtable = TRUE,
                      caption = "City sample for cities ranked 101-200. Notice here the ranking is based on 1975 population, and the requirement that each city is still independent by 2020.",
                      label = "top-200-cities"
    ) %>% kableExtra::column_spec(1, width = "5cm")
    # cat(kableExtra::landscape(ta), file = file.path(datatables(), "france-pop-1950.tex"))
    cat(kableExtra::kable_styling(ta,latex_options = c("repeat_header")), file = file.path(datatables(), "top-200-sample.tex"))
}



writepop <- function(){
    pop = readxl::read_excel(file.path(datadir(),"base-pop-historiques-1876-2015.xls"),skip = 5)
    names(pop)[5:33] <- paste0(c(2015,2014	,2013,2012	,2011	,2010	,2009	,2008	,2007	,2006	,1999	,1990	,1982	,1975	,1968	,1962	,1954	,1936	,1931	,1926	,1921	,1911	,1906	,1901	,1896	,1891	,1886	,1881	,1876), "-01-01")
    pop %<>%
        tidyr::gather(year, population , -c(CODGEO,REG,DEP,LIBGEO)) %>%
        dplyr::mutate(date = as.Date(year), DEP = as.integer(DEP), REG = as.integer(REG), year = lubridate::year(date))
    saveRDS(pop,file.path(datadir(),"base-pop-historiques-1876-2015.Rds"))
    return(pop)
}

#' Read Census Data
#'
#' @export
readpop <- function(){
    readRDS(file.path(datadir(),"base-pop-historiques-1876-2015.Rds"))
}

#' Complement Census Data with Toutain
#'
#' This function complements INSEE population count data with Toutain's series.
#' The output of this is our measure for \emph{Population} in the model.
#'
#' Source: Toutain (1991) Tableau 1: Evolution de la Population des menages agricoles de 1789 a 1968
census_add_toutain <- function(){
    toutain = tribble(
        ~year, ~population, ~rural_pop, ~menage_agricoles,
        1700, 19, 16.1 , NA,
        1789, 27, 20.9, 18.2,
        1801, 27.5, 21.2,18.2,
        1821, 30.5, 23.4, 18.9,
        1846, 35.4, 26.8, 20.1,
        1861, 37.4, 26.6, 19.9,
        1872, 36.1, 24.0, 18.5,
        1881, 37.7, 24.6, 18.2,
        1891, 38.3, 24, 17.4,
        1901, 38.9, 23, 16.1,
        1911, 39.6, 22.1, 15.1,
        1921, 39.2, 21, 13.8,
        1931, 41.8, 20.4, 11.5,
        1936, 41.9, 19.9, 10.6,
        1946, 40.5, 19, 10.2,
        1954, 42.8, 18.8, 9.5,
        1962, 46.5, 18.8, 9.5,
        1968, 49.8, 17.2, 7.3
    )

    ce = readpop() %>%
        group_by(year) %>%
        summarise(INSEE = sum(population, na.rm = TRUE) / 1000000) %>%
        ungroup() %>%
        full_join(toutain %>%
                       select(year, population)) %>%
        select(year, population, INSEE ) %>%
        rename(toutain = population) %>%
        arrange(year)

    pce = ce %>%
        tidyr::pivot_longer(toutain:INSEE)
    ggplot(pce, aes(year,y = value, color = name)) + geom_line()
    ggsave(file.path(dataplots(), "France-population-insee-toutain.pdf"))

    cet = ce %>%
        mutate(population = pmax(toutain,INSEE,na.rm=T))

    cet %>%
        select(year, population) %>%
        readr::write_csv(path = file.path(datadir(), "France-population.csv"))

    ggplot(cet, aes(year, y = population)) +geom_line()
    ggsave(file.path(dataplots(), "France-population.pdf"))
}

