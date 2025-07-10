# functions to work with shlomo's data

shdir <- function(){file.path(datadir(), "shlomo-angel","Historical-cities","Paris")}

shyears <- function() c(1800,1832,1855,1880,1900,1928,1955,1979)
shfile <- function(yr){
    file.path(shdir(),paste0("Paris_ftpt_smallGaps_filled_",yr,".img"))
}
shload <- function(yr){
    raster::raster(shfile(yr))
}
# compute area for given raster in km2
shareayr <- function(r){
    m2 = sum(r[] > 0, na.rm = TRUE) * raster::res(r)[1]^2
    m2 / (1000*1000) #km2
}

# compute area for all years
sharea <- function(){
    d = data.table(year = shyears(), area_km2 = 0)
    for (iy in shyears()) {
        r = shload(iy)
        d[year == iy, area_km2 := shareayr(r)]
    }
    d
}

# combine with our measures
shcombine <- function(){
    d = sharea()
    d = d[,list(year,area = area_km2, pop = NA, type = "shlomo")]
    f = read_output()
    f <- f[LIBGEO == "Paris", list(year,pop,area,type)]
    x = rbind(d,f)
    x <- x[order(year)]
    fwrite(x, file.path(outdir(),"data","paris-shlomo.csv"))
    x
}


