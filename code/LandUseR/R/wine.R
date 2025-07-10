
AOC <- function(){"https://www.data.gouv.fr/fr/datasets/delimitation-des-aires-geographiques-des-siqo/"}

# "https://demo-terravisu.autonomens.com/visualiser/appellations-geographiques#layers=1c915fd4511bec597c5f3e2e14c30407"


writeAOCdta <- function(){
    a = haven::read_dta(file.path(datadir(), "communes-20190101","AOC_zips_Final_Tomerge.dta"))
    a$insee = stringr::str_pad(paste(a$insee),width = 5,pad = "0",side = "left")
    saveRDS(a,file.path(datadir(), "communes-20190101","AOC_zips_Final_Tomerge.Rds") )
}
readAOC <- function(){
    readRDS(file.path(datadir(), "communes-20190101","AOC_zips_Final_Tomerge.Rds"))
}

wine <- function(){
    w = readAOC()
    x = read_output()
    x[, AOC := CODGEO %in% w$insee]
    setkey(x, rank, year)
    diffs <- x[,.(pop = diff(pop),
                  ppop = 100*diff(pop) / pop[-.N],
                  area = diff(area),
                  parea = 100*diff(area) / area[-.N],
                  AOC = AOC[1],
                  year = paste(year[-.N],year[-1],sep = "-")),
               by = .(CODGEO, LIBGEO) ]
    list(wine = x, diffs = diffs)
}


wineplots <- function(){
    w = wine()
    d = w$wine[, list(area = mean(area), .N), by = .(AOC,year)]
    d[ , idx := rep(1:7,2)]
    p1 =  ggplot(d[year < 2016], aes(x = idx, y = area, fill = AOC)) + geom_bar(position = "dodge",stat = "identity" ) + scale_x_continuous(name = "year",breaks = 1:7, labels = unique(d$year)) + scale_y_continuous("Urban Area (km2)") +
        ggtitle("Cities Surrounded by Certified (AOC) Wine Land are Smaller in Area", subtitle = "77 Non-AOC and 23 AOC cities") + theme_bw()

    p2 = ggplot(w$diffs[,list(m = mean(parea)),by = .(year, AOC)], aes(x = year, y = m, color = AOC, group = AOC)) + geom_line() + scale_y_continuous("% increase in area") + geom_point() + ggtitle("% Increase in Area from year t to t+1")
}

wineplots_popclass <- function() {
    w = wine()
    d = w$wine[year < 2016]

    # split into interquartile ranges in each year
    dp = d[,.(CODGEO, pop, popclass = ggplot2::cut_number(pop, n = 3, labels = FALSE)),by = year]

    dp[, popclass_y := paste(year,popclass,sep = "_")]
    d = merge(d, dp[,.(CODGEO,year,popclass_y)], by = c("CODGEO","year"))

    d3 = d[,list(area = mean(area), density = mean(pop / area), .N), by = .(AOC,popclass_y)]
    d3[ , c("year","popclass") := tstrsplit(popclass_y, split = "_")]
    d3[ , popclass_y := NULL]
    yt = d3[, .(year = sort(unique(year), decreasing = FALSE), idx = 1:6)]
    d3 = merge(d3,yt, by = "year")

    qt = d3[, .(popclass = paste(1:3), popclasslab = c("pop: 0-25%","pop: 25%-75%","pop: 75%-100%"))]
    d3 = merge(d3,qt, by = "popclass")



    p1 =  ggplot(d3, aes(x = idx, y = area, fill = AOC)) + geom_bar(position = "dodge",stat = "identity" ) + scale_x_continuous(name = "year",breaks = yt$idx, labels = yt$year) + scale_y_continuous("Urban Area (km2)") + facet_grid(rows = vars(popclasslab), scales = "free_y") + theme_bw() + ggtitle("Urban Area by AOC Wine Classification",subtitle = "Grouping into interquartile ranges of Population by year.")
    p2 =  ggplot(d3, aes(x = idx, y = density, fill = AOC)) + geom_bar(position = "dodge",stat = "identity" ) + scale_x_continuous(name = "year",breaks = yt$idx, labels = yt$year) + scale_y_continuous("Urban Density") + facet_grid(rows = vars(popclasslab), scales = "free_y") + theme_bw() + ggtitle("Urban Density by AOC Wine Classification",subtitle = "Grouping into interquartile ranges of Population by year.")

    ggsave(plot = p1, filename = file.path(PRAdir(),"wine-popclass.pdf"), width = 9, height = 6)
    ggsave(plot = p2, filename = file.path(PRAdir(),"wine-popclass-density.pdf"), width = 9, height = 6)
    p1
}

wineplotsFE <- function(){
    w = wine()
    d = w$wine[year < 2016]
    f0 = fixest::feols(area ~ 1 | year * AOC, d)
    f0log = fixest::feols(log(area) ~ 1 | year * AOC, d)
    f = fixest::feols(area ~ pop | year * AOC, d)
    flog = fixest::feols(log(area) ~ log(pop) | year * AOC, d)
    fx = data.table(fixest::fixef(f))

    flm0 = lm(area ~ factor(year)*AOC, d)
    flm1log = lm(log(area) ~ log(pop) + factor(year)*AOC, d)
    flm1 = lm(area ~ pop + factor(year)*AOC, d)
    nd = d[,.(pop = median(pop)),by = .(year)]
    nd = rbind(nd,nd)
    nd$AOC <- rep(c(TRUE,FALSE),each = 6)
    nd$pred0 = predict(flm0,newdata = nd)
    nd$pred1 = predict(flm1,newdata = nd)
    nd$pred1log = predict(flm1log,newdata = nd)
    nd$idx = rep(1:6,2)

    p0 =  ggplot(nd[year < 2016], aes(x = idx, y = pred0, fill = AOC)) + geom_bar(position = "dodge",stat = "identity" ) + scale_x_continuous(name = "year",breaks = 1:6, labels = unique(nd$year)) + scale_y_continuous("Urban Area (km2)") +
        ggtitle("Cities Surrounded by Certified (AOC) Wine Land are Smaller in Area", subtitle = "Not controlling for pop") + theme_bw()

    p1 =  ggplot(nd[year < 2016], aes(x = idx, y = pred1, fill = AOC)) + geom_bar(position = "dodge",stat = "identity" ) + scale_x_continuous(name = "year",breaks = 1:6, labels = unique(nd$year)) + scale_y_continuous("Urban Area (km2)") +
        ggtitle("Cities Surrounded by Certified (AOC) Wine Land are Smaller in Area", subtitle = "Predicted at Median Pop in Each Year") + theme_bw()

    p1log =  ggplot(nd[year < 2016], aes(x = idx, y = pred1log, fill = AOC)) + geom_bar(position = "dodge",stat = "identity" ) + scale_x_continuous(name = "year",breaks = 1:6, labels = unique(nd$year)) + scale_y_continuous("Urban Area (km2)") +
        ggtitle("Cities Surrounded by Certified (AOC) Wine Land are Smaller in Area", subtitle = "Predicted at Median Pop in Each Year") + theme_bw()


    d = w$wine[, list(area = mean(area), .N), by = .(AOC,year)]
    d[ , idx := rep(1:7,2)]
    p1 =  ggplot(d[year < 2016], aes(x = idx, y = area, fill = AOC)) + geom_bar(position = "dodge",stat = "identity" ) + scale_x_continuous(name = "year",breaks = 1:7, labels = unique(d$year)) + scale_y_continuous("Urban Area (km2)") +
        ggtitle("Cities Surrounded by Certified (AOC) Wine Land are Smaller in Area", subtitle = "77 Non-AOC and 23 AOC cities") + theme_bw()

    p2 = ggplot(w$diffs[,list(m = mean(parea)),by = .(year, AOC)], aes(x = year, y = m, color = AOC, group = AOC)) + geom_line() + scale_y_continuous("% increase in area") + geom_point() + ggtitle("% Increase in Area from year t to t+1")
}

