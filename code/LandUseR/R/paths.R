
# file path setup

#' Get Dropbox Root
#'
#' This returns the root of our shared dropbox folder on your machine.
#' On my computer this would be `~/Dropbox/research/LandUse`
#' @export
dboxdir <- function() {
    db = Sys.getenv("R_LANDUSE")
    if (db == ""){
        stop("You must set the environment variable R_LANDUSE in your ~/.Renviron file. It needs to point to the root of this replication package.")
    }
    db
}

# this folder is not included in the published replication package.
notshared <- function(){file.path(dboxdir(),"not-shared")}
notshareddata <- function(){file.path(notshared(),"data")}
notsharedoutput <- function(){file.path(notshared(),"output")}
notsharedplots <- function(){file.path(notshared(),"output","plots")}

datadir <- function(){file.path(dboxdir(),"data")}
appCdir <- function(){file.path(dboxdir(),"appendix-C-data")}
IVdir <- function(){file.path(datadir(),"schauberger-yields")}
AOCdir <- function(){file.path(datadir(),"AOC")}
GAEZdir <- function(){file.path(datadir(),"GAEZ")}
dataplots <- function(){file.path(outdir(),"data","plots")}
datatables <- function(){file.path(outdir(),"data","tables")}
entddir <- function(){file.path(datadir(),"ENTD")}
landvaldir <- function(){file.path(datadir(),"statistique-agricole","departement")}
PRAdir <- function(){file.path(datadir(),"statistique-agricole","petite-region-agricole")}
OCRdir <- function(){file.path(datadir(),"statistique-agricole","departement","nanonets-OCR-output")}
outdir <- function(){file.path(dboxdir(),"output")}
CASDdir <- function(){file.path(datadir(),"CASD")}
outdatadir <- function(){file.path(dboxdir(),"output","data")}
outmodeldir <- function(){file.path(dboxdir(),"output","model")}
instoutdir <- function(){system.file("data",package = "LandUseR")}
instindir <- function(){file.path(here::here(),"inst","data")}
yielddir <- function(){file.path(datadir(),"schauberger-yields")}
R2dir <- function(){file.path(dboxdir(),"RESTud","round2")}
igndir <- function(){file.path(dboxdir(),"data","IGN-chef-lieux")}

setup_gsheet <- function(){
    googledrive::drive_upload(file.path(datadir(),"france_cities.csv"), type = "spreadsheet")
}


cpthis <- function(to){
    loc = here::here()
    print(loc)
    file.copy(file.path(loc,"R"),file.path(to), recursive = TRUE)
    file.copy(file.path(loc,"man"),file.path(to), recursive = TRUE)
    file.copy(file.path(loc,"renv"),file.path(to), recursive = TRUE)
    file.copy(file.path(loc,"tests"),file.path(to), recursive = TRUE)
    file.copy(file.path(loc,"vignettes"),file.path(to), recursive = TRUE)
    file.copy(file.path(loc,"DESCRIPTION"),file.path(to))
    file.copy(file.path(loc,"NAMESPACE"),file.path(to))
    file.copy(file.path(loc,"renv.lock"),file.path(to))
    file.copy(file.path(loc,".Rprofile"),file.path(to))
}

citethis <- function(out = "../../"){
    grateful::cite_packages(output = "paragraph", out.dir = out, dependencies = TRUE)
}