# Libraries --------------------------------------------------

# close all connections
closeAllConnections()

# clear environment objects
rm(list = ls())

# Hello, you will have to edit this to be your own computer's working directories:
crs_data_dir <- "C:/Users/user/Dropbox/CGD/Projects/DAC_ODA_2019_spending/input/CRS"
input_dir <- "C:/Users/user/Dropbox/CGD/Projects/DAC_ODA_2019_spending/input"
output_dir <- "C:/Users/user/Dropbox/CGD/Projects/DAC_ODA_2019_spending/output"
setwd(crs_data_dir)

# Import packages. If it's a new package, automatically download it.
code2name <- function(x) {countrycode(x,"iso3c","country.name")}
name2code <- function(x) {countrycode(x,"country.name","iso3c")}
list.of.packages <-
  c( "tidyverse", "ggpubr", "ggrepel", "extrafont", "wbstats",
     "ggplot2", "data.table", "dplyr", "countrycode",
     "ggthemes", "rio", "RCurl", "foreign", "readxl", "XML",
     "dplyr", "forcats", "ggridges", "povcalnetR", "scales", "fst")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages, dependencies = TRUE)
for (package in list.of.packages) {
  library(eval((package)), character.only = TRUE)
}
closeAllConnections()

# set GGPLOT default theme:
theme_set(
  theme_clean() + 
    theme(plot.background = element_rect(color = "white"),
          legend.title = element_blank(),
          legend.background = element_rect(color = NA),
          legend.position = "top",
          legend.justification='left',
          legend.direction='horizontal'
          )
)

# create a function that STOPS my code if it runs into an error:
waitifnot <- function(cond) {
  if (!cond) {
    msg <- paste(deparse(substitute(cond)), "is not TRUE")
    if (interactive()) {
      message(msg)
      while (TRUE) {}
    } else {
      stop(msg)
    }
  }
}
code2name <- function(x) {countrycode(x,"iso3c","country.name")}
name2code <- function(x) {countrycode(x,"country.name","iso3c")}
name2region <- function(x) {countrycode(x,"country.name","un.region.name")}

# a function used for inserting line breaks into strings:
con <- function(string_) {cat(strwrap(string_, 60),sep="\n")}


# Options -----------------------------------------------------------------

use_fst <- TRUE

# Load CRS Data---------------------------------------------------
if (use_fst) {
  dac <- read.fst(dac.fst) %>% as.data.table()
} else {
  dac <- list()
  for (i_ in dir()) {
    toget <- paste0(crs_data_dir, "/", i_, "/", i_, ".txt")
    dac[[as.character(i_)]] <- fread(toget,
                                     sep = "|",
                                     header = T,
                                     quote = "",
    )
  }
  dac <- rbindlist(dac, idcol = TRUE)
  setwd(input_dir)
  fst::write.fst(dac, "dac.fst")
}

# Clean names ----------------------------------------------------
names(dac) <-  sub("\"", "", names(dac))
names(dac) <-  sub("\"", "", names(dac))
names(dac) <- tolower(names(dac))
dac <- as.data.table(dac)

# VALIDATION SET ----------------------------------------------------------

China_spending <- setDT(dac)[recipientname == "China (People's Republic of)" & donorname %in%c('Australia','Austria','United States','Germany','Japan','United Kingdom','France','Sweden','Netherlands','Canada','Norway','Switzerland','Italy','Korea','Denmark','Belgium','Spain','Ireland','Finland','New Zealand','Luxembourg','Poland','Portugal','Hungary','Czech Republic','Iceland','Greece','Slovak Republic','Slovenia') & sectorname != "Administrative Costs of Donors", .(sum(usd_grantequiv, na.rm = TRUE))] %>% unlist() %>% as.vector()

North_Korea_spending <- setDT(dac)[recipientname == "Democratic People's Republic of Korea" & donorname %in%c('Australia','Austria','United States','Germany','Japan','United Kingdom','France','Sweden','Netherlands','Canada','Norway','Switzerland','Italy','Korea','Denmark','Belgium','Spain','Ireland','Finland','New Zealand','Luxembourg','Poland','Portugal','Hungary','Czech Republic','Iceland','Greece','Slovak Republic','Slovenia') & sectorname != "Administrative Costs of Donors", .(sum(usd_grantequiv, na.rm = TRUE))] %>% unlist() %>% as.vector()

Kosovo_spending <- setDT(dac)[recipientname == "Kosovo" & donorname %in%c('Australia','Austria','United States','Germany','Japan','United Kingdom','France','Sweden','Netherlands','Canada','Norway','Switzerland','Italy','Korea','Denmark','Belgium','Spain','Ireland','Finland','New Zealand','Luxembourg','Poland','Portugal','Hungary','Czech Republic','Iceland','Greece','Slovak Republic','Slovenia') & sectorname != "Administrative Costs of Donors", .(sum(usd_grantequiv, na.rm = TRUE))] %>% unlist() %>% as.vector()

# Filter DAC -----------------------------------

dac <- dac[donorname %in% 
        c('Australia', 'Austria', 'United States', 'Germany', 'EU
              Institutions', 'Japan', 'United Kingdom', 'France',
          'Sweden', 'Netherlands', 'Canada', 'Norway', 'Switzerland',
          'Italy', 'Korea', 'Denmark', 'Belgium', 'Spain', 'Ireland',
          'Finland', 'New Zealand', 'Luxembourg', 'Poland',
          'Portugal', 'Hungary', 'Czech Republic', 'Iceland',
          'Greece', 'Slovak Republic', 'Slovenia'),]
# restrict to ODA flows only
dac <- dac[flowname %in% c("ODA Grants",
                           "ODA Loans",
                           "Equity Investment"), ]
# now get rid of administrative costs of donors
dac <- dac[sectorname != "Administrative Costs of Donors", ]
dac <- dac[, .(year,
               donorname,
               recipientname,
               flowname,
               usd_commitment,
               usd_grantequiv)]
# remove regional groups:
dac[, regions := grepl("regional", recipientname)]
dac <- dac[regions == FALSE]

# add in iso3c codes for donors and recipients to make merging easy
newcrosswalk <- data.table(recipientname = union(unique(dac$recipientname), unique(dac$donorname)))
newcrosswalk[, iso3c := countrycode(
  recipientname,
  origin = "country.name",
  destination = "iso3c",
  custom_match = c(
    "Bilateral, unspecified" = NA,
    "Kosovo" = "XKX",
    "Micronesia" = "FSM",
    "States Ex-Yugoslavia unspecified" = NA,
    "EU Institutions" = NA
  )
)]

before <- nrow(dac)
dac <- merge(dac,
             newcrosswalk,
             by = "recipientname",
             all.x = TRUE,
             allow.cartesian = FALSE)
waitifnot(before == nrow(dac))
before <- nrow(dac)

newcrosswalk <- rename(newcrosswalk, 
                       iso3c_d = iso3c,
                       donorname = recipientname)
dac <- merge(dac,
             newcrosswalk,
             by = "donorname",
             all.x = TRUE,
             allow.cartesian = FALSE)
waitifnot(before == nrow(dac))

dac <- dac[!is.na(iso3c) & !is.na(iso3c_d) &
             iso3c != "" &
             iso3c_d != "" &
             donorname != "" &
             recipientname != "" &
             flowname != "", ]
# check that spending in China/North Korea/Kosovo is 
# the same as we had prior to data cleaning
waitifnot(China_spending ==unlist(setDT(dac)[(iso3c == "CHN"), .(sum(usd_grantequiv, na.rm = TRUE))]))
waitifnot(North_Korea_spending ==unlist(setDT(dac)[(iso3c == "PRK"), .(sum(usd_grantequiv, na.rm = TRUE))]))
waitifnot(Kosovo_spending ==unlist(setDT(dac)[(iso3c == "XKX"), .(sum(usd_grantequiv, na.rm = TRUE))]))

# WDI import --------------------------------------------------------------
# Use the wb_stats library to get some useful info from WDI:

wdidata <- wb_data(
  country = "countries_only",
  indicator = c(
    # GDP per capita
    "gdppc" = "NY.GDP.PCAP.PP.CD",
    # GDP
    "GDP" = "NY.GDP.MKTP.CD",
    # Population
    "Population" = "SP.POP.TOTL",
    # Refugee population by DESTINATION
    "refugeepop.dest" = "SM.POP.REFG",
    # Refugee population by ORIGIN
    "refugeepop.orig" = "SM.POP.REFG.OR"
  ),
  return_wide = TRUE,
  start_date = 2014,
  end_date = 2019,
)
# drop n/a in iso3
wdidata <- wdidata %>% drop_na(iso3c)
# we don't want to include places that are regions like "World".
# These places often have ISO-2 codes that have numbers in them.
# so, create a new column indicating whether these columns should be deleted,
# and delete the columns.
wdidata$numiso <- as.numeric(gsub("([0-9]+).*$", "\\1", wdidata$iso2c))
wdidata <- wdidata[is.na(wdidata$numiso),]
# select the columns we're interested in:
wdidata <- wdidata[, c(
  "iso3c",
  "date",
  "gdppc",
  "GDP",
  "Population",
  "refugeepop.orig",
  "refugeepop.dest"
)]
# We want to get the most recent data available, with cutoff at 2015.
wdidata <- rename(wdidata, year = date)
setDT(wdidata)

# World Governance Indicators ---------------------------------------------
setwd(input_dir)
wgi <- rio::import("wgidataset.dta")
makeVlist <- function(dta) { 
  labels <- sapply(dta, function(x) attr(x, "label"))
  tibble(name = names(labels),
         label = labels)
}
namha <- makeVlist(wgi)
val_names <- namha$name[grep("Estimate", namha$label, value = FALSE)]
wgi <- setDT(wgi)[year>=2015,c("code", "year", val_names), with= FALSE] %>% as.data.table()
wgi[, wgi:= rowMeans(.SD, na.rm=T), .SDcols = val_names]
wgi <- wgi[!is.na(wgi),.(iso3c = code, year, wgi)]
setDF(wgi)

# DISTANCE, COLONIES ---------------------------------------------------------

setwd(input_dir)
# http://www.cgeh.nl/data#Geodist
cepii <- rio::import("dist_cepii.dta")
cepii <- cepii[, c("iso_o", "iso_d", "distcap", "colony")]
cepii <- dplyr::distinct(cepii)
cepii <- rename(cepii, "iso3c" = "iso_o", "iso3c_d" = "iso_d")
setDT(cepii)[,year:=2019]

# DEMOCRACY ---------------------------------------------------------------
# The Freedom House Democracy dataset is relatively clean. 

read.xl.sheets <- function(Test_Cases, sheet_start, skip) {
  names.init<-excel_sheets(Test_Cases)
  names.init <- names.init[sheet_start:length(names.init)]
  test.ex<-list()
  for (val in names.init) {
    test.ex[[val]]<-as.data.frame(readxl::read_excel(Test_Cases,sheet=val,skip=skip))
    test.ex[[val]] <- test.ex[[val]][,c("Country/Territory","Edition","Total")]
    test.ex[[val]] <- test.ex[[val]] %>% rename(
      "country" = "Country/Territory",
      "year" = "Edition",
      "total" = "Total"
    )
    test.ex[[val]]$total <- as.numeric(test.ex[[val]]$total)
  }
  test.ex <- lapply(test.ex, as.data.frame)
  test.ex
}

dem <- read.xl.sheets("All_data_FIW_2013-2021.xlsx", 2,1)
dem <- dem$`FIW13-21`
dem$country[grepl(" and Pr",dem$country)] <- "Sao Tome and Principe"
dem$iso3c <-
  countrycode(
    dem$country,
    "country.name",
    "iso3c",
    custom_match =
      c("Kosovo" = "XKX",
        "Micronesia" = "FSM",
        "Abkhazia" = NA,
        "Crimea" = NA,
        "Eastern Donbas" = NA,
        "Nagorno-Karabakh" = NA,
        "Somaliland" = NA,
        "South Ossetia" = NA,
        "Tibet" = NA,
        "Transnistria" = NA
        )
  )
dem <- na.omit(dem) %>% as.data.table()

# dem[,n:=.N,by=.(year,iso3c)]
# dem[n>=2,][order(iso3c)]

dem <- dem[!country%in%c("Northern Cyprus","Indian Kashmir", "Pakistani Kashmir"),]
dem[,total:=mean(total,na.rm=TRUE), by = .(iso3c, year)]
dem <- distinct(dem[,.(iso3c,year,total)])
dem <- dem[year>=2015 & year<=2019,] %>% as.data.table()

# TRADE LINKS -------------------------------------------------------
# This has bilateral trade between all countries. The way that
# it works is that it first gets the data from a URL. But, that data in the URL
# is imported through an XML format. We have a series of lines of code that
# turns that XML data set into a dataframe. I basically do copied this code
# from an online source, and I do not really understand what it's doing, but it
# ends up getting us the correct data. After I do that I set the names of the
# WTI data set to be lowercase and I remove some spaces. I also delete some of
# the country codes that are not actually countries. So for example WLD stands
# for 'world' and EUR stand for European Union. If you sum up the WTI data set
# by Imports, you'll see that the total number of imports is actually greater
# than 100% for each ISO code. This is because the WTI data set has more regions
# besides EUR and WLD in it that I did not have the time to delete. However,
# because we are doing a left merge onto our data set of CRS and funding, it
# doesn't really matter that this data set of the bilateral trade flows has
# some regions in it.
# Set link to website

waaati <- list()

for (i_ in seq(2014,2018)) {

link1 <-
  paste0(
    "http://wits.worldbank.org/API/V1/SDMX/V21/datasource/tradestats-trade/reporter/all/year/",
    i_,
    "/partner/all/indicator/XPRT-PRTNR-SHR"
  )
  
# Get data from webpage
data_prices <- getURL(link1)
# Parse XML data
xmlfile <- xmlParse(data_prices)
# Get place nodes
places <- getNodeSet(xmlfile, "//Series")
# Get values for each place
values <- lapply(places, function(x){
  # Get current place id
  pid <- xmlAttrs(x)
  # Get values for each gas type for current place
  newrows <- lapply(xmlChildren(x), function(y){
    # Get type and update time values
    attrs <- xmlAttrs(y)
    # Get price value
    price <- xmlValue(y)
    names(price) <- "price"
    # Return values
    return(c(pid, attrs, price))
  })
  # Combine rows to single list
  newrows <- do.call(rbind, newrows)
  # Return rows
  return(newrows)
})
# Combine all values into a single dataframe
df <- as.data.frame(do.call(rbind, values), stringsAsFactors = FALSE)
# Clean:
wti <- as.data.table(df)
names(wti) <- tolower(make.names(names(wti)))
wti <- wti[,.(iso3c_d = reporter, iso3c = partner, exports = obs_value)]
wti <- wti[!(iso3c%in%c("WLD","EUR","UNS","SSF","FRE","EAS")) &
             !(iso3c_d%in%c("WLD","EUR","FRE","UNS","SSF","EAS")),]
wti[,exports:=as.numeric(exports)]
wti <- na.omit(wti)
wti[,year:=i_]
waaati[[as.character(i_)]] <- wti %>% as.data.table()
}
wti <- waaati[lengths(waaati) != 0]
wti <- rbindlist(waaati) %>% as.data.table()
waaati <- NULL


# POVCALNET ---------------------------------------------------------------

# This downloads data from povcal. I include every country
# and get the latest year of data (dropping data before
# 2015). The output result is the headcount of people under a
# certain level of poverty. Here I get $3.8 and $1.9 dollars
# poverty.

getpovhed <- function(rate_) {
  a <- povcalnet(
    "all",
    rate_,
    "all",
    aggregate = FALSE,
    fill_gaps = FALSE,
    coverage = "national"
  )
  setDT(a)
  a[, maxyr := max(year, na.rm = TRUE), by = countrycode]

  # population is in millions
  # get the total number of ppl under a certain range
  a[,(paste0("pov",rate_)):=population * headcount * (10 ^ 6)]
  a <- rename(a, iso3c = countrycode,
              year = year)
  a <- a[year>=2010,c("iso3c", "year", paste0("pov",rate_)),with=FALSE]
  # if both income and consumption measures for something exist, 
  # then take the mean of the two:
  a[,(paste0("pov",rate_)):= mean(unlist(.SD), na.rm=TRUE),by = .(iso3c,year) ,.SDcols = paste0("pov",rate_)]
  
  # IF the ONLY data we have is 2013 OR earlier, then use that data
  # and label it as 2014 data.
  # IF, however, we have more than 1 data point AFTER 2013, then keep
  # all those data points.
  a[,before_2013:= year<=2013,by=iso3c]
  a[,n:=.N,by = .(iso3c, before_2013)]
  a[,n_tot:=sum(n,na.rm=T) ,by=iso3c]
  a[(n_tot==n) & (before_2013==TRUE),year:=2014]
  a <- a[!(before_2013==TRUE),]
  a[,`:=`(before_2013 = NULL, n = NULL, n_tot = NULL)]
  
  a <- distinct(a)
  a
}

pov1 <- getpovhed(1.9) %>% as.data.table()
pov2 <- getpovhed(3.8) %>% as.data.table()

# MERGE ------------------------------------------------------
# Finally, we merge all of the data together. The easiest way
# to do this is by using the 'Reduce' function. The 'Reduce'
# function basically does the same operation on a list of
# objects. So, if you 'Reduce' merge, then you would merge
# many dataframes together. Obviously, this is very fraught
# process, because, for example, in a many-to-many merge,
# there are tons of issues that arise. So, what we do is we
# first make sure that the ISO codes in our data frames that
# we want to merge with our DAC data set are not duplicated.
# So that is what I'm doing in the first two lines with the
# waitifnot function. Again, the waitifnot function is the
# same as the assert function in STATA or Python. After that,
# I want to make sure that the number of rows that I'm adding
# to my data set is 0. This is because I want to make sure
# that I'm doing a many-to-one merge, and I don't want to be
# creating new rows by having a cartesian product of each of
# the rows. So, I make sure that I define an object that
# counts the number of rows in my data set, and then after
# I've done all of the merging, I check to see that the
# number of rows is the same.
#
# I do this entire process two times, because there are two
# types of data sets. The first type of data set is where I'm
# just describing the recipient country. So, this would
# include the World Development Indicators database. The
# second group of data is where I have data on BOTH the donor
# and the recipient. So, this includes data about the
# distance between each of those two country's capitals. In
# this case, I'm using the exact same code as I had before,
# except I am merging by both the donor ISO3c ID as well as
# the recipient country ISO3c ID. I'm also making sure that
# the number of rows that I had before is the same as the
# number of rows that I have after.
#
# Finally after all this, I make sure that I saved the
# dataset as a STATA readable DTA file. It is important to
# note that iso3c variable stands for the recipient country
# ISO codes. The iso3c_d variable stands for the donor
# country ISO code.

list_all_isos <- countrycode::codelist$iso3c %>% unique %>% na.omit
list_all_isos <- union(list_all_isos, dac$iso3c)
exp_grid <- CJ(unique(dac$iso3c_d), list_all_isos, seq(2014,2019,1)) %>% as.data.table()
dac_collapsed <- dac %>% as.data.table()
dac_collapsed <-
  dac_collapsed[, .(
    usd_commitment = sum(usd_commitment, na.rm = TRUE),
    usd_grantequiv = sum(usd_grantequiv, na.rm = TRUE)
  ),
  by = .(iso3c,
         iso3c_d,
         year)]

exp_grid <- merge(exp_grid[,.(iso3c_d = V1, iso3c = list_all_isos, year = V3)], dac_collapsed, 
                  all.x=TRUE,by = c("iso3c_d", "iso3c", "year"))
exp_grid[is.na(usd_commitment),usd_commitment:=0]
exp_grid[is.na(usd_grantequiv),usd_grantequiv:=0]

df_list <- list(exp_grid, wdidata, dem, wgi, pov1, pov2)
waitifnot(nrow(wdidata[,.(iso3c, year)])==nrow(distinct(wdidata[,.(iso3c, year)])))
waitifnot(nrow(setDT(pov1)[,.(iso3c, year)])==nrow(distinct(pov1[,.(iso3c, year)])))
waitifnot(nrow(setDT(pov2)[,.(iso3c, year)])==nrow(distinct(pov2[,.(iso3c, year)])))
waitifnot(nrow(setDT(dem)[,.(iso3c, year)])==nrow(distinct(dem[,.(iso3c, year)])))
waitifnot(nrow(setDT(wgi)[,.(iso3c, year)])==nrow(distinct(wgi[,.(iso3c, year)])))

before <- nrow(exp_grid)
exp_grid <- Reduce(function(d1, d2) merge(d1, d2, by = c("iso3c", "year"),
                                          all.x = TRUE,
                                          allow.cartesian = FALSE),
                   df_list)
waitifnot(nrow(exp_grid) == before)
df_list_2 <- list(exp_grid,cepii, wti)
before <- nrow(exp_grid)
exp_grid <- Reduce(function(d1, d2) merge(d1, d2, by = c("iso3c", "iso3c_d", "year"),
                                          all.x = TRUE,
                                          allow.cartesian = FALSE),
                   df_list_2)
waitifnot(nrow(exp_grid) == before)
# FURTHER DATA CLEANING ---------------------------------------------------
# check that spending in China/North Korea/Kosovo is 
# the same as we had prior to data cleaning
waitifnot(
  abs(
    China_spending - unlist(setDT(exp_grid)[(iso3c == "CHN"), .(sum(usd_grantequiv, na.rm = TRUE))])
  ) < 0.1
)
waitifnot(
  abs(
    North_Korea_spending - unlist(setDT(exp_grid)[(iso3c == "PRK"), .(sum(usd_grantequiv, na.rm = TRUE))])
  ) < 0.1
)

waitifnot(
  abs(
    Kosovo_spending - unlist(setDT(exp_grid)[(iso3c == "XKX"), .(sum(usd_grantequiv, na.rm = TRUE))])
  ) < 0.1
)

# EXPORT ------------------------------------------------------------------
exp_grid %>% foreign::write.dta(., "dac.dta")

# GGridges for DAC ODA ----------------------------------------------------

bob <- dac %>%
  filter(donorname %in% c("Australia", "Canada","France","Germany","Japan","Korea","Netherlands","Norway","United Kingdom","United States")) %>% 
  as.data.table()
bob <- bob[,.(donorname, recipientname, usd_grantequiv, GDP, Population, refugeepop.dest)]
bob[,gdppc:=GDP/Population]
bob[,totcom:=sum(usd_grantequiv,na.rm=TRUE),by=donorname]
bob[,usd_grantequiv:=usd_grantequiv/totcom]
bob <- bob[usd_grantequiv>0,]
bob[,comit:=usd_grantequiv]
invisible(lapply(names(bob),function(.name) set(bob, which(is.infinite(bob[[.name]])), j = .name,value =NA)))
bob <- na.omit(bob)
bob[,temp:=comit*gdppc]
bob[donorname=="United Kingdom", donorname:="UK"]
bob[donorname=="United States", donorname:="US"]
ordered <- as.vector(unlist(bob[,.(sum(temp)), by = donorname][order(V1)][,.(donorname)]))
bob$donorname <- factor(bob$donorname, levels = ordered)

plot1 <- ggplot(bob, aes(gdppc, weight = comit)) + 
  geom_histogram(bins=40, fill = "aquamarine4", color = "white") + 
  # theme_clean() +
  theme(legend.background = element_blank())+
  theme(plot.background = element_rect(color = "white"))+
  labs(
    x = "Recipient Country Per Capita GDP (2019 Current USD)",
    y = "Donor",
    title = "Where does DAC Official Development Assistance go?",
    caption = "Source: OECD Common Reporting Standard (CRS). All figures are grant equivalent spending."
  ) + 
  scale_fill_colorblind()+
  scale_x_continuous(labels = scales::dollar_format(), breaks = seq(0,14000,2000),
                     limits = c(0,14000)) + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y=element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_line( size=.5, color="grey", linetype = "dotted"),
    axis.title=element_text(size=12,face="bold")
  )+
  facet_grid(donorname~., switch="both")


ggsave("ridges_histogram.png", plot1, dpi=1000, width = 8, height = 10)

# Creating graph of final results from rotated table:--------------

# List our of all the regressions
files_rs <- c(
"r_squared_2019 Logistic_usd_commitment.xlsx",
"r_squared_2019 Logistic_usd_grantequiv.xlsx",
"r_squared_2019 OLS ln_usd_commitment.xlsx",
"r_squared_2019 OLS ln_usd_grantequiv.xlsx",
"r_squared_2019 OLS positive_usd_commitment.xlsx",
"r_squared_2019 OLS positive_usd_grantequiv.xlsx",
"r_squared_2019 OLS_usd_commitment.xlsx",
"r_squared_2019 OLS_usd_grantequiv.xlsx",
"r_squared_2019 Poisson positive_usd_commitment.xlsx",
"r_squared_2019 Poisson positive_usd_grantequiv.xlsx",
"r_squared_2014-19 OLS ln_usd_commitment.xlsx",
"r_squared_2014-19 OLS ln_usd_grantequiv.xlsx",
"r_squared_2014-19 OLS_usd_commitment.xlsx",
"r_squared_2014-19 OLS_usd_grantequiv.xlsx"
)

# read these into a list
rsl <- lapply(files_rs, read_xlsx)
names(rsl) <- files_rs

# delete columns with "_N", and get the mean and standard deviation:
clean_rsq <- function(x) {
  # remove the columns that have _N in them, indicating that they're about
  # sample sizes:
    toremove <- grep("_N", names(x), value = T)
    tokeep <- setdiff(names(x), toremove)
  x <- x[, tokeep]

  # convert from wide to long so that we have regression r squared values for
  # each type of regression
  x <-
    gather(
      x,
      "reg",
      "rsq",
      `GDP per capita, Population`:`Full regression (distance, colony, exports)`
    )

  # get the mean, min, and max r squared of all of these regressions by the
  # country (or all dac / top 10 dac regressions):
  x <-
    setDT(x)[, .(
      estimate = mean(rsq, na.rm = T),
      min = min(rsq, na.rm = T),
      max = max(rsq, na.rm = T)
    ), by = Country]
  x <- as.data.table(x)
  x
}

# create a separate data.table for the non-r-squared values: we will later
# graph the 'rsl' data.table independently.

exprsl <- rsl %>% copy
rsl <- lapply(rsl, clean_rsq)
rsl <- rbindlist(rsl, idcol = "regression")
exprsl <- rbindlist(exprsl, idcol = "regression")

# function that cleans the outcome and regression names:
clean_outcome_name <- function(x) {
    x <- as.data.table(x)
    x[, outcome := regression %>%
          grepl("usd_commitment", .) %>%
          as.character() %>%
          gsub("TRUE", "Commitment", .) %>%
          gsub("FALSE", "Grant Equiv", .)]
    x[, regression :=
          regression %>%
          gsub("r_squared_", "", .) %>%
          gsub("_usd_commitment.xlsx", "", .) %>%
          gsub("_usd_grantequiv.xlsx", "", .) %>%
          gsub("OLS ln", "OLS Log", .) %>%
          gsub("Poisson positive", "Poisson", .)]
    x <- x %>% rename(donor = Country) %>% as.data.table()
    x
}

rsl <- clean_outcome_name(rsl) %>% as.data.table()
exprsl <- clean_outcome_name(exprsl) %>% as.data.table()
tokeep <- grep("_N", names(exprsl), value = T)
tokeep <- setdiff(names(exprsl), tokeep)
exprsl <- setDF(exprsl)[, tokeep]
exprsl <-
    gather(
      exprsl,
      "reg",
      "rsq",
      `GDP per capita, Population`:`Full regression (distance, colony, exports)`
    ) %>% as.data.table()
exprsl <- exprsl[regression %in%
                   c("2019 OLS Log",
                     "2019 OLS positive",
                     "2019 OLS",
                     "2014-19 OLS Log",
                     "2014-19 OLS")]
exprsl <- exprsl[, .(estimate = mean(rsq),
                     max = max(rsq),
                     min = min(rsq)), by = .(donor, reg)][order(donor)] %>% 
  as.data.table()

exprsl[, donor :=
         donor %>%
         countrycode(
           .,
           origin = "iso3c",
           destination = "country.name",
           custom_match = c("ALL DAC" = "All DAC",
                            "TOP TEN DAC" = "Top 10 DAC")
         )]

top <- c("Top 10 DAC", "All DAC")

exprsl[, donor :=
         factor(donor,
         levels = c(sort(setdiff(unique(exprsl$donor), top)), top)
         )]

exprsl[, reg := factor(
  reg,
  levels = c(
    "Full regression (distance, colony, exports)",
    "GDP per capita, Population, Refugee In/Outflows, Governance Scores",
    "GDP per capita, Population, Refugee In/Outflows",
    "GDP per capita, Population"
  )
)]

# this is a function that defines the ggplot error bars
duh_points_and_deh_wiscers <- function(reg_, nudge_) {
  list(
    geom_point(
      data = exprsl[reg == reg_],
      aes(
        x = estimate,
        y = donor,
        color = factor(
          reg,
          levels = c(
            "Full regression (distance, colony, exports)",
            "GDP per capita, Population, Refugee In/Outflows, Governance Scores",
            "GDP per capita, Population, Refugee In/Outflows",
            "GDP per capita, Population"
          )
        )
      ),
      position = position_nudge(y = nudge_)
    ),
    geom_errorbarh(
      data = exprsl[reg == reg_],
      height = 0,
      aes(
        xmin = min,
        xmax = max,
        y = donor,
        color = factor(
          reg,
          levels = c(
            "Full regression (distance, colony, exports)",
            "GDP per capita, Population, Refugee In/Outflows, Governance Scores",
            "GDP per capita, Population, Refugee In/Outflows",
            "GDP per capita, Population"
          )
        )
      ),
      position = position_nudge(y = nudge_)
    )
  )
}


plot_nice <- ggplot() +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "grey72") +
  duh_points_and_deh_wiscers("GDP per capita, Population, Refugee In/Outflows, Governance Scores",-0.08) +
  duh_points_and_deh_wiscers("Full regression (distance, colony, exports)", -0.2) +
  duh_points_and_deh_wiscers("GDP per capita, Population", 0.2) +
  duh_points_and_deh_wiscers("GDP per capita, Population, Refugee In/Outflows", 0.08) +
  labs(
    x = "",
    y = "",
    title = "R-Squared Across Countries",
    subtitle = paste(
      strwrap(
        "Each interval shows the minimum, maximum, and mean R-squared values from all OLS regressions (zeros excluded, zeros included, logged outcome) across all years (2014-2019 or 2019) and all outcome variables (grant equivalent spending, commitments). Logistic and Poisson robustness regressions were excluded.",
        65
      ),
      collapse = "\n"
    )
  ) +
  # facet_grid(donor ~., scales = "free") +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    plot.subtitle = element_text(
      size = 8,
      hjust = 0,
      color = "black"
    ),
    legend.text = element_text(
      size = 8,
      hjust = 0,
      color = "black"
    ),
    legend.key.size = unit(0.1, 'lines')
  ) +
  guides(colour = guide_legend(ncol = 1)) +
  # scale_x_continuous(breaks = pretty_breaks(5))+
  scale_color_discrete(
    limits = c(
      "GDP per capita, Population",
      "GDP per capita, Population, Refugee In/Outflows",
      "GDP per capita, Population, Refugee In/Outflows, Governance Scores",
      "Full regression (distance, colony, exports)"
    )
  ) + 
  scale_x_continuous(limits = c(0,1))

ggsave(
  paste0("all dac regression plot whisker.png"),
  plot_nice,
  width = 5,
  height = 7,
  limitsize = FALSE,
  dpi = 320
)

graph_some_df <- function(some_df, var_, file_ = "") {
  if (var_ == "R-squared") {
    sub_ <-
      "Each interval represents the minimum and maximum R-squared values from 4 regressions. The 4 regressions are 1) outcome on GDP per capita and Population 2) outcome on GDP per capita, population, and refugee in/outflows 3) outcome on GDP per capita, population, refugee in/outflows, and governance scores 4) full regression, including variables on distance, colony, and exports. For logistic and poisson regressions, McFadden's adjusted R-squared was used."
  } else {
    sub_ <-
      "Each interval represents the 95 percent confidence interval for this coefficient. Robust standard errors were used on the OLS and Poisson estimates."
  }
  
  
  plot <- ggplot() +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = "grey72") +
    geom_point(
      data = some_df[outcome == "Grant Equiv"],
      aes(
        x = estimate,
        y = countrycode(
          donor,
          origin = "iso3c",
          destination = "country.name",
          custom_match = c("ALL DAC" = "All DAC",
                           "TOP TEN DAC" = "Top 10 DAC")
        ),
        color = outcome
      ),
      position = position_nudge(y = 0.2)
    ) +
    geom_errorbarh(
      data = some_df[outcome == "Grant Equiv"],
      height = .2,
      aes(
        xmin = min,
        xmax = max,
        color = outcome,
        y = countrycode(
          donor,
          origin = "iso3c",
          destination = "country.name",
          custom_match = c("ALL DAC" = "All DAC",
                           "TOP TEN DAC" = "Top 10 DAC")
        )
      ),
      position = position_nudge(y = 0.2)
    ) +
    geom_point(
      data = some_df[outcome == "Commitment"],
      aes(
        x = estimate,
        y = countrycode(
          donor,
          origin = "iso3c",
          destination = "country.name",
          custom_match = c("ALL DAC" = "All DAC",
                           "TOP TEN DAC" = "Top 10 DAC")
        ),
        color = outcome
      ),
      position = position_nudge(y = -0.2)
    ) +
    geom_errorbarh(
      data = some_df[outcome == "Commitment"],
      height = .2,
      aes(
        xmin = min,
        xmax = max,
        color = outcome,
        y = countrycode(
          donor,
          origin = "iso3c",
          destination = "country.name",
          custom_match = c("ALL DAC" = "All DAC",
                           "TOP TEN DAC" = "Top 10 DAC")
        )
      ),
      position = position_nudge(y = -0.2)
    ) +
    labs(
      x = "",
      y = "",
      title = var_,
      subtitle = paste(strwrap(sub_, 97),
                       collapse = "\n"),
      caption = file_
    ) +
    facet_grid( ~ regression, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous(breaks = pretty_breaks(5))
  
  ggsave(
    paste0("all dac regression plot", make.names(var_), make.names(file_), ".pdf"),
    plot,
    width = 10,
    height = 6,
    limitsize = FALSE,
    dpi = 320
  )
}

graph_some_df(rsl, "R-squared")

for (file_ in c("total_regressions59.txt", "pov_regressions59.txt")) {
  sally <- fread(file_)
  if (file_ == "pov_regressions59.txt") {
    reg_type <- "Poverty headcount Regression"
  }
  if (file_ == "total_regressions59.txt") {
    reg_type <- "GDP per capita  Regression"
  }
  sally <- t(sally) %>% as.matrix
  sally[sally == ""] <- NA
  sally[sally == "-"] <- NA
  names.sally <-
    coalesce(sally[2,], paste0(shift(sally[2,]), " Standard Error"))
  sally <- as.data.table(sally[3:nrow(sally),])
  names(sally) <- names.sally
  sally <-
    setDF(sally)[,!as.vector(unlist(lapply(sally, function(x)
      sum(is.na(
        x
      )))) == nrow(sally))]
  sally <- sally %>% as.data.table()
  todelete <- grep("NA Standard", names(sally), value = T)
  todelete <- c(todelete, "Constant", "Constant Standard Error")
  sally[, (todelete) := lapply(.SD, function(x)
    x = NULL), .SDcols = todelete]
  tonumeric <- setdiff(names(sally), "LABELS")
  sally[, (tonumeric) := lapply(.SD, function(x)
    gsub("\\(", "", x) %>%
      gsub("\\)", "", .) %>%
      gsub("\\*", "", .) %>%
      as.numeric) , .SDcols = tonumeric]
  sally[, LABELS := LABELS %>% gsub("All DAC", "All", .) %>%
          gsub("2014-19", "2014", .)]
  sally[, `outcome var` := grepl("usd_grantequiv", sally$LABELS) %>%
          as.character %>% gsub("TRUE", "Grant Equiv", .) %>%
          gsub("FALSE", "Commitment", .)]
  sally[, LABELS := LABELS %>% gsub("usd_grantequiv", "", .) %>% gsub("usd_commitment", "", .)]
  sally[, year := as.numeric(unlist(regmatches(
    LABELS, gregexpr("[[:digit:]]+", LABELS))))]
  
  newcols <- sally$LABELS %>% strsplit(., "2014 |2019 ") %>% as.data.table() %>% t %>% as.matrix() %>% gsub("\\_","",.) %>% as.data.frame()
  newcolnames <- c("donor","regression")
  
  sally[,(newcolnames):=newcols]
  sally[, donor := donor %>% gsub("All", "ALL DAC", .)]
  sally[, LABELS := NULL]
  sally[, year := year %>% as.character %>% gsub("2014", "2014-19", .)]
  sally[, regression := paste0(year, " ", regression)]
  
  bob <- as.data.table(sally)
  
  vec.vars <-
    grep("Standard", names(bob), value = T) %>%
    gsub(" Standard Error", "", .) %>%
    setdiff(., "Constant")
  
  list.se <- list()
  
  for (var_ in vec.vars) {
    bob <- as.data.table(bob)
    temp <-
      bob[,
          c("donor",
            var_,
            paste0(var_, " Standard Error"),
            "regression",
            "outcome var"), with = FALSE] %>% as.data.table()
    names(temp) <-
      c("donor", "estimate", "se", "regression", "outcome")
    temp[, estimate := estimate %>% gsub("*", "", ., fixed = T) %>%
           as.numeric]
    temp[, se := se %>% gsub("(", "", ., fixed = T) %>%
           gsub(")", "", ., fixed =
                  T) %>%
           as.numeric]
    temp <- na.omit(temp)
    list.se[[var_]] <- temp %>% as.data.table()
    
    temp[, regression :=
           regression %>%
           gsub("OLS ln", "OLS Log", .)
         %>%
           gsub("Poisson positive", "Poisson", .)]
    
    # temp[,regression:=regression %>%
    #        factor(.,
    #               levels = c("2019 OLS",
    #                          "2014-19 OLS",
    #                          "2019 OLS Log",
    #                          "2019 Poisson"))]
    temp <- na.omit(temp)
    
    temp[, min := estimate - 2 * se]
    temp[, max := estimate + 2 * se]
    
    graph_some_df(temp, var_, file_)
    
  }
}
# ses <- rbindlist(list.se, fill = TRUE, idcol = "variable")
# ses[,se.div.est:=se/estimate]
# ses[donor=="ALL DAC",][order(abs(se.div.est))]
