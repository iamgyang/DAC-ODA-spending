##Library----

# close all connections
closeAllConnections()

# clear environment objects
rm(list = ls())

# Hello, you will have to edit this to be your own computer's working directories:
crs_data_dir <- "C:/Users/gyang/Dropbox/CGD/Projects/DAC_ODA_2019_spending/input/CRS 2019 data"
input_dir <- "C:/Users/gyang/Dropbox/CGD/Projects/DAC_ODA_2019_spending/input"
output_dir <- "C:/Users/gyang/Dropbox/CGD/Projects/DAC_ODA_2019_spending/output"
setwd(crs_data_dir)

# Import packages. If it's a new package, automatically download it.
list.of.packages <-
  c( "tidyverse", "ggpubr", "ggrepel", "extrafont", "WDI",
     "ggplot2", "data.table", "dplyr", "countrycode",
     "ggthemes", "rio", "RCurl", "foreign", "readxl", "XML",
     "dplyr", "forcats", "ggridges", "povcalR" )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages, dependencies = TRUE)
for (package in list.of.packages) {
  library(eval((package)), character.only = TRUE)
}
closeAllConnections()

# set GGPLOT default theme:
theme_set(theme_clean() + theme(plot.background = element_rect(color = "white")))

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

#Load CRS Data----
crs <- fread(
  "CRS 2019 data.txt",
  sep = "|",
  header = T,
  quote = "",
)

## Clean names ----
names(crs) <-  sub("\"", "", names(crs))
names(crs) <-  sub("\"", "", names(crs))
names(crs) <- tolower(names(crs))
crs <- as.data.table(crs)

# VALIDATION SET ----------------------------------------------------------

China_spending <- setDT(crs)[year == 2019 &recipientname == "China (People's Republic of)" & donorname %in%c('Australia','Austria','United States','Germany','Japan','United Kingdom','France','Sweden','Netherlands','Canada','Norway','Switzerland','Italy','Korea','Denmark','Belgium','Spain','Ireland','Finland','New Zealand','Luxembourg','Poland','Portugal','Hungary','Czech Republic','Iceland','Greece','Slovak Republic','Slovenia') & sectorname != "Administrative Costs of Donors" & flowname == "ODA Grants", .(sum(usd_grantequiv, na.rm = TRUE))] %>% unlist() %>% as.vector()

North_Korea_spending <- setDT(crs)[year == 2019 &recipientname == "Democratic People's Republic of Korea" & donorname %in%c('Australia','Austria','United States','Germany','Japan','United Kingdom','France','Sweden','Netherlands','Canada','Norway','Switzerland','Italy','Korea','Denmark','Belgium','Spain','Ireland','Finland','New Zealand','Luxembourg','Poland','Portugal','Hungary','Czech Republic','Iceland','Greece','Slovak Republic','Slovenia') & sectorname != "Administrative Costs of Donors" & flowname == "ODA Grants", .(sum(usd_grantequiv, na.rm = TRUE))] %>% unlist() %>% as.vector()

Kosovo_spending <- setDT(crs)[year == 2019 &recipientname == "Kosovo" & donorname %in%c('Australia','Austria','United States','Germany','Japan','United Kingdom','France','Sweden','Netherlands','Canada','Norway','Switzerland','Italy','Korea','Denmark','Belgium','Spain','Ireland','Finland','New Zealand','Luxembourg','Poland','Portugal','Hungary','Czech Republic','Iceland','Greece','Slovak Republic','Slovenia') & sectorname != "Administrative Costs of Donors" & flowname == "ODA Grants", .(sum(usd_grantequiv, na.rm = TRUE))] %>% unlist() %>% as.vector()

## Filter DAC ----
dac <-
  crs %>%
  as.data.frame() %>% 
  filter(donorname %in% 
           c('Australia', 'Austria', 'United States', 'Germany', 'EU
              Institutions', 'Japan', 'United Kingdom', 'France',
             'Sweden', 'Netherlands', 'Canada', 'Norway', 'Switzerland',
             'Italy', 'Korea', 'Denmark', 'Belgium', 'Spain', 'Ireland',
             'Finland', 'New Zealand', 'Luxembourg', 'Poland',
             'Portugal', 'Hungary', 'Czech Republic', 'Iceland',
             'Greece', 'Slovak Republic', 'Slovenia')
  ) %>% 
  as.data.frame() %>% 
  # restrict to ODA flows only
  filter(flowname %in% c("ODA Grants",
                         "ODA Loans",
                         "Equity Investment")) %>%
  # now get rid of administrative costs of donors
  filter(sectorname != "Administrative Costs of Donors") %>%
  dplyr::select(
    c(
      "year",
      "donorname",
      "recipientname",
      "flowname",
      "usd_commitment",
      "usd_grantequiv"
    )
  ) %>%
  # remove regional groups:
  mutate(regions = grepl("regional", recipientname)) %>%
  filter(regions == FALSE) %>%
  #add in iso3c codes for donors and recipients to make merging easy
  mutate(
    iso3c = countrycode(
      recipientname,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Bilateral, unspecified" = NA,
        "Kosovo" = "XKX",
        "Micronesia" = "FSM",
        "States Ex-Yugoslavia unspecified" = NA
      )
    )
  ) %>%
  mutate(iso3c_d = countrycode(
    donorname,
    origin = "country.name",
    destination = "iso3c",
    custom_match = c("EU Institutions" = NA)
  )) %>%
  filter(
    !is.na(iso3c) & !is.na(iso3c_d) &
      iso3c != "" &
      iso3c_d != "" & 
      donorname != "" & 
      recipientname != "" & 
      flowname != ""
    )

# check that spending in China/North Korea/Kosovo is 
# the same as we had prior to data cleaning
waitifnot(China_spending ==unlist(setDT(dac)[(iso3c == "CHN") & (flowname == "ODA Grants"), .(sum(usd_grantequiv, na.rm = TRUE))]))
waitifnot(North_Korea_spending ==unlist(setDT(dac)[(iso3c == "PRK") & (flowname == "ODA Grants"), .(sum(usd_grantequiv, na.rm = TRUE))]))
waitifnot(Kosovo_spending ==unlist(setDT(dac)[(iso3c == "XKX") & (flowname == "ODA Grants"), .(sum(usd_grantequiv, na.rm = TRUE))]))

# WDI import --------------------------------------------------------------
# Use the WDI library to get some useful info from WDI:
#
# GNI / cap Atlas (current$)
# Population, total
# Poverty Headcount 1.90
# CPIA Public Sector and Institutions Score
# Personal Remittances received
# Mean Pov Gap as % of poverty line
# Survey mean consumption or income in 2011 PPPs.
wdidata <- WDI(
  country = "all",
  indicator = c(
    # GDP per capita
    "gdppc" = "NY.GDP.PCAP.PP.CD",
    # GDP
    "GDP" = "NY.GDP.MKTP.CD",
    # GNI per capita, Atlas method (current US$)
    "gnicapatlas" = "NY.GNP.PCAP.CD",
    # Population
    "Population" = "SP.POP.TOTL",
    # Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)
    "pov190hc" = "SI.POV.DDAY",
    # CPIA public sector management and institutions cluster average (1=low to 6=high)
    "cpiapsinst" = "IQ.CPA.PUBS.XQ",
    # Personal remittances, received (current US$)
    "remitperscurrent" = "BX.TRF.PWKR.CD.DT",
    # Poverty gap at $1.90 a day (2011 PPP) (%)
    "povgap190" = "SI.POV.GAPS",
    # Annualized average growth rate in per capita real survey mean
    # consumption or income, total population (%)
    "meanconsincppp" = "SI.SPR.PCAP",
    # Refugee population by DESTINATION
    "refugeepop.dest" = "SM.POP.REFG",
    # Refugee population by ORIGIN
    "refugeepop.orig" = "SM.POP.REFG.OR"
  ),
  start = 2018,
  end = 2019,
  extra = TRUE
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
  "year",
  "cpiapsinst",
  "gdppc",
  "GDP",
  "gnicapatlas",
  "income",
  "meanconsincppp",
  "Population",
  "pov190hc",
  "povgap190",
  "refugeepop.orig",
  "refugeepop.dest",
  "remitperscurrent"
)]
# We want to get either 2018 or 2019 data (which ever is most recent and available)
# So, the next few lines of code looks at whether there is data in 2019. If there is
# not data in 2019, then it replaces the data from 2019, which is empty, with the
# data in 2018. The way that it does this is that it first turns the data set into
# a data table object. Then, it Loops through all the variables and create a lag
# column. finally if in year 2019 that variable is empty, then it sets that
# variable equal to the lag column.
wdidata <- wdidata[order(wdidata$iso3c,wdidata$year),]
setDT(wdidata)
variables <- setdiff(names(wdidata),c("iso3c","year"))
for (val_ in variables) {
  wdidata <- as.data.table(wdidata)
  wdidata[, tempcol := shift(.SD, n = 1L, type = "lag"),
          by = iso3c,
          .SDcols = val_]
  wdidata[year == 2019 & is.na(eval(as.name(val_))),
          (val_) := tempcol]
  wdidata[,tempcol:=NULL]
  wdidata <- as.data.table(wdidata)
}
wdidata <- wdidata[year==2019][,c("iso3c",variables),with=FALSE]
setDF(wdidata)
# World Governance Indicators ---------------------------------------------
setwd(input_dir)
read.xl.sheets <- function(Test_Cases, sheet_start, skip) {
  names.init<-excel_sheets(Test_Cases)
  names.init <- names.init[sheet_start:length(names.init)]
  test.ex<-list()
  for (val in names.init) {
    test.ex[[val]]<-as.data.frame(readxl::read_excel(Test_Cases,sheet=val,skip=skip))
    name.est <- tail(grep("Estimate",names(test.ex[[val]]), value=TRUE),1)
    test.ex[[val]] <- test.ex[[val]][,c("Code",name.est)]
    names(test.ex[[val]]) <- c("iso3c","wgi_value")
    test.ex[[val]]$wgi_value <- as.numeric(test.ex[[val]]$wgi_value)
  }
  test.ex <- lapply(test.ex, as.data.table)
  test.ex
}
wgi <- read.xl.sheets("wgidataset.xlsx", skip = 14, sheet_start = 2)
wgi <- rbindlist(wgi, idcol = "indic", fill = TRUE)
wgi <- wgi[,.(wgi = mean(as.numeric(wgi_value), na.rm=TRUE)),by = iso3c] %>% 
  as.data.frame()
wgi <- na.omit(wgi)
# DISTANCE, COLONIES ---------------------------------------------------------
setwd(input_dir)
# http://www.cgeh.nl/data#Geodist
cepii <- rio::import("dist_cepii.dta")
cepii <- cepii[, c("iso_o", "iso_d", "distcap", "colony")]
cepii <- dplyr::distinct(cepii)
cepii <- rename(cepii, "iso3c" = "iso_o", "iso3c_d" = "iso_d")
# # REMITTANCE (2016) --------------------------------------------------------------
# setwd(input_dir)
# remit <- fread("bilateral-remittance.csv")
# remit[,1] <- NULL
# names(remit)[1] <- "send"
# remit$World <- NULL
# remit <- remit %>% gather(., "receive","remittance", Afghanistan:Zimbabwe)
# remit$receive[remit$receive==0] <- NA
# setDT(remit)[,iso3c_d:= countrycode(remit$send, origin = "country.name",
#                            destination = "iso3c",
#                            custom_match = c("Channel Islands" = NA,
#                                             "World" = NA,
#                                             "Kosovo" = "XKX"))]
# remit[,iso3c:=countrycode(remit$receive, origin = "country.name",
#                            destination = "iso3c",
#                            custom_match = c("Channel Islands" = NA,
#                                             "World" = NA,
#                                             "Kosovo" = "XKX"))]
# remit$remittance <- as.numeric(remit$remittance)
# remit <- setDT(remit)[remittance!=0]
# remit <- remit[iso3c!=iso3c_d,]
# remit <- na.omit(remit)
# remit <- as.data.frame(remit)
# remit <- remit[,c("iso3c","iso3c_d","remittance")]
# DEMOCRACY ---------------------------------------------------------------
# The Freedom House Democracy dataset is relatively clean. All we have to do is
# delete certain countries that we are not interested in. These countries often
# appear with an astrix after the country name.
# Except, we might be interested in for example Hong Kong, and Western Sahara.
# So we first we code Hong Kong to not have an astrix and we code Western
# Sahara to not have an asterix. Then we create a new column that indicates
# whether the country variable has an Asterix in it. We then set our data
# frame to be the rows where that new indicator was false. Finally we use the
# country code package to turn everything into ISO 3C codes. The country code
# package does not recognize Kosovo, Micronesia, and Sao Tomei and Principe.
# So, we have to set custom match for the country code package. Finally we
# just make sure that there are no duplicates for ISO 3 C codes. And we do this
# through the waitifnot function in R. This is the same as the assert function
# in STATA or Python.
dem <- rio::import("freedomhousedemocracy.csv")
names(dem) <- tolower(make.names(names(dem)))
dem[dem$country=="Hong Kong*", "country"] <- "Hong Kong"
dem[dem$country=="Western Sahara*", "country"] <- "Western Sahara"
dem$special_region <- grepl("\\*",dem$country)
dem <- dem[!dem$special_region,]
dem$country[grepl(" and Pr",dem$country)] <- "Sao Tome and Principe"
dem$iso3c <-
  countrycode(
    dem$country,
    "country.name",
    "iso3c",
    custom_match =
      c("Kosovo" = "XKX",
        "Micronesia" = "FSM",
        "São Tomé and Pr�ncipe" = "STP")
  )
dem <- na.omit(dem)
dem$dup <- duplicated(dem$iso3c)
iso_dups <- dem$iso3c[dem$dup==TRUE]
waitifnot(identical(iso_dups, character(0)))
dem[dem$iso3c%in%iso_dups,]
dem <- dem[,c("iso3c","total","political.rights","civil.liberties")]
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
link1 <-"http://wits.worldbank.org/API/V1/SDMX/V21/datasource/tradestats-trade/reporter/all/year/2018/partner/all/indicator/XPRT-PRTNR-SHR"
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
setDF(wti)


# POVCAL ------------------------------------------------------------------



# MERGE -------------------------------------------------------------------
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
df_list <- list(dac, wdidata, dem, wgi)
waitifnot(length(wdidata$iso3c)==length(unique(wdidata$iso3c)))
waitifnot(length(dem$iso3c)==length(unique(dem$iso3c)))
waitifnot(length(wgi$iso3c)==length(unique(wgi$iso3c)))
before <- nrow(dac)
dac <- Reduce(function(d1, d2) merge(d1, d2, by = "iso3c",
                                     all.x = TRUE,
                                     allow.cartesian = FALSE),
              df_list)
waitifnot(nrow(dac) == before)
df_list_2 <- list(dac,cepii, wti)
before <- nrow(dac)
dac <- Reduce(function(d1, d2) merge(d1, d2, by = c("iso3c", "iso3c_d"),
                                     all.x = TRUE,
                                     allow.cartesian = FALSE),
              df_list_2)
waitifnot(nrow(dac) == before)
# FURTHER DATA CLEANING ---------------------------------------------------
dac$income <- NULL
dac$year <- NULL
# check that spending in China/North Korea/Kosovo is 
# the same as we had prior to data cleaning
waitifnot(China_spending ==unlist(setDT(dac)[(iso3c == "CHN") & (flowname == "ODA Grants"), .(sum(usd_grantequiv, na.rm = TRUE))]))
waitifnot(North_Korea_spending ==unlist(setDT(dac)[(iso3c == "PRK") & (flowname == "ODA Grants"), .(sum(usd_grantequiv, na.rm = TRUE))]))
waitifnot(Kosovo_spending ==unlist(setDT(dac)[(iso3c == "XKX") & (flowname == "ODA Grants"), .(sum(usd_grantequiv, na.rm = TRUE))]))
# EXPORT ------------------------------------------------------------------
dac %>% foreign::write.dta(., "dac.dta")
####################CLEANING DONE################################
# Time to build some very basic plots.
#Start with something super basic - just plot every commitment made, against the year it was made in, with a different colour for each income classification, and jittering the results so we can make out different dots
#ODAA1 <- just assigns all the work I'm doing do an object which will then be saved, in this case a graph.
#ODAA1----
ODAA1 <-
  ggplot(dac,
         aes(
           x = year,
           y = usd_commitment,
           colour = incomegroupname,
           alpha = 0.01
         )) +
  geom_jitter() +
  theme_fira() +
  scale_colour_fira()
#ODAA2 ----
ODAA2 <- recmean %>%
  filter(incomegroupname != "Part I unallocated by income") %>%
  mutate(recipientname = fct_reorder(recipientname, usd_commitment)) %>%
  ggplot(aes(x = recipientname, y = usd_commitment, colour = incomegroupname)) +
  geom_point() +
  theme_minimal()
#ODAA3----
ODAA3 <- dac %>%
  filter(incomecat %in% c("L", "LM", "UM", "H")) %>%
  filter(flowname == "ODA Grants") %>%
  ggplot() +
  geom_jitter(aes(x = gnicapatlas, y = usd_commitment, colour = incomecat),
              alpha = 0.1) +
  scale_x_log10() +
  geom_smooth(aes(x = gnicapatlas, y = usd_commitment)) +
  theme_minimal() +
  coord_cartesian(ylim = quantile(as.numeric(dac$usd_commitment), c(0.01, 0.99), na.rm = TRUE)) +
  labs(x = "GNI per Capita, Atlas Method, log scale", y = "USD Millions", title =
         "Commitment size (grants only), 2006-19") +
  scale_colour_manual(
    values = c("dodgerblue4", "palegreen3", "orange", "grey"),
    name = "Income Classification",
    breaks = c("L", "LM", "UM", "H"),
    labels = c("LIC", "LMIC", "UMIC", "HIC")
  )
## Create a data set with the mean usd_commitment for each recipient year (grants + loans) If I want grants only, first filter grants only into a new df and then run the code below with the new df.----
timeseriesmean <-
  aggregate(
    usd_commitment ~ recipientname + year + incomegroupname + regionname + Recipient.Year,
    dac,
    FUN = mean
  )
## Link to wdidata
timeseriesmean <-
  left_join(x = timeseriesmean, y = wdidata, by = "Recipient.Year")
str(timeseriesmean)
timeseriesmean$recipientname <-
  as.factor(timeseriesmean$recipientname)
timeseriesmean$Recipient.Year <-
  as.factor(timeseriesmean$Recipient.Year)
#ODAA5 ----
ODAA5 <- timeseriesmean %>%
  filter(incomecat %in% c("L", "LM", "UM", "H")) %>%
  ggplot() +
  geom_point(aes(x = gnicapatlas, y = usd_commitment, colour = incomecat)) +
  scale_x_log10() +
  ylim(0, 35) +
  geom_smooth(aes(x = gnicapatlas, y = usd_commitment)) +
  geom_text_repel(
    data = subset(timeseriesmean, usd_commitment > 10),
    aes(x = gnicapatlas, y = usd_commitment, label = Recipient.Year),
    size = 3,
    point.padding = 0.3
  ) +
  theme_minimal() +
  theme(text = element_text(size = 9)) +
  labs(
    x = "GNI per Capita, Atlas Method, log scale",
    y = "USD Millions",
    title = "Average Commitment size, 2006-19",
    caption = "Each point is a country-year."
  ) +
  scale_colour_manual(
    values = c("dodgerblue4", "palegreen3", "orange", "grey"),
    name = "Income Classification",
    breaks = c("L", "LM", "UM", "H"),
    labels = c("LIC", "LMIC", "UMIC", "HIC")
  )
#####################Atousa:
#code 1: incomegroup.year plot gni and usd commitment----
dac <- dac %>%
  mutate(incomegroup.Year = paste(incomegroupname, year)) %>%
  select(incomegroup.Year, everything()) %>%
  left_join(., wdidata %>% select("Recipient.Year", "gnicapatlas")) %>%
  select(Recipient.Year, gnicapatlas, everything())
summstats <- dac %>%
  group_by(incomegroupname, year, incomegroup.Year) %>%
  summarise(
    usd_commitment = mean(usd_commitment, na.rm = T),
    gnicapatlas = mean(gnicapatlas, na.rm = T)
  ) %>%
  ungroup()
ODAA7 <- summstats %>%
  filter(!is.na(incomegroupname)) %>%
  filter(incomegroupname %in% c("LICs", "LMICs", "UMICs")) %>%
  ggplot() +
  geom_point(aes(x = gnicapatlas, y = usd_commitment, colour = incomegroupname)) +
  scale_x_log10() +
  theme_minimal() +
  theme(text = element_text(size = 9)) +
  labs(
    x = "GNI per Capita, Atlas Method, log scale",
    y = "USD Millions",
    title = "Average Commitment size, 2006-19",
    caption = "Each point is a incomegroup-year."
  )
print(ODAA7)
#code 2: sectors throughout the years----
#LIC - sector group 2 -----
##first we do boxplots, further down we do geomline
dac$incomegroupname <- as.character(dac$incomegroupname)
#New DF; LIC, filter by income grp name
LICs <- dac %>% filter(incomegroupname == "LICs")
LICs <- LICs %>% filter(!is.na(usd_grantequiv))
#LIC boxplot
LICsectors <-
  ggplot(LICs, aes(fill = sectorgroup2, y = usd_grantequiv, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y = "USD Disbursement (in Millions)", title = "USD Disbursement for LICs") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))
print(LICsectors)
#LIC geomline
names(LICs)
LIC2 <- LICs[, c(78, 22, 74, 75)]
names(LIC2)
LIC2 <- na.omit(LIC2)
#group LIC data by sectorgroup2
LICgrpd2 <- group_by(LIC2, year, sectorgroup2)
LICgrpd_sum2 <- summarise(LICgrpd2, sum(usd_grantequiv))
LICsectors2 <-
  ggplot(
    LICgrpd_sum2,
    aes(
      x = LICgrpd_sum2$year,
      y = LICgrpd_means2$`sum(usd_grantequiv)` ,
      group = sectorgroup2,
      colour = sectorgroup2
    )
  ) +
  geom_line() +
  labs(title = "USD Disbursement for LICs",
       y = "Sum of Disbursement", x = "Year") +
  scale_color_discrete("Sector")
print(LICsectors2)
########LMIC, sector group 2-----
##first we do boxplots, further down we do geomline
Dac$incomegroupname <- as.character(dac$incomegroupname)
#New DF; LMIC, filter by income grp name
LMICs <- dac %>% filter(incomegroupname == "LMICs")
LMICs <- LMICs %>% filter(!is.na(usd_grantequiv))
#LMIC boxplot
LMICsectors <-
  ggplot(LMICs, aes(fill = sectorgroup2, y = usd_grantequiv, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y = "USD Disbursement (in Millions)", title = "USD Disbursement for LMICs") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))
print(LMICsectors)
#LMIC geomline
names(LMICs)
LMIC2 <- LMICs[, c(76, 20, 72, 73)]
names(LMIC2)
LMIC2 <- na.omit(LMIC2)
#group LMIC data by sectorgroup2
LMICgrpd2 <- group_by(LMIC2, year, sectorgroup2)
LMICgrpd_sum2 <- summarise(LMICgrpd2, sum(usd_grantequiv))
LMICsectors2 <-
  ggplot(
    LICgrpd_sum2,
    aes(
      x = LICgrpd_sum2$year,
      y = LICgrpd_sum2$`sum(usd_grantequiv)` ,
      group = sectorgroup2,
      colour = sectorgroup2
    )
  ) +
  geom_line() +
  labs(title = "Disbursement by Sectoral Allocation for LMICs",
       y = " USD Disbursement", x = "Year") +
  scale_color_discrete("Sector")
print(LMICsectors2)
########UMIC, sector group 2-----
##first we do boxplots, further down we do geomline
dac$incomegroupname <- as.character(dac$incomegroupname)
#New DF; UMIC, filter by income grp name
UMICs <- dac %>% filter(incomegroupname == "UMICs")
UMICs <- UMICs %>% filter(!is.na(usd_grantequiv))
#UMIC boxplot
UMICsectors <-
  ggplot(UMICs, aes(fill = sectorgroup2, y = usd_grantequiv, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y = "USD Disbursement (in Millions)", title = "USD Disbursement for UMICs") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))
print(UMICsectors)
#UMIC geomline
names(UMICs)
UMIC2 <- UMICs[, c(76, 20, 72, 73)]
names(UMIC2)
UMIC2 <- na.omit(UMIC2)
#group LMIC data by sectorgroup2
UMICgrpd2 <- group_by(UMIC2, year, sectorgroup2)
UMICgrpd_sum2 <- summarise(UMICgrpd2, sum(usd_grantequiv))
UMICsectors2 <-
  ggplot(
    UMICgrpd_sum2,
    aes(
      x = UMICgrpd_sum2$year,
      y = UMICgrpd_sum2$`sum(usd_grantequiv)` ,
      group = sectorgroup2,
      colour = sectorgroup2
    )
  ) +
  geom_line() +
  labs(title = "Disbursement for UMICs",
       y = "USD Disbursement", x = "Year") +
  scale_color_discrete("Sector")
print(UMICsectors2)
#code 3: flowname (grant, loan...) for differnet income groups----
#Change the variable to a character; makes it easier to filter the data; currently a factor variable
dac$incomegroupname <- as.character(dac$incomegroupname)
#New DF; LIC, filter by income grp name == LIC & get rid rowsthat have NAs
LICs <- dac %>% filter(incomegroupname == "LICs")
LICs <- LICs %>% filter(!is.na(usd_grantequiv))
#LIC
LICflowname <-
  ggplot(LICs, aes(fill = flowname, y = usd_grantequiv, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y = "USD Disbursement (in Millions)", title = "USD Disbursement for LICs") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))
#view plot
LICflowname
#Same thing repeated for other categories ----
#LMIC:
LMICs <- dac %>% filter(incomegroupname == "LMICs")
LMICs <- LMICs %>% filter(!is.na(usd_grantequiv))
LMICflowname <-
  ggplot(LMICs, aes(fill = flowname, y = usd_grantequiv, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y = "USD Disbursement (in Millions)", title = "USD Disbursement for LMICs") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))
LMICflowname
#UMIC
UMICs <- dac %>% filter(incomegroupname == "UMICs")
UMICs <- UMICs %>% filter(!is.na(usd_commitment))
#trying to get labels on the plot
UMICflowname <-
  ggplot(UMICs, aes(fill = flowname, y = usd_grantequiv, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y = "USD Disbursement (in Millions)", title = "USD Disbursement for UMICs") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))
UMICflowname
#list countries that have graduated income categories into different subsets----
graduateL2LM <-
  dac %>%
  filter(
    recipientname %in% c(
      "Bangladesh",
      "Bhutan",
      "Cameroon",
      "C?te d'Ivoire",
      "Ghana",
      "Kenya",
      "Kyrgyz Republic",
      "Lao PDR",
      "Lesotho",
      "Mauritania",
      "Moldova",
      "Mongolia",
      "Myanmar",
      "Nicaragua",
      "Nigeria",
      "Pakistan",
      "Papua New Guinea",
      "Senegal",
      "Solomon Islands",
      "South Sudan",
      "Sudan",
      "Tajikstan",
      "Timor-Leste",
      "Uzbekistan",
      "Viet Nam",
      "Yemen, Rep.",
      "Zambia"
    )
  )
graduateLM2UM <-
  dac %>%
  filter(
    recipientname %in% c(
      "Angola",
      "Albania",
      "Algeria",
      "Azerbaijan",
      "Belarus",
      "Belize",
      "Brazil",
      "Bulgaria",
      "Cambodia",
      "China",
      "Colombia",
      "Congo Republic",
      "Costa Rica",
      "Cuba",
      "Dominican Republic",
      "Ecuador",
      "Fiji",
      "Georgia",
      "Gibraltar",
      "Guyana",
      "India",
      "Iran, Islamic Rep",
      "Iraq",
      "Jamaica",
      "Jordan",
      "Kazakhstan",
      "Macedonia, FYR",
      "Maldives",
      "Marshall Islands",
      "Namibia",
      "Paraguay",
      "Peru",
      "Romania",
      "S?o Tom? and Principe",
      "Suriname",
      "Thailand",
      "Tonga",
      "Tunisia",
      "Turkmenistan",
      "Tuvalu"
    )
  )
graduateUM2H <-
  dac %>%
  filter(
    recipientname %in% c(
      "Antigua and Barbuda",
      "Argentina",
      "Bahrain",
      "Barbados",
      "Bosnia and Herzegovina",
      "Chile",
      "Croatia",
      "Equatorial Guinea",
      "Estonia",
      "Hungary",
      "Latvia",
      "Lithuania",
      "Northern Mariana Islands",
      "Oman",
      "Poland",
      "Russian Federation",
      "Saudi Arabia",
      "Seychelles",
      "Slovak Republic",
      "St. Kitts and Nevis",
      "Trinidad and Tobago",
      "Uruguay",
      "Venezuela, RB"
    )
  )
## bar chart for DAC's contribution to different income groups
df <- dac %>%
  filter(incomegroupname %in% c("LICs", "LMICs", "UMICs")) %>%
  ggplot (aes(year, usd_grantequiv, fill = incomegroupname)) + geom_bar(stat =
                                                                            "identity", position = "dodge")
#make a group for multilateral channel---- (incomplete)
Multilateralchannel <-
  dac %>%
  filter (channelcode %in% c("40000", "41000", "42000", "43000", "44000", "45000", "47000"))

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

