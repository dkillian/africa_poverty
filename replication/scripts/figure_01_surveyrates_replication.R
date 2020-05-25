source("code_figs/00_dependencies.R")

library(dplyr)

######################################################
# Create DF with % pop surveyed for each year in the world
######################################################

#get country by year # of people surveyed in povcal listed SURVEY data

# povcal <- read.csv("../data/surveys/povcal_time_pop.csv")

?read_csv
povcal <- read_csv("data/surveys/povcal_time_pop.csv",
                   col_types = "cccccccccccccccccccc") %>%
    as.data.frame %>%
    pivot_longer(cols=2:20,
                 names_to="year") %>%
    set_names(nm=c("country","year","povcal")) %>%
    mutate(povcal=tidyr::replace_na(povcal, 0))

str(povcal)
head(povcal)


# some columns are character where there is an occasional entry of 'x'. meaning of 'x' unexplained
# imported data from sheet where 'x' removed manually
# turned out I couldn't do this as the 'x' was used later and I couldn't replicate the code without it


# povcal = melt(povcal, id="country")


# names(povcal) = c("country", "year", "povcal")

#povcal[is.na(povcal$povcal) | povcal$povcal == "", "povcal"] = 0 # I don't understand the 'povcal' 
#povcal$povcal = as.numeric(povcal$povcal)

#get country by year # of people surveyed in DHS SURVEY data

# dhs = read.csv("../data/surveys/dhs_time.csv", stringsAsFactors = F)
# dhs = melt(dhs)
# names(dhs) = c("country", "year", "dhs")

dhs <- read_csv("data/surveys/dhs_time.csv")

dhs <- pivot_longer(dhs, 
                    cols=2:18,
                    names_to="year")

names(dhs) <- c("country", "year", "dhs")


# get country by year population
# pop = read.csv("../data/surveys/population_time.csv", stringsAsFactors = F)
# pop = melt(pop)
# names(pop) = c("country", "iso3", "year", "pop")
# pop = dplyr::select(pop, -country)

pop <- read_csv("data/surveys/population_time.csv") %>%
    as.data.frame

str(pop)

pop <- pivot_longer(pop,
                    cols=3:63,
                    names_to="year")

names(pop) <- c("country", "iso3", "year", "pop")

pop <- pop %>%
    dplyr::select(-country)


# crosswalk country ids
cross = read.csv("../data/crosswalks/crosswalk_countries.csv", stringsAsFactors = F)

cross <- read_csv("data/crosswalks/crosswalk_countries.csv") %>%
    as.data.frame
str(cross)


# merge povcal, population, and crosswalk country names
# dhs = merge(dhs, cross, by.y="country_simp", by.x="country", all.x=T)[, -6]
# povcal = merge(povcal, cross, by.y="country_simp", by.x="country", all.x=T)[, -6]
# full = merge(dhs, povcal, by=c("country", "year", "iso2", "iso3", "country_pred", "country_wb"), all=T)
# full = merge(full, pop, by=c("iso3", "year"), all.x=T)
# full = dplyr::select(full, year, country, iso2, dhs, pop, povcal)


dhs2 = merge(dhs, cross, by.y="country_simp", by.x="country", all.x=T)[, -6] %>%
    arrange(country, year)

dhs <- dhs %>%
    left_join(cross[,-3],
              by=c("country"="country_simp")) %>%
    arrange(country, year)

dhs == dhs2


povcal2 <- merge(povcal, cross, by.y="country_simp", by.x="country", all.x=T)[, -6] %>%
    arrange(country, year)


povcal <- povcal %>%
    left_join(cross[,-3],
              by=c("country"="country_simp")) %>%
    arrange(country, year)

povcal==povcal2

full2 = merge(dhs, povcal, by=c("country", "year", "iso2", "iso3", "country_pred", "country_wb"), all=T)

full2 = merge(full2, pop, by=c("iso3", "year"), all.x=T)

full2 = dplyr::select(full2, year, country, iso2, dhs, pop, povcal) %>%
    arrange(country, year)


full <- dhs %>%
    full_join(povcal) %>% # merge with all = T is full join
    left_join(pop) %>% # merge with all.x = T is left_join
    dplyr::select(year, country, iso2, dhs, pop, povcal) %>%
    arrange(country, year)

full == full2


#sub in the years of povcal without sample sizes
# samp = as.numeric(full$povcal)/full$pop
# samp = quantile(samp[samp != 0], 0.9, na.rm=T)
# full$povcal[full$povcal == "x" & !is.na(full$povcal)] = samp*full$pop[full$povcal == "x" & !is.na(full$povcal)]
# full$povcal = as.numeric(full$povcal)

    # for 128-129, I think I already did this when I removed 'x' manually

samp <- as.numeric(full$povcal)/full$pop
samp

samp <- quantile(samp[samp != 0], 0.9, na.rm=T)

f <- full$povcal
f

full$povcal[full$povcal == "x" & !is.na(full$povcal)] = samp*full$pop[full$povcal == "x" & !is.na(full$povcal)] # i don't understand!


f2 <- full$povcal
f2

f == f2


full <- full %>%
    mutate(povcal = as.numeric(povcal),
           year=as.numeric(year)) %>%
    as.data.frame

str(full)
str(full2)
describe(full$povcal)


#get year in better format
# full$year = full$year %>% as.character() %>% substr(2, 6) %>% as.numeric() 

#get the US and filter the rest to relevant countries
full = full[full$country %in% 
                c("Algeria", "Angola",  "Benin", "Botswana", "Burkina Faso", "Burundi",
                  "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", 
                  "Democratic Republic of the Congo", "Djibouti", "Egypt", 
                  "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", 
                  "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", 
                  "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", 
                  "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", 
                  "Republic of the Congo", "Rwanda", "Sao Tome and Principe", "Senegal", 
                  "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", 
                  "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia",  
                  "Zimbabwe"), ]
                        
#get the aggregated yearly revisit rate over all the countries in full
# year = aggregate(full[, c('dhs', 'pop', 'povcal')], by=list(full$year), sum, na.rm=TRUE)
# names(year) = c("year", 'dhs', 'pop', 'povcal')
# year$surv_perc = (year$pop*365)/(year$dhs + year$povcal)

year2 = aggregate(full[, c('dhs', 'pop', 'povcal')], by=list(full$year), sum, na.rm=TRUE)

year <- full %>%
    group_by(year) %>%
    summarize_at(vars(dhs, pop, povcal), sum, na.rm=T)

str(year)
str(year2)

year==year2

year <- year %>%
    mutate(surv_perc = (pop*365) / (dhs + povcal)) %>%
    as.data.frame

str(year)
head(year)

#combine the us dhs and povcal with ACS etc
# us_pop = pop[pop$iso3=="USA",]
# us = read.csv('../data/surveys/us_surveys_time.csv')
# us = melt(us)
# us = aggregate(us$value, by=list(us$variable), FUN=sum, na.rm=T)
# us$year = us$Group.1 %>% as.character() %>% substr(2, 6) %>% as.numeric() 
# us = merge(us, us_pop, by.x="Group.1", by.y="year")
# us$survey_perc = (us$pop*365)/(us$x)


us_pop <- pop %>%
    filter(iso3=="USA") %>%
    mutate(year=as.numeric(year))
us_pop
str(us_pop)
tail(us_pop)

us <- read_csv('data/surveys/us_surveys_time.csv')

us <- us %>%
    pivot_longer(cols=2:20,
                 names_to="year") %>%
    arrange(country, year) %>%
    mutate(year=as.numeric(year)) %>%
    as.data.frame 

us
str(us)

us == usM

us <- us %>%
    group_by(year) %>%
    dplyr::summarize(value=sum(value)) %>%
    as.data.frame

str(us)
us

usM <- reshape2::melt(us) %>%
    arrange(country, variable)
usM

us2 = aggregate(us$value, by=list(us$year), FUN=sum, na.rm=T)
us2

us == us2

us$year = us$Group.1 %>% as.character() %>% substr(2, 6) %>% as.numeric() 

us3 = merge(us2, us_pop, by.x="Group.1", by.y="year")
us3

us <- us %>%
    left_join(us_pop) %>%
    mutate(survey_perc = (pop*365) / value)

str(us)

us == us3

us

us3$survey_perc = (us3$pop*365)/(us3$x)

us3


######################################################
# Create DF with % pop imaged for each year in the world
######################################################

gee <- read_csv("data/overpass/dhs_sample_GEE.csv")

gee <- gee %>% 
    group_by(year) %>% #multiply modis *8 because its an 8 day composite
    dplyr::summarize(l5 = 365/(sum(num_l5, na.rm=T)/500), 
                     l7 = 365/(sum(num_l7, na.rm=T)/500), 
                     l8 = 365/(sum(num_l8, na.rm=T)/500),
                     s2 = 365/(sum(num_s2, na.rm=T)/500), 
                     s1 = 365/(sum(num_s1, na.rm=T)/500), 
                     all_s = 365/(sum(num_s1, num_s2, na.rm=T)/500),
                     all_l = 365/(sum(num_l5, num_l8, num_l7, na.rm=T)/500), 
                     modis = 365/((sum(num_modis, na.rm=T)*8)/500))
                         
planet <- read_csv("data/overpass/dhs_sample_Planet.csv")

planet <- dplyr::select(planet, year, cluster_id, count_PlanetScope, count_RapidEye)

planet <- planet %>% 
    group_by(year) %>% 
    dplyr::summarize(planetscope = 365/(sum(count_PlanetScope, na.rm=T)/500), 
              rapideye = 365/(sum(count_RapidEye, na.rm=T)/500), 
              all_planet = 365/(sum(count_RapidEye, count_PlanetScope, na.rm=T)/500))

quickbird <- read_csv("data/overpass/landinfo_dhs_sample_nocatalog.csv")

quickbird <- quickbird %>%
    mutate(year = paste0("20", substr(as.character(date),
                                     nchar(as.character(date))-1, nchar(as.character(date)))),
           cloud = substr(as.character(cloud), 1, nchar(as.character(cloud))-1))

quickbird <- quickbird %>% 
    dplyr::filter(cloud <= 30) %>% 
    group_by(year) %>% 
    dplyr::summarise(dg=365/(n()/500)) %>%
    mutate(year = as.numeric(year))

str(gee)
str(planet)
str(quickbird)

overpass <- gee %>%
    inner_join(planet) %>%
    inner_join(quickbird)

overpass2 <- merge(gee, planet, by="year")

overpass == overpass2

overpass 

overpass[overpass==Inf] <- NA

overpass <- overpass %>%
    pivot_longer(cols=2:13,
                 names_to="variable")

overpassM <- reshape2::melt(overpass, id.vars="year")
overpassM

overpass

overpass <- overpass[overpass$variable %in% c("s2", "all_l", "planetscope", "dg", "rapideye"),]

resolution <- data.frame(variable=c("s2", "all_l", "planetscope", "dg", "rapideye"), 
                        res = c(10, 30, 3, .6, 5))

overpass <- overpass %>%
    inner_join(resolution)



######################################################
# Make line plot of revisit rate over time
######################################################

reverselog_trans = function(base = 10) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv, 
              log_breaks(base = base), 
              domain = c(1e-100, Inf))
}

year <- year %>%
    mutate(variable = "africa_surveys",
           year=as.numeric(year)) %>%
    dplyr::select(year, variable, surv_perc) %>%
    set_names(nm=c("year","variable","value"))


us <- us %>%
    mutate(variable = "us_surveys",
           year=as.numeric(year)) %>%
    dplyr::select(year, variable, survey_perc) %>%
    set_names(nm=c("year","variable","value"))

head(us)
head(year)

year = rbind(us, year) 
str(year)
year
tail(year)

frq(year$year)

options(scipen=100)

year

res
resolution
overpass
str(overpass)
str(year)

ggplot() + 
    geom_line(data = overpass, aes(year, value, group=variable, color=as.factor(res)), size=.8) +
    geom_line(data = year, aes(year, value, group=variable), size=.8, linetype="1232") +
    
    scale_y_continuous(trans=reverselog_trans(10), limits = c(4927500, 0.5),
                       breaks = c(1, 7, 30, 365, 3650, 36500, 365000, 4927500), 
                       labels = c("a day", "a week", "a month", "a year", "10 years", "100 years", "1,000 years", "13,500 years"),
                       sec.axis=sec_axis(~., 
                                         breaks=lab[,2],
                                         labels=lab[,1])) + 
    
    scale_x_continuous(limits = c(2000, 2018), breaks=seq(2000, 2018, 2)) + 
    
    geom_hline(yintercept=1, size=0.7, alpha=0.5, color="grey", linetype="longdash") + 
    geom_hline(yintercept=7,size=0.7, alpha=0.5, color="grey", linetype="longdash") + 
    geom_hline(yintercept=30, size=0.7, alpha=0.5, color="grey", linetype="longdash") +
    geom_hline(yintercept=365, size=0.7, alpha=0.5, color="grey", linetype="longdash") +
    geom_hline(yintercept=3650, size=0.7, alpha=0.5, color="grey", linetype="longdash") +
    geom_hline(yintercept=36500, size=0.7, alpha=0.5, color="grey", linetype="longdash") +
    geom_hline(yintercept=365000, size=0.7, alpha=0.5, color="grey", linetype="longdash") +
    geom_hline(yintercept=4927500, size=0.7, alpha=0.5, color="grey", linetype="longdash") + 
    
    labs(y="Avg. household revisit interval (days)",
         x="",
         title="Average household revisit rate (surveys)\nand average location revisit rate (satellites)") + 
    theme_anne("sans", size=25) +
    scale_colour_manual(values = colorRampPalette(c("#06276E", "#9BC7FF"), bias=2)(5)) + 
    theme_minimal() + 
    theme(legend.position = "none",
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.title=element_text(hjust=.5)) +
    nobord 

ggsave("viz/Figure 1b household or location revisit rates.png",
       type="cairo",
       device="png",
       height=5, 
       width=7)


# labels on right

head(year)

labY <- year %>%
    distinct(variable) %>%
    na.omit %>%
    unlist

labY

case_endsY <- year %>%
    group_by(variable) %>%
    top_n(1,year) %>%
    pull(value)

case_endsY


tail(overpass)

labOV <- overpass %>%
    distinct(variable) %>%
    na.omit %>%
    unlist

labOV

case_endsOV <- overpass %>%
    group_by(variable) %>%
    top_n(1,year) %>%
    pull(value)

case_endsOV


lab <- data.frame(label = c("U.S. surveys","Africa surveys","Sentinel 2","LandSat", "PlanetScope","Rapid Eye","Digital Globe"),
                  value = c(case_endsY, case_endsOV)) %>%
    arrange(value)

lab

year <- year %>%
    mutate(label = ifelse(year==max(year), as.character(variable), NA_character_),
           country2 = fct_reorder(country, cases, max))

frq(byDay$label)

?rank
rank(byDay$country)
frq(byDay$country2)

str(byDay)
head(byDay)
tail(byDay)

?distinct
lab <- byDay %>%
    distinct(label) %>%
    na.omit %>%
    unlist

lab

case_ends <- byDay %>%
    group_by(country) %>%
    top_n(1,cases) %>%
    pull(cases)

case_ends

scale_y_continuous(limits=c(0,225000),
                   breaks=seq(0,225000,25000),
                   labels=comma,
                   sec.axis=sec_axis(~., 
                                     breaks=case_ends,
                                     labels=lab[1:6]))




p

ggsave("../raw_fig/Figure1b_revisitrate.pdf", plot=p,  width=11.5, height=7.4)


######################################################
# Create DF with % pop surveyed for each country in the world
######################################################

country = full[full$year <= 2014 & full$year >= 2000, ]
country = aggregate(country[, c('dhs', 'pop', 'povcal')], by=list(country$country), sum, na.rm=TRUE)
names(country) = c("country", 'dhs', 'pop', 'povcal')
country$perc = (country$dhs + country$povcal)/(country$pop*(365))
country = merge(country, cross, by.x="country", by.y="country_simp")

cols = 2:20

povcal = read.csv("../data/surveys/povcal_time_pop.csv")
povcal[povcal==""] = 0
povcal[, -1] = !is.na(povcal[, -1])
povcal$povsum = rowSums(povcal[, cols], na.rm=T)

dhs = read.csv("../data/surveys/dhs_time.csv")
dhs[, -1] = dhs[, -1] > 0
dhs$dhssum = rowSums(dhs[, cols], na.rm=T)

census = merge(dhs, povcal, by='country', all=T)
census = merge(census, cross, by.x="country", by.y="country_simp")
census = dplyr::select(census, country, iso3, povsum, dhssum)

census[is.na(census)] = 0
survt = (census$povsum + census$dhssum)!=0
census[survt, "survsum"] = (2018-1999)/(census[survt, "dhssum"] + census[survt, "povsum"])
census[!survt, "survsum"] = 0
africa = readRDS("../data/shapefiles/africa_gadm.rds")
africa = merge(africa, census, by.x ="GID_0", by.y="iso3")
africa$survsum[is.na(africa$survsum)] = 0

######################################################
# Make maps
######################################################

surv = map(africa, 'survsum', '', 'years btwn surveys', color=c("#F7F7B8", "#F7EB7D", "#F7E04C", "#F7CA23", "#FFA94D", "#FF4D4D"), #mincol="#FFFFA7", maxcol="#FF0000", 
           breaks=c(1, 2, 3, 4, 5, 10, 19), font="sans")
ggsave("../raw_fig/Figure1a_mapsurveysovertime.pdf", plot=surv,  width=7, height=7)
