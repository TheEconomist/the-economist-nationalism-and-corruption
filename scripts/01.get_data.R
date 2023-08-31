# This scripts details how the data was constructed

# Stage 1: Load required pages ---------------------------------------------------------------------------------------------

library(countrycode)
library(tidyverse)
library(plm)
library(readxl)
library(vdemdata)
# Note: This load the data frame "vdem"

inspect <- F

# Stage 2: Load and shape ideology data: ---------------------------------------------------------------------------------------------
# Restrict vdem data to 1900 + and relevant columns
ve <- vdem[vdem$year > 1900, c('country_name', 'year', 'v2exl_legitideol', 'v2exl_legitideolcr_0',
               'v2exl_legitideolcr_1',
               'v2exl_legitideolcr_2',
               'v2exl_legitideolcr_3',
               'v2exl_legitideolcr_4')]

#  Calculate top ideology
ve$top_ideology <- NA
for(i in 1:nrow(ve)){
  ve$top_ideology[i] <- c(c('nationalist',
                          'socialist/communist',
                          'restorative/conservative',
                          'separatist/autonomist',
                          'religious')[which.max(ve[i, c('v2exl_legitideolcr_0',
                                             'v2exl_legitideolcr_1',
                                             'v2exl_legitideolcr_2',
                                             'v2exl_legitideolcr_3',
                                             'v2exl_legitideolcr_4')])], NA)[1]
}

# Define nationalism as the extent to which the government is assessed as deriving legitimacy through ideology, and at least 10% of experts assess this as motivated by nationalism, or “restorative/conservative”.
ve$nationalism <- ve$v2exl_legitideol
#ve$nationalism[!(ve$v2exl_legitideolcr_0 > 0.1 | ve$v2exl_legitideolcr_2 > 0.1 | ve$v2exl_legitideolcr_4 > 0.1)] <- NA
ve$nationalism[!(ve$v2exl_legitideolcr_0 > 0.1 | ve$v2exl_legitideolcr_4 > 0.1)] <- NA

# Normalize to 1 to 5 scale:
ve$nationalism <- ve$nationalism - min(ve$nationalism, na.rm = T)
ve$nationalism <- 5*ve$nationalism/max(ve$nationalism, na.rm = T)

# check <- ve[ve$year >= 1990 & !is.na(ve$nationalism), c('country', 'year', 'v2exl_legitideolcr_0', 'v2exl_legitideolcr_2', 'v2exl_legitideolcr_4')]
# check$sum <- check$v2exl_legitideolcr_0 + check$v2exl_legitideolcr_2 + check$v2exl_legitideolcr_4

if(inspect){
  # Plot scores on world map
  pdat <- ve[ve$year == 2022, ]
  pdat$iso3c <- countrycode(pdat$country_name, 'country.name', 'iso3c')
  plot_data_on_world_map(pdat[, c('iso3c', 'nationalism')])
}

# Stage 3: Add world bank covariates: ---------------------------------------------------------------------------------------------
# library(WDI)
# wdi <- WDI(country = 'all',
#            indicator = c('wdi_prop_less_2_usd_day' = 'SI.POV.DDAY',
#                          'wdi_gdppc_nominal' = 'NY.GDP.PCAP.CD',
#                          'wdi_gdppc_ppp' = 'NY.GDP.PCAP.PP.CD',
#                          'wdi_urban_population_pct' = 'SP.URB.TOTL.IN.ZS',
#                          'wdi_urban_pop_1m_cities_pct' = 'EN.URB.MCTY.TL.ZS',
#                          'wdi_gini_index' = 'SI.POV.GINI',
#                          'wdi_life_expectancy_at_birth' = 'SP.DYN.LE00.IN',
#                          'wdi_pop_over_65' = 'SP.POP.65UP.TO.ZS',
#                          'wdi_pop_under_15' = 'SP.POP.0014.TO.ZS',
#                          'wdi_population' = 'SP.POP.TOTL'))
# temp <- wdi[wdi$year == 2021,] # Assume 2021 values for 2022
# temp$year <- 2022
# wdi <- rbind(wdi, temp)
# saveRDS(wdi, 'source-data/wdi_cache.RDS')
wdi <- readRDS('source-data/wdi_cache.RDS')

ve$iso3c <- countrycode(ve$country_name, 'country.name', 'iso3c')
ve <- merge(ve, wdi, by = c('year', 'iso3c'), all.x = T)

# Stage 4: Add corruption data ---------------------------------------------------------------------------------------------
# Source: https://www.transparency.org/en/cpi/2021

cr <- read_xlsx('source-data/CPI2022_GlobalResultsTrends.xlsx', sheet = 2, skip = 1)
cr <- data.frame(cr) # Tibbles are not convenient objects

cr <- pivot_longer(cr, cols=colnames(cr)[4:ncol(cr)])
cr$name <- tolower(cr$name)
cr <- cr[cr$name %in% paste0('cpi.score.', 2012:2022), ]
cr$year <- t(data.frame(strsplit(cr$name, 'score.')))[, 2]
cr$corruption <- 100-cr$value # reversing to be more intuitive
cr$value <- NULL

cr$iso3c <- countrycode(cr$Country...Territory, 'country.name', 'iso3c')

# Merge corruption data in:
ve <- merge(ve, cr[, c('year', 'iso3c', 'corruption')], by = c('iso3c', 'year'), all.x = T)

# Stage 5: Export data to file ---------------------------------------------------------------------------------------------
ve <- ve[!is.na(ve$iso3c) & ve$iso3c != 'PSE', ]

# Adding some descriptive column names
ve$regime_use_of_ideology_measure <- ve$v2exl_legitideol
ve$ideology_is_nationalist_percent_of_experts <- ve$v2exl_legitideolcr_0
ve$ideology_is_socialist_or_communist_percent_of_experts <- ve$v2exl_legitideolcr_1
ve$ideology_is_restorative_or_conservative_percent_of_experts <- ve$v2exl_legitideolcr_2
ve$ideology_is_separatist_or_autonomist_percent_of_experts <- ve$v2exl_legitideolcr_3
ve$ideology_is_religious_percent_of_experts <- ve$v2exl_legitideolcr_4

write_csv(ve, 'output-data/nat_and_corruption.csv')
write_csv(ve[ve$year >= 1980, c(1,2,3,10,25:ncol(ve))], 'output-data/nat_and_corruption_summary.csv')


