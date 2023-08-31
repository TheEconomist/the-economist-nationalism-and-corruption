# Analysis

# Stage 1: Load required packages ---------------------------------------------------------------------------------------------
library(countrycode)
library(tidyverse)
library(plm)
library(readxl)
library(vdemdata)

# Stage 2: Load data ---------------------------------------------------------------------------------------------
ve <- read_csv('output-data/nat_and_corruption.csv')
source('scripts/plot_data_on_world_map.R')

# Stage 3: Quotes for text ---------------------------------------------------------------------------------------------
# Ideologies are judged by some experts as nationalist 95% of the time:
sum(ve$v2exl_legitideolcr_0[!is.na(ve$v2exl_legitideol)] > 0)[1]/length(ve$v2exl_legitideolcr_0[!is.na(ve$v2exl_legitideol)])
sum(ve$v2exl_legitideolcr_0[!is.na(ve$v2exl_legitideol) & ve$year >= 2012] > 0)[1]/length(ve$v2exl_legitideolcr_0[!is.na(ve$v2exl_legitideol) & ve$year >= 2012])

# See also further down, in panel regression section, for effect size.

# Stage 4: Charts for article ---------------------------------------------------------------------------------------------

# Chart changes over time:
start_year <- 1980
ggplot(ve[ve$year >= start_year & !is.na(ve$nationalism), ], aes(x=year, y=nationalism))+
  geom_line(data= ve[ve$year >= start_year &
                       ve$iso3c %in% unique(ve$iso3c[order(ve$wdi_population, decreasing = T)])[1:10], ],
            aes(col = country_name))+
  geom_smooth(aes(group=1, weight = wdi_population, linetype='World-wide average\n(population-weighted)'), method = 'loess', span =0.1, se = F, size = 2)+
  theme_minimal()+theme(legend.title = element_blank())+ylab('')+xlab('')+
  ggtitle('CHART 1: Government use of ideology to legitimize rule\nIn countries were some experts judged ideology to be nationalist')+xlab('Sources:V-dem, UN, The Economist')
ggsave('plots/CHART_1.pdf')


# Chart of nationalism in 2022:
ve$nationalism_plot <- ifelse(is.na(ve$nationalism), min(ve$nationalism, na.rm = T), ve$nationalism)

manual_breaks <- as.numeric(quantile(ve$nationalism[ve$year == 2022], probs = 1:5/5, na.rm = T))
names(manual_breaks) <- c('low', rep('', length(manual_breaks)-2), 'high')

plot_data_on_world_map(ve[ve$year == 2022, c('iso3c', 'nationalism_plot')])+ggtitle('CHART 2: Government use of nationalism* as legitimization strategy, 2022\n')+xlab('*Use of ideology to legitimize rule, where at least 20% of experts judged this ideology to be nationalist. \nOften nationalist in combination with e.g. religion and right/left politics.\nSource: V-dem, The Economist')+scale_fill_binned(low ='gray95', high ='red', breaks = manual_breaks)+theme(legend.title = element_blank())
ggsave('plots/CHART_2.pdf')


# Chart of relationship between corruption and nationalism:
ggplot(ve[, ], aes(x=corruption, y=nationalism, col=country_name, size=wdi_population))+geom_point()+geom_smooth(aes(col='trend', weight=wdi_population), method = 'lm')+theme_minimal()+theme(legend.pos = 'none')+ggtitle('CHART 3: Government nationalism and corruption')+xlab('Corruption\n\nDots = countries by year\nSize = population\nLine = population-weighted linear trend\nSources: Transparency International, VDEM, UN, The Economist')+ylab('Nationalism')
ggsave('plots/CHART_3.pdf')

# Stage 5: Regression analysis ---------------------------------------------------------------------------------------------
library(QuickCoefPlot)
qcp(lm(corruption ~ nationalism + as.factor(year) + as.factor(iso3c), data=ve), include = 1:3)
summary(lm(corruption ~ nationalism + as.factor(year) + as.factor(iso3c), data=ve, weights = wdi_population))
summary(lm(corruption ~ nationalism + as.factor(year) + as.factor(iso3c), data=ve, weights = log(wdi_population)))

# Panel
pve <- ve[ve$year < 2022, ] # Skipping most recent year, which is especially uncertain
pve <- pve[order(pve$year), ]
pve <- pve[!is.na(pve$nationalism) & !is.na(pve$corruption), ]
pve <- pdata.frame(data.frame(pve), index = c('iso3c', 'year'))

# Panel regression, with year fixed effects and unconditional robust covariance matrix estimators a la Beck and Katz for panel models (a.k.a. Panel Corrected Standard Errors (PCSE))
plm_fit <- plm(corruption ~ nationalism + as.factor(year), data=pve, model ='within') # equivalent to: plm_fit <- plm(corruption ~ nationalism + as.factor(year) + as.factor(iso3c), data=pve)
coeftest(plm_fit, vcov=vcovBK(plm_fit, cluster = c('group')))

# Controlling for GDP PC PPP changes
plm_fit <- plm(corruption ~ nationalism + wdi_gdppc_ppp + as.factor(year), data=pve, model ='within') # equivalent to: plm_fit <- plm(corruption ~ nationalism + as.factor(year) + as.factor(iso3c), data=pve)
coeftest(plm_fit, vcov=vcovBK(plm_fit, cluster = c('group')))

# Between-country comparisons
plm_fit <- plm(corruption ~ nationalism + as.factor(year), data=pve, model ='pooling')
coeftest(plm_fit, vcov=vcovBK(plm_fit, cluster = c('group')))

# Raw comparison:
plm_fit <- plm(corruption ~ nationalism + as.factor(year), data=pve, model ='pooling')
coeftest(plm_fit, vcov=vcovBK(plm_fit, cluster = c('group')))

# Between-country comparisons, controlling for GDP per capita
plm_fit <- plm(corruption ~ nationalism + wdi_gdppc_ppp + as.factor(year), data=pve, model ='pooling')
coeftest(plm_fit, vcov=vcovBK(plm_fit, cluster = c('group')))

# Effect size (between), normalized:
plm_fit <- plm(corruption ~ nationalism, data=pve, model ='pooling')
coef(plm_fit)[2]*sd(pve$nationalism)/sd(pve$corruption)
coef(plm_fit)[2]*sd(pve$nationalism)
coef(plm_fit)[2]*1

# Effect size (within), normalized:
plm_fit <- plm(corruption ~ nationalism, data=pve, model ='within')
coef(plm_fit)[1]*mean(aggregate(pve$nationalism, list(pve$iso3c), FUN=sd)[, 2], na.rm = T)/mean(aggregate(pve$corruption, list(pve$iso3c), FUN=sd)[, 2], na.rm = T)
coef(plm_fit)[1]*mean(aggregate(pve$nationalism, list(pve$iso3c), FUN=sd)[, 2], na.rm = T)
coef(plm_fit)[1]*1

# Change in India
(ve$nationalism[ve$year == 2022 & ve$iso3c == 'IND'] - ve$nationalism[ve$year == 2012 & ve$iso3c == 'IND'])/mean(aggregate(pve$corruption, list(pve$iso3c), FUN=sd)[, 2], na.rm = T)


# Stage 6: Panel Granger causality ---------------------------------------------------------------------------------------------
balanced_pve <- ve[ve$year < 2022, ] # Skipping most recent year, which is especially uncertain
balanced_pve <- balanced_pve[order(balanced_pve$year), ]

# Ensure no missing variables
balanced_pve <- balanced_pve[!is.na(balanced_pve$corruption) & !is.na(balanced_pve$nationalism), ]

# Drop countries which do not vary on either variable
balanced_pve <- balanced_pve[ave(balanced_pve$nationalism, balanced_pve$iso3c, FUN = function(x) length(unique(x))) >= 3, ]
balanced_pve <- balanced_pve[ave(balanced_pve$corruption, balanced_pve$iso3c, FUN = function(x) length(unique(x))) >= 3, ]

# Ensure panel is balanced
balanced_pve <- balanced_pve[balanced_pve$iso3c %in% names(table(balanced_pve$iso3c))[table(balanced_pve$iso3c) == max(table(balanced_pve$iso3c))], ]
balanced_pve$iso3c <- as.factor(balanced_pve$iso3c)

# Convert to panel data frame
balanced_pve <- pdata.frame(data.frame(balanced_pve), index = c('iso3c', 'year'))

# Check for stationarity using Maddala and Wu (1999) procedure and Choi (2021) procedure
purtest(balanced_pve$corruption, lags=1, test ='madwu' )
purtest(balanced_pve$nationalism, lags=1, test='madwu')
purtest(balanced_pve$corruption, lags=1, test ='Pm')
purtest(balanced_pve$nationalism, lags=1, test='Pm')

# Check:
# balanced_pve$norm <- rnorm(nrow(balanced_pve))
# purtest(balanced_pve$norm, lags=1, test='madwu')

# Check for granger-causation in both directions
pgrangertest(corruption ~ nationalism, data = balanced_pve, test = 'Ztilde', order = 1)
# Finding: nationalism granger-causes corruption

pgrangertest(nationalism ~ corruption, data = balanced_pve, test = 'Ztilde', order = 1)
# Finding: corruption does not granger-cause nationalism




