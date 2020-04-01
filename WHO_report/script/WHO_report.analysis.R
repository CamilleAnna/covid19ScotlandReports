# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   SCRIPT ANALYSING THE WHO DATA    #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# DIRECTORIES & DATE SET UP ----

setwd('/Users/s1687811/Desktop/covid19/Africa_report/')
today<- Sys.Date() -2



# LOAD DATA & SOURCE USEFUL FUNCTIONS ----
source('/Users/s1687811/Documents/GitHub/covid19/script/sourced_functions_doublingTime_reports.R')


who.tab<- read.csv('WHO_countries.txt', sep = '\t') # WHO list of countries
colnames(who.tab)<- c('country', 'name_plot')

who<- as.character(read.csv('WHO_countries.txt', sep = '\t')[,1])
pops<- read_excel('Africa_popsizes.xlsx')

d<- 
  read_excel(paste0('./', today, '/Africa_data_', today, '.xlsx'), sheet = 2) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() %>%
  select(c(date, which(colnames(.) %in% who)))

who.in<- who[is.element(who, colnames(d))]
who.out<- who[!is.element(who, colnames(d))]

d.long<-
  d %>%
  gather('country', 'cumNumCases', 2:ncol(d))

d.deaths<- 
  read_excel(paste0('./', today, '/Africa_data_', today, '.xlsx'), sheet = 1) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() %>%
  select(c(date, which(colnames(.) %in% who)))

d.deaths.long<-
  d.deaths %>%
  gather('country', 'cumNumDeaths', 2:ncol(d.deaths))


d.10k.log.long<- 
  d %>%
  gather('country', 'cumcases', 2:ncol(d)) %>%
  left_join(pops, by = 'country') %>%
  mutate(cumcases_10k = cumcases * (10000/popsize)) %>%
  na_if(0) %>%
  mutate(cumcases_10k_log = log10(cumcases_10k)) %>%
  select(date, country, cumcases_10k_log)

d.10k.log<-
  d.10k.log.long %>%
  spread(country, cumcases_10k_log)


d.deaths.10k.log.long<- 
  d.deaths %>%
  gather('country', 'cumdeaths', 2:ncol(d.deaths)) %>%
  left_join(pops, by = 'country') %>%
  mutate(cumdeaths_10k = cumdeaths * (10000/popsize)) %>%
  na_if(0) %>%
  mutate(cumdeaths_10k_log = log10(cumdeaths_10k)) %>%
  select(date, country, cumdeaths_10k_log)


d.deaths.10k.log<-
  d.deaths.10k.log.long %>%
  spread(country, cumdeaths_10k_log)


palette<- brewer.pal(10, 'Paired')
palette.f1<- c(rep('black', ncol(d) - length(palette)), palette)


