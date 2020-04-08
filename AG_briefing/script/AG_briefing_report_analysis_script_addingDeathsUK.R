# ----------------------------------- #
# AG briefing report analysis script #
# ---------------------------------- #

# This R script runs all the analyses for the AG briefing reports.
# It outputs a csv file of the doubling times
# And saves the R session as an RData object
# The automated report (Rmarkdown doc) then only load this RData object to run plots commands in it


# SETUP ----

setwd('/Users/s1687811/Documents/GitHub/covid19/AG_briefing/')

today<- Sys.Date() - 3
its = 10
set.seed(as.numeric(today))

source('/Users/s1687811/Documents/GitHub/covid19/script/sourced_functions_doublingTime_reports.R') 

time_window<- 7
t2.define<- today - 1
t1.define<- t2.define - time_window



# LOADING DATA ----


d.uk<- # UK data, formatting variable names to fit in rest of the script
  read_excel(paste0('./data/', today, '/UK_data_', today, '.xlsx'), sheet = 1) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame()

pops.shb<- read_excel('./input_files/popsizes_grouped.xlsx', sheet = 1) %>% as.data.frame() # Scottish health board population data
pops.uk<- read_excel('./input_files/popsizes_grouped.xlsx', sheet = 2) %>% as.data.frame() # UK population data


d.cases<- # Scotland data --> no need to edit
  read.xlsx(paste0('./data/', today, '/SARS-Cov-2-Scotland_all_', today,'.xlsx'), 4)[1:as.numeric(as.Date(format(today,"%Y/%m/%d"))-as.Date("2020/03/01")-1),] %>%
  rename(date = Dates) %>%
  mutate(date = seq(as.Date("2020/03/01"), as.Date(format(today,"%Y/%m/%d")), by = "day")[-c(2,3)]) %>%
  as.data.frame()
names(d.cases) <- gsub(pattern="[.][.]", replacement=" ", names(d.cases))## read.xlsx will change space between words to ., so change back to space
names(d.cases) <- gsub(pattern="[.]", replacement=" ", names(d.cases))


d.death<-
  read_excel(paste0('./data/', today, '/SARS-Cov-2-Scotland_all_', today,'.xlsx'), sheet = 1,skip=1) %>%
  rename(date = Date,
         cumNumCases = Deaths_Cum) %>% # ! ambiguous variable naming, but this is indeed DEATHS we are dealing with. However the sim.epi() and compute.td.m1.v2() functions take as argument a table where the cumulative number variable is called cumNumCases, whatever "case" is, can be cases, deaths, .... Improvment needed on the sim.epi() and compute.td() function to make them more general!
  mutate(date = seq(as.Date("2020/03/01"), as.Date(format(today,"%Y/%m/%d")), by = "day")) %>%
  select(date, cumNumCases) %>%
  as.data.frame()

# Dataframe which will be filled throughout this script and write as csv at the end. Collects all Td & 95%CI.
Td.report<- data.frame(variable = character(), Td.obs = numeric(), ci.low = numeric(), ci.upp = numeric(), t1 = character(), t2 = character())

  
# ANALYSIS - Deaths ----

# This re-defines t1 and t2 to the closests dates as possible as those defined by the user (this is just in case there are missing days in dataset. If not missing days, this is no effect)

t2<-  d.death$date[which.min(abs(d.death$date-t2.define))]
t1<- d.death$date[which.min(abs(d.death$date-(t1.define)))]

# Dt & CI computation
# Simulate poisson error. Call function sim.epi() from the sourced R script
# Output: original dataframe extended with numNewCases (= number of new cases instead of cumulative) + one column per simulation, with variable name being "V.". Simulations output are number of new cases.
d.death.sim<- sim.epi(d.death, its = its, plotsim = FALSE) 


d.death.sim.2<- # format the df correctly to feed in the compute.td() function (from sourced script)
  d.death.sim %>%
  select(-date, -cumNumCases) %>% # remove cumNumCases which become redundant
  mutate_all(~ cumsum(.)) %>%     # compute cumulative incidence of all simulations (+ re-compute it from original data)
  rename(cumNumCases = numNewCases) %>%
  cbind(date = d.death.sim$date) %>%
  select(c(ncol(.), seq(1, ncol(.)-1))) # put variables in the right order for compute.td() function


d.death.sim.2.list<- # Transform into a list to sapply compute.td() on each dataset
  d.death.sim.2 %>%
  select(-date) %>%
  as.list()

Tds.death<- sapply(d.death.sim.2.list,
                   Td.lapply, # Function from sourced script. Simply applies compute.td() on a list of epidemic data
                   dates = d.death.sim.2$date,
                   t1 = t1, t2 = t2)


# Observed and bootstrap distribution of Tds
Td.death.obs<- as.numeric(Tds.death['cumNumCases']) # First element of the list of Tds.deaths is the observed data
Td.death.bootstrap<- as.numeric(Tds.death[-which(names(Tds.death) == 'cumNumCases')]) # And those are the simulated = bootstrap distribution
Td.death.bootstrap<- Td.death.bootstrap[!is.na(Td.death.bootstrap == TRUE)] # NAs come from cases where there were still zero cases observed at t1 in a simulated dataset. Normal to happen when still very low number of cases observed. Will becomes less of a problem as number of cases rise.


# Get CI from distribution of Td
ci.death.low<- round(quantile(Td.death.bootstrap, c(0.05), method = 6), 1)[[1]] # choice of method based on "https://amstat.tandfonline.com/doi/full/10.1080/00031305.2015.1089789#.Xoc0yZNKjBI", see section 4.6
ci.death.upp<- round(quantile(Td.death.bootstrap, c(0.95), method = 6), 1)[[1]]


# Update the doubling time report dataframe with the death doubling time & 95%CI
Td.report<- rbind(Td.report, 
                  data.frame(variable = 'Scotland death',
                             Td.obs = Td.death.obs,
                             ci.low = ci.death.low, ci.upp = ci.death.upp,
                             t1 = t1, t2 = t2))


# ANALYSIS - UK  ----
# This is basically running the exact same process as for death
# Once for each column of the UK data, using a loop over the "regions" vector, where "regions = c('London', 'Scotland', 'Rest of UK')


t2<-  d.uk$date[which.min(abs(d.uk$date-t2.define))]
t1<- d.uk$date[which.min(abs(d.uk$date-(t1.define)))]


regions<- colnames(d.uk)[-1]


for(r in 1:length(regions)){
  
  d.uk.focal<- # Select focal UK region
    d.uk[,c(1, which(colnames(d.uk) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>%
    as.data.frame()
  
  
  d.clean.sim<- sim.epi(d.uk.focal, its = its, plotsim = FALSE)
  
  
  d.clean.sim.norm<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>%
    mutate_all(~ cumsum(.) * (10000/(pops.uk[pops.uk$region == regions[r],'popsize']))) %>% # extra step here: 10k pop. normalisation
    rename(cumNumCases = numNewCases) %>%
    cbind(date = d.clean.sim$date) %>%
    select(c(ncol(.), seq(1, ncol(.)-1)))
  
  
  #t2<- tail(tail(d.clean.sim.norm$date, 8), 1) # t2: latest date
  #t1<- head(tail(d.clean.sim.norm$date, 8), 1) # t1: t2 - 7 available time steps
  
  
  d.clean.sim.norm.list<-
    d.clean.sim.norm %>%
    select(-date) %>%
    as.list()
  
  Tds<- sapply(d.clean.sim.norm.list, Td.lapply, dates = d.clean.sim.norm$date, t1 = t1, t2 = t2)
  
  
  Td.obs<- as.numeric(Tds['cumNumCases'])
  Td.bootstrap<- as.numeric(Tds[-which(names(Tds) == 'cumNumCases')])
  Td.bootstrap<- Td.bootstrap[!is.na(Td.bootstrap == TRUE)] 
  
  
  ci.low<- round(quantile(Td.bootstrap, c(0.05), method = 6), 1)[[1]]
  ci.upp<- round(quantile(Td.bootstrap, c(0.95), method = 6), 1)[[1]]
  

  Td.report<- rbind(Td.report,
                    data.frame(variable = paste(regions[r], 'cases'),
                               Td.obs = Td.obs[[1]],
                               ci.low = ci.low,
                               ci.upp = ci.upp,
                               t1 = t1,
                               t2 = t2)
  )
  
} 

# ANALYSIS - Health boards ----
# Exactly as for the UK data, but this time looping over the Scottish "regions", a.k.a. the Health Boards

regions<- colnames(d.cases)[-1] 

# Extra step here: We store the output of each simulations, in order to compute the cumulative confidence interval for the histogram plot. Declare a list with as many items as there is are health board, and will keep the dataframes with appended simulations in each element of the list.
sims.store<- vector('list',
                    length = length(regions)) 


for(r in 1:length(regions)){
  
  d<- # Additional step: Scottish data have the issue of negative numbers. So run it through data.cleaner() function, which detects those points and smoothe over it. Wrote that function as taking as argument the 'numNewCases', so transmorfing data back to that [...]
    d.cases[,c(1, which(colnames(d.cases) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>%
    mutate(numNewCases = c(cumNumCases[1], diff(cumNumCases))) %>%
    select(date, numNewCases)
  
  d.clean <- # [...] then pass data through cleaner [...]
    data.cleaner(d) %>%
    mutate(cumNumCases = cumsum(numNewCases)) %>%  # [...] and land back on our feet with the cumulative cases, but smoothed.
    select(date, cumNumCases)
  
  
  d.clean.sim<- sim.epi(d.clean, its = its, plotsim = FALSE)
  
  d.clean.sim.norm<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>%
    mutate_all(~ cumsum(.) * (10000/(pops.shb[pops.shb$Health_board == regions[r],'popsize']))) %>%
    rename(cumNumCases = numNewCases) %>%
    cbind(date = d.clean.sim$date) %>%
    select(c(ncol(.), seq(1, ncol(.)-1)))
  
  # Store simulations in the list
  names(sims.store)[r]<- regions[r]
  sims.store[[r]]<- d.clean.sim.norm
  
  # Issue may occir of the focal t1 and t2 happen to be exactly on a day that got "smoothed out".
  # the next two lines allow to select t1 and t2 the closest to the use defined dates
  t2<-  d.clean.sim.norm$date[which.min(abs(d.clean.sim.norm$date-t2.define))]
  t1<- d.clean.sim.norm$date[which.min(abs(d.clean.sim.norm$date-t1.define))]
 
 #print(paste(r, t1, t2))
  
  
  d.clean.sim.norm.list<-
    d.clean.sim.norm %>%
    select(-date) %>%
    as.list()
  
  Tds<- sapply(d.clean.sim.norm.list, Td.lapply, dates = d.clean.sim.norm$date, t1 = t1, t2 = t2)
  
  Td.obs<- as.numeric(Tds['cumNumCases'])
  Td.bootstrap<- as.numeric(Tds[-which(names(Tds) == 'cumNumCases')])
  Td.bootstrap<- Td.bootstrap[!is.na(Td.bootstrap == TRUE)] 
  
  ci.low<- round(quantile(Td.bootstrap, c(0.05), method = 6), 1)[[1]]
  ci.upp<- round(quantile(Td.bootstrap, c(0.95), method = 6), 1)[[1]]
  
  Td.report<- rbind(Td.report,
                    data.frame(variable = paste(regions[r], 'cases'),
                               Td.obs = Td.obs[[1]],
                               ci.low = ci.low,
                               ci.upp = ci.upp,
                               t1 = t1,
                               t2 = t2)
  )
  
} 


# EPIDEMIC PROGRESSION - UK ----
# Take the uk data, format it to be per 10k population, then pass it through the epidemic.diff() function, from sourced script.

test<- d.uk %>%
  gather('region', 'cumCases', 2:4) %>%
  left_join(pops.uk, by = 'region') %>%
  mutate(cumCases10k = cumCases * (100000/popsize)) %>%
  select(-cumCases, -popsize) %>%
  spread('region', 'cumCases10k')


Scotland_vs_London<- epidemic.diff(test, 'Scotland', 'London')
Scotland_vs_rUKxL<- epidemic.diff(test, 'Scotland', 'Rest of UK')

# To use in main body  of the text. If difference is negative, the 'focal' country is behind. If the difference is positive, the 'focal' country is ahead. The main body of the text will automatically be updated to match the value of computed difference.
text.Scotland_vs_London<- ifelse(Scotland_vs_London > 0, 'ahead', 'behind')
text.Scotland_vs_rUKxL<- ifelse(Scotland_vs_rUKxL > 0, 'ahead', 'behind')


# Do it for all pairwise comparison (could use it to make a heatmap..?)
pairwise.comp<- 
expand.grid(
  data.frame(focal = colnames(test)[-1],
             versus = colnames(test[-1]))
)
pairwise.comp$epidemic.diff<- NA
pairwise.comp$focal<- as.character(pairwise.comp$focal)
pairwise.comp$versus<- as.character(pairwise.comp$versus)
for(i in 1:nrow(pairwise.comp)){
  
  pairwise.comp$epidemic.diff[i]<- epidemic.diff(test, pairwise.comp$focal[i], pairwise.comp$versus[i])
  
}

pairwise.comp$focal<- as.character(pairwise.comp$focal)
pairwise.comp$focal[which(pairwise.comp$focal == 'Rest of UK')]<- 'rUKxL'
pairwise.comp$versus<- as.character(pairwise.comp$versus)
pairwise.comp$versus[which(pairwise.comp$versus == 'Rest of UK')]<- 'rUKxL'


pairwise.comp.df<-
  pairwise.comp[order(pairwise.comp$epidemic.diff, na.last = TRUE, decreasing = TRUE), ]
pairwise.comp.df$epidemic.diff.text<- formatC(pairwise.comp.df$epidemic.diff, digits = 1, format = "f")
pairwise.comp.df$focal<- factor(pairwise.comp.df$focal, levels = rev(unique(as.character(pairwise.comp.df$focal))))
pairwise.comp.df$versus<- factor(pairwise.comp.df$versus, levels = unique(as.character(pairwise.comp.df$versus)))

# EPIDEMIC PROGRESSION -  SCOTTISH HB ----


test.HB<- d.cases %>%
  gather('Health_board', 'cumCases', 2:ncol(.)) %>%
  left_join(pops.shb, by = 'Health_board') %>%
  mutate(cumCases10k = cumCases * (100000/popsize)) %>%
  select(-cumCases, -popsize) %>%
  spread('Health_board', 'cumCases10k')


# Do it for all pairwise comparison (could use it to make a heatmap..?)
pairwise.comp.HB<- 
  expand.grid(
    data.frame(focal = colnames(test.HB)[-1],
               versus = colnames(test.HB[-1]))
  )
pairwise.comp.HB$epidemic.diff<- NA
pairwise.comp.HB$focal<- as.character(pairwise.comp.HB$focal)
pairwise.comp.HB$versus<- as.character(pairwise.comp.HB$versus)
for(i in 1:nrow(pairwise.comp.HB)){
  
  pairwise.comp.HB$epidemic.diff[i]<- epidemic.diff(test.HB, pairwise.comp.HB$focal[i], pairwise.comp.HB$versus[i])
  
}


pairwise.comp.HB.df<-
  pairwise.comp.HB[order(pairwise.comp.HB$epidemic.diff, na.last = TRUE, decreasing = TRUE), ]
pairwise.comp.HB.df$epidemic.diff.text<- formatC(pairwise.comp.HB.df$epidemic.diff, digits = 1, format = "f")
pairwise.comp.HB.df$focal<- factor(pairwise.comp.HB.df$focal, levels = rev(unique(as.character(pairwise.comp.HB.df$focal))))
pairwise.comp.HB.df$versus<- factor(pairwise.comp.HB.df$versus, levels = unique(as.character(pairwise.comp.HB.df$versus)))
#upper<- time.diff.df[-which(na.omit(time.diff.df$`Time difference (days)`) < 0),] # This is the dataframe used for the heatmap in final report



# PREP SOME REFORMATTED DATA TO BE USED IN THE RMARKDOWN TEXT ----
# (adjust digits number, renames variables, etc.) 

# Reformatted Td.report for nicer looking table
Td.report.analyses<- Td.report # keep a copy for the figures
Td.report$Td.obs<- formatC(Td.report$Td.obs, digits = 1, format = "f")
Td.report$ci.low<- formatC(Td.report$ci.low, digits = 1, format = "f")
Td.report$ci.upp<- formatC(Td.report$ci.upp, digits = 1, format = "f")


# Dataframes used for both figure 3 and to fetch some numbers used in the text
df.fig3<-
  d.cases %>%
  gather('Health_board', 'cumcases', 2:ncol(d.cases)) %>%
  left_join(pops.shb, by = 'Health_board') %>%
  mutate(cumcases_10k = cumcases * (10000/popsize))


df.fig3b<-
  #d.cases %>%
  #gather('Health_board', 'cumcases', 2:ncol(d.cases)) %>%
  #left_join(pops.shb, by = 'Health_board') %>%
  #mutate(cumcases_10k = cumcases * (10000/popsize))%>%
  df.fig3 %>%
  filter(cumcases_10k > 0) %>%
  select(date, Health_board,cumcases_10k) %>%
  spread(Health_board,cumcases_10k)


# Colorpalette for figures
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 'red', 'green', 'gold', 'cyan')



# RE-RUN ANALYSIS FOR THE PREVIOUS 7 DAYS, OUTPUT THE DOUBLING TIME TABLE ----
t2.define<- t2.define - 7
t1.define<- t2.define - 7


# Dataframe which will be filled throughout this script and write as csv at the end. Collects all Td & 95%CI.
Td.reportMINUS7<- data.frame(variable = character(), Td.obs = numeric(), ci.low = numeric(), ci.upp = numeric(), t1 = character(), t2 = character())


# ANALYSIS - Deaths ----
# This re-defines t1 and t2 to the closests dates as possible as those defined by the user (this is just in case there are missing days in dataset. If not missing days, this is no effect)

t2<-  d.death$date[which.min(abs(d.death$date-t2.define))]
t1<- d.death$date[which.min(abs(d.death$date-(t1.define)))]

# Dt & CI computation
# Simulate poisson error. Call function sim.epi() from the sourced R script
# Output: original dataframe extended with numNewCases (= number of new cases instead of cumulative) + one column per simulation, with variable name being "V.". Simulations output are number of new cases.
d.death.sim<- sim.epi(d.death, its = its, plotsim = FALSE) 


d.death.sim.2<- # format the df correctly to feed in the compute.td() function (from sourced script)
  d.death.sim %>%
  select(-date, -cumNumCases) %>% # remove cumNumCases which become redundant
  mutate_all(~ cumsum(.)) %>%     # compute cumulative incidence of all simulations (+ re-compute it from original data)
  rename(cumNumCases = numNewCases) %>%
  cbind(date = d.death.sim$date) %>%
  select(c(ncol(.), seq(1, ncol(.)-1))) # put variables in the right order for compute.td() function


d.death.sim.2.list<- # Transform into a list to sapply compute.td() on each dataset
  d.death.sim.2 %>%
  select(-date) %>%
  as.list()

Tds.death<- sapply(d.death.sim.2.list,
                   Td.lapply, # Function from sourced script. Simply applies compute.td() on a list of epidemic data
                   dates = d.death.sim.2$date,
                   t1 = t1, t2 = t2)


# Observed and bootstrap distribution of Tds
Td.death.obs<- as.numeric(Tds.death['cumNumCases']) # First element of the list of Tds.deaths is the observed data
Td.death.bootstrap<- as.numeric(Tds.death[-which(names(Tds.death) == 'cumNumCases')]) # And those are the simulated = bootstrap distribution
Td.death.bootstrap<- Td.death.bootstrap[!is.na(Td.death.bootstrap == TRUE)] # NAs come from cases where there were still zero cases observed at t1 in a simulated dataset. Normal to happen when still very low number of cases observed. Will becomes less of a problem as number of cases rise.


# Get CI from distribution of Td
ci.death.low<- round(quantile(Td.death.bootstrap, c(0.05), method = 6), 1)[[1]] # choice of method based on "https://amstat.tandfonline.com/doi/full/10.1080/00031305.2015.1089789#.Xoc0yZNKjBI", see section 4.6
ci.death.upp<- round(quantile(Td.death.bootstrap, c(0.95), method = 6), 1)[[1]]


# Update the doubling time report dataframe with the death doubling time & 95%CI
Td.reportMINUS7<- rbind(Td.reportMINUS7, 
                        data.frame(variable = 'Scotland death',
                                   Td.obs = Td.death.obs,
                                   ci.low = ci.death.low, ci.upp = ci.death.upp,
                                   t1 = t1, t2 = t2))


# ANALYSIS - UK  ----
# This is basically running the exact same process as for death
# Once for each column of the UK data, using a loop over the "regions" vector, where "regions = c('London', 'Scotland', 'Rest of UK')


t2<-  d.uk$date[which.min(abs(d.uk$date-t2.define))]
t1<- d.uk$date[which.min(abs(d.uk$date-(t1.define)))]


regions<- colnames(d.uk)[-1]


for(r in 1:length(regions)){
  
  d.uk.focal<- # Select focal UK region
    d.uk[,c(1, which(colnames(d.uk) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>%
    as.data.frame()
  
  
  d.clean.sim<- sim.epi(d.uk.focal, its = its, plotsim = FALSE)
  
  
  d.clean.sim.norm<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>%
    mutate_all(~ cumsum(.) * (10000/(pops.uk[pops.uk$region == regions[r],'popsize']))) %>% # extra step here: 10k pop. normalisation
    rename(cumNumCases = numNewCases) %>%
    cbind(date = d.clean.sim$date) %>%
    select(c(ncol(.), seq(1, ncol(.)-1)))
  
  
  #t2<- tail(tail(d.clean.sim.norm$date, 8), 1) # t2: latest date
  #t1<- head(tail(d.clean.sim.norm$date, 8), 1) # t1: t2 - 7 available time steps
  
  
  d.clean.sim.norm.list<-
    d.clean.sim.norm %>%
    select(-date) %>%
    as.list()
  
  Tds<- sapply(d.clean.sim.norm.list, Td.lapply, dates = d.clean.sim.norm$date, t1 = t1, t2 = t2)
  
  
  Td.obs<- as.numeric(Tds['cumNumCases'])
  Td.bootstrap<- as.numeric(Tds[-which(names(Tds) == 'cumNumCases')])
  Td.bootstrap<- Td.bootstrap[!is.na(Td.bootstrap == TRUE)] 
  
  
  ci.low<- round(quantile(Td.bootstrap, c(0.05), method = 6), 1)[[1]]
  ci.upp<- round(quantile(Td.bootstrap, c(0.95), method = 6), 1)[[1]]
  
  
  Td.reportMINUS7<- rbind(Td.reportMINUS7,
                          data.frame(variable = paste(regions[r], 'cases'),
                                     Td.obs = Td.obs[[1]],
                                     ci.low = ci.low,
                                     ci.upp = ci.upp,
                                     t1 = t1,
                                     t2 = t2)
  )
  
} 

# ANALYSIS - Health boards ----
# Exactly as for the UK data, but this time looping over the Scottish "regions", a.k.a. the Health Boards

regions<- colnames(d.cases)[-1] 

# Extra step here: We store the output of each simulations, in order to compute the cumulative confidence interval for the histogram plot. Declare a list with as many items as there is are health board, and will keep the dataframes with appended simulations in each element of the list.
sims.store<- vector('list',
                    length = length(regions)) 


for(r in 1:length(regions)){
  
  d<- # Additional step: Scottish data have the issue of negative numbers. So run it through data.cleaner() function, which detects those points and smoothe over it. Wrote that function as taking as argument the 'numNewCases', so transmorfing data back to that [...]
    d.cases[,c(1, which(colnames(d.cases) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>%
    mutate(numNewCases = c(cumNumCases[1], diff(cumNumCases))) %>%
    select(date, numNewCases)
  
  d.clean <- # [...] then pass data through cleaner [...]
    data.cleaner(d) %>%
    mutate(cumNumCases = cumsum(numNewCases)) %>%  # [...] and land back on our feet with the cumulative cases, but smoothed.
    select(date, cumNumCases)
  
  
  d.clean.sim<- sim.epi(d.clean, its = its, plotsim = FALSE)
  
  d.clean.sim.norm<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>%
    mutate_all(~ cumsum(.) * (10000/(pops.shb[pops.shb$Health_board == regions[r],'popsize']))) %>%
    rename(cumNumCases = numNewCases) %>%
    cbind(date = d.clean.sim$date) %>%
    select(c(ncol(.), seq(1, ncol(.)-1)))
  
  # Store simulations in the list
  names(sims.store)[r]<- regions[r]
  sims.store[[r]]<- d.clean.sim.norm
  
  # Issue may occir of the focal t1 and t2 happen to be exactly on a day that got "smoothed out".
  # the next two lines allow to select t1 and t2 the closest to the use defined dates
  t2<-  d.clean.sim.norm$date[which.min(abs(d.clean.sim.norm$date-t2.define))]
  t1<- d.clean.sim.norm$date[which.min(abs(d.clean.sim.norm$date-t1.define))]
  
  #print(paste(r, t1, t2))
  
  
  d.clean.sim.norm.list<-
    d.clean.sim.norm %>%
    select(-date) %>%
    as.list()
  
  Tds<- sapply(d.clean.sim.norm.list, Td.lapply, dates = d.clean.sim.norm$date, t1 = t1, t2 = t2)
  
  Td.obs<- as.numeric(Tds['cumNumCases'])
  Td.bootstrap<- as.numeric(Tds[-which(names(Tds) == 'cumNumCases')])
  Td.bootstrap<- Td.bootstrap[!is.na(Td.bootstrap == TRUE)] 
  
  ci.low<- round(quantile(Td.bootstrap, c(0.05), method = 6), 1)[[1]]
  ci.upp<- round(quantile(Td.bootstrap, c(0.95), method = 6), 1)[[1]]
  
  Td.reportMINUS7<- rbind(Td.reportMINUS7,
                          data.frame(variable = paste(regions[r], 'cases'),
                                     Td.obs = Td.obs[[1]],
                                     ci.low = ci.low,
                                     ci.upp = ci.upp,
                                     t1 = t1,
                                     t2 = t2)
  )
  
} 



Td.reportMINUS7$Td.obs<- formatC(Td.reportMINUS7$Td.obs, digits = 1, format = "f")
Td.reportMINUS7$ci.low<- formatC(Td.reportMINUS7$ci.low, digits = 1, format = "f")
Td.reportMINUS7$ci.upp<- formatC(Td.reportMINUS7$ci.upp, digits = 1, format = "f")



# SAVE & WRITE ----

t2.define<- today 
t1.define<- t2.define - time_window
t2 = t2.define
t1 = t1.define


write.csv(Td.report.analyses, paste0('output/Td_report_', today, '_t1.', t1, '_t2.', t2, '.csv'))
write.csv(Td.reportMINUS7, paste0('output/Td_report_', today, '_PREVIOUS_7_DAYS.csv'))
save.image(paste0('output/AG_briefing_analysis_output_', today, '.RData'))


