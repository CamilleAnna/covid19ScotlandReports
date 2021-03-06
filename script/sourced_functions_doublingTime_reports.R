# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#      C19AG briefing report      #
#        SOURCED SCRIPT           #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# LOADED PACKAGES ----
library(readxl); library(ggplot2); library(dplyr); library(tidyr); library(rdrop2); library(Rmisc); library(lubridate);library("shiny"); library("rsconnect"); library(tidyverse); library(plotly);library(yarrr); library(tidyverse);library(knitr);library(RColorBrewer);library(rJava); library('xlsx')



# ANALYSIS FUNCTIONS ----
data.cleaner<- function(df){
  
  df$date<- as.Date(df$date); df.clean<- df
  
  # Flag problematic datapoints
  negs<- df$date[which(df$numNewCases < 0)];
  flags<- c(negs, negs - 1);
  df.clean$flag<- ifelse(df.clean$date %in% flags, 'flag', 'ok')
  
  # Identify each "streak"" to be able to identify groups of days to average over
  df.clean$streak<- NA;
  streak<- 1;
  df.clean$streak[1]<- streak
  for(i in 2:nrow(df.clean)){
    
    if(df.clean$flag[i] == df.clean$flag[i-1]){
      df.clean$streak[i] = streak
      
    }else{
      
      streak = streak + 1
      df.clean$streak[i] = streak
    }
  }
  
  # Compute average of numNewCases per "streak". Only those over flagged days are releavant
  df.clean <-
    df.clean %>%
    group_by(streak) %>%
    mutate(mean_streak = round(mean(numNewCases), 0),
           midpoint = min(date) + (max(date) - min(date)) / 2) %>%
    ungroup() %>%
    as.data.frame()
  
  # Replacement timepoints
  replacement <-
    df.clean %>%
    filter(flag == 'flag') %>%
    select(midpoint, mean_streak) %>%
    unique() %>%
    rename(date = midpoint, numNewCases = mean_streak)
  
  # Remove the flagged days, replace with new datapoints
  df.out <-
    df.clean %>%
    filter(flag != 'flag') %>%
    select(date, numNewCases) %>%
    rbind(replacement) %>%
    arrange(date) %>%
    as.data.frame()
  
  return(df.out)
  
} # input: cbind(date, numNewCases)


compute.td.m1.v2<- function(dat, user.t1, user.t2){
  
  # Format data
  dat<- filter(dat, cumNumCases > 0)
  
  # Compute Td
  user.t1<- as.Date(user.t1); user.t2<- as.Date(user.t2)
  nb.days<- as.numeric(user.t2 - user.t1)
  user.n.t1<- dat %>% filter(date == user.t1) %>% select(cumNumCases)
  user.n.t2<- dat %>% filter(date == user.t2) %>% select(cumNumCases)
  Td<- round(nb.days/(log2(user.n.t2[[1]]/user.n.t1[[1]])), 1) #same as nb.days * (log(2)/(log(user.n.t2[[1]]/user.n.t1[[1]])))
  
  Td = ifelse(length(Td) > 0, Td, NA) # If cases/deaths have not started yet or started after t1 specified, the Td cannot be computed. Td will be numeric(0), if that is the case make the function spits out an NA.
  
  return(Td)
  
} # input: cbind(date, cumNumCases)

sim.epi<- function(df, its, plotsim = FALSE){
  
  # Format data
  df0 <-
    df %>%
    filter(cumNumCases > 0) %>% # trim data to first reported case
    mutate(date = as.Date(date),
           numNewCases = c(cumNumCases[1], diff(cumNumCases)))
  
  # Simulate epicurves - append simulations to original dataframe
  # Each timepoint, draw a number of new cases from a poisson distribution
  # Mean of rpois() = observed number of new reported cases for that day
  df0<- cbind(df0, as.data.frame(matrix(NA, nrow = nrow(df0), ncol = its)))
  sim.indices<- which(substr(colnames(df0), 1, 1) == "V")
  for(j in 1:nrow(df0)){
    
    df0[j, sim.indices]<- rpois(n = its, lambda = df0$numNewCases[j])
  }
  
  # Plot simulated curves if plotsim = TRUE
  if(plotsim == TRUE){
    plot(cumsum(numNewCases) ~ date, data = df0, type = 'l', lwd = 2)
    for(k in 1:length(sim.indices)){
      sim.k<- df0[,c(1,sim.indices[k])]
      lines(cumsum(sim.k[,2]) ~ sim.k[,1], lwd = 2, col = 'grey')
      rm(sim.k)
    }
    lines(cumsum(numNewCases) ~ date, data = df0, lwd = 3)
    
  }
  
  # Return dataframe with simulated datasets appended
  return(df0)
  
} # cbind(date, cumNumCases)


Td.lapply<- function(cumNumCases, dates, t1, t2){
  d<- data.frame(date = as.Date(dates), cumNumCases = cumNumCases)
  compute.td.m1.v2(dat = d, user.t1 = t1, user.t2 = t2)
}



epidemic.diff<- function(data, focal.country, vs.country){
  
  a1 = NA
  
  data2 = data[,c('date', focal.country, vs.country)]
  data3 = data2[intersect(which(!is.na(data2[,2])), which(!is.na(data2[,3]))), ]
  
  data = data3
  ahead.country = names(which.max(tail(data,1)[2:3]))
  behind.country = names(which.min(tail(data,1)[2:3]))
  
  
  if(focal.country == ahead.country){
    
    a <- data[nrow(data),behind.country]
    for (i in 1:nrow(data)) {
      if (data[i,ahead.country] < a &
          data[i+1,ahead.country] > a){
        a1 <-nrow(data)-i - (a-data[i,ahead.country])/(data[i+1,ahead.country]-data[i,ahead.country])
      }
    }
    
    diff = round(a1[[1]], 1)
    
  }else if(focal.country != ahead.country){
    
    a <- data[nrow(data),behind.country]
    for (i in 1:nrow(data)) {
      if (data[i,ahead.country] < a &
          data[i+1,ahead.country] > a){
        a1 <-nrow(data)-i - (a-data[i,ahead.country])/(data[i+1,ahead.country]-data[i,ahead.country])
      }
    }
    
    diff = round(-a1[[1]], 1)
    
  }
  
  return(diff)
  
}
