
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(purrr)
library(vtable)
library(ggplot2)
library(lme4)
library(lmerTest)
library(nlme)
library(sjPlot)

#reading---------------------------------------
mgmt <- read.csv("mgmt2.csv", header = T, stringsAsFactors = F)
#str(mgmt)
#View(mgmt)

#variable string and day count extraction------------
non.id <- names(subset(mgmt, select = -Assessment.ID))#take out ID variable from names; creating list of surveys

survey_plus <- non.id %>% 
  str_split("Day") %>% unlist() %>% 
  map_chr(str_extract, "^[:alpha:]+") %>% 
  unique() %>% na.omit() %>% as.character()

surveys <- survey_plus %>% .[-(which(. == "Case"))]

num_days <- mgmt %>%
  names() %>%
  str_extract("[:digit:]{1,}$") %>%
  as.numeric() %>%
  max(na.rm = T)

#reverse scoring
#x is variable/survey, y is question number to reverse score, n is max possible likert value for survey
rev_score <- function(survey = x, questionN = y, maxLikert = n){
  for(i in 1:num_days){
    mgmt[, which(
      names(mgmt) == paste0(
        deparse(substitute(x)),
        deparse(substitute(y)),
        "Day",
        i))] <<- mgmt[, which(
                        names(mgmt) == paste0(
                          deparse(substitute(x)),
                          deparse(substitute(y)),
                          "Day",
                          i))] %>%
      map_dbl(~ (n + 1) - .x)
  }
}

rev_score(Complain, 3, 5)
rev_score(Fatigue, 1, 5)

# item_pull <- function(x, y, z){
#   x[, which(
#     names(x) == paste0(
#       deparse(substitute(y)),
#       deparse(substitute(z)),
#       "Day",
#       i))]
# }
# 
# rev_score <- function(x, nQ){
#   map_dbl(x, ~ (nQ + 1) - .x)
# }

#survey separation/scoring --------------------
list_arrange <- function(df){ #x = surveys, df = mgmt
  assign("data_list_unscored", list(), .GlobalEnv)
  for(i in 1:length(surveys)){
    assign(paste0(surveys[i], "_responses"),
           df %>% select(matches(paste0("^", surveys[i])))
    ) ->> data_list_unscored[[i]]
  }
}
list_arrange(mgmt)
names(data_list_unscored) <- surveys

#enter survey name to view df with survey-specific responses
unscored_survey_df <- function(i){
  View(data_list_unscored[[deparse(substitute(i))]])
}
scored_survey_df <- function(i){
  View(data_list_scored[[deparse(substitute(i))]])
}

#scoring---------------------------------

assign("data_list_scored", list(), .GlobalEnv)

for(i in 1:length(surveys)){
  data_list_scored[[i]] <- data.frame(
    matrix(
      data = rep(NA, times = nrow(data_list_unscored[[i]])*num_days),
      ncol = num_days, nrow = nrow(data_list_unscored[[i]])))
  data_list_scored[[i]] <- data_list_scored[[i]] %>%
    setNames(., c(paste0(surveys[i], "Day", 1:num_days)))
}

for(i in 1:length(surveys)){
  for(j in 1:num_days){
    data_list_scored[[i]][, paste0(surveys[i], "Day", j)] <- data_list_unscored[[i]][, str_subset(names(data_list_unscored[[i]]), paste0("Day", j, "$"))] %>%
    as.data.frame() %>% apply(1, mean)
    #ifelse(mean(is.na(as.data.frame(data_list_unscored[[i]][, str_subset(names(data_list_unscored[[i]]), paste0("Day", j, "$"))]))) >= .6, apply(as.data.frame(data_list_unscored[[i]][, str_subset(names(data_list_unscored[[i]]), paste0("Day", j, "$"))]), 1, mean), NA)
      #how many values involved in mean
  }
}
names(data_list_scored) <- surveys

#long format----------------------------
for (i in 1:length(surveys)){
  data_list_scored[[i]] <- data_list_scored[[i]] %>% tibble::rownames_to_column()
  colnames(data_list_scored[[i]])[1] <- "id"
  
  data_list_scored[[i]] <- data_list_scored[[i]] %>%
    select(id, contains(surveys[i])) %>%
    pivot_longer(
           cols = contains(paste0(surveys[i])),
           names_to = "time",
           values_to = surveys[i])
  
  data_list_scored[[i]]$time <- data_list_scored[[i]]$time %>%
    str_extract("[:digit:]{1,}") %>% as.numeric()
}

#re-df-----------------------------------------
df1 <- bind_cols(data_list_scored[1:length(surveys)])
df <- bind_cols(select(df1, id, time), df1[surveys])

#analysis df w/ time, OCB, proactive performance
adf_miss <- df[c(1, 2, 13, 15, 16)]
adf <- adf_miss %>% filter(!is.na(PrevFocus) & !is.na(TaskPerf) & !is.na(OCB))


#visualization --------------------------------
set.seed(1)
filter(adf, id %in% base::sample(adf$id, size = 20, replace = F)) %>% 
  ggplot(aes(x = time, y = TaskPerf)) +
  geom_point(alpha = .6) + 
  geom_smooth(method = "lm", se =F) +
  theme_bw() + facet_wrap(~id)

#centering ---------------------------------
adf$time_c <- adf$time - mean(adf$time)
adf$PrevFocus_c <- adf$PrevFocus - mean(adf$PrevFocus)
adf$OCB_c <- adf$OCB - mean(adf$OCB)

#modeling -----------------------------------
m0 <- lmer(data = adf, TaskPerf ~ time + (1 + time|id),
           REML = F,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m0, show.aic = T, show.r2 = F, show.ci = F, show.se = T)

m1 <- lmer(data = adf, TaskPerf ~ 1 + (1|id),
           REML = F,
           control = lmerControl(optimizer = "bobyqa",
                     optCtrl = list(maxfun = 200000)))
tab_model(m0, m1, show.aic = T, show.r2 = F, show.ci = F, show.se = T)

m2 <- lmer(data = adf, TaskPerf ~ OCB_c + (1|id),
           REML = F,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m1, m2, show.aic = T, show.r2 = F, show.ci = F, show.se = T)

m3 <- lmer(data = adf, TaskPerf ~ PrevFocus_c + (1|id),
           REML = F,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m2, m3, show.aic = T, show.r2 = F, show.ci = F, show.se = T)

m4 <- lmer(data = adf, TaskPerf ~ OCB_c + PrevFocus_c + (1|id),
           REML = F,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m3, m4, show.aic = T, show.r2 = F, show.ci = F, show.se = T)

m5 <- lmer(data = adf, TaskPerf ~ OCB_c*PrevFocus_c + (1|id),
           REML = F,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m4, m5, show.aic = T, show.r2 = F, show.ci = F, show.se = T)

m6 <- lmer(data = adf, TaskPerf ~ OCB_c + PrevFocus_c + (1 + OCB_c + PrevFocus_c|id),
           REML = F,
           control = lmerControl(optimizer = "bobyqa",
                                 optCtrl = list(maxfun = 200000)))
tab_model(m4, m6, show.aic = T, show.r2 = F, show.ci = F, show.se = T)

#model 6 is a winner

#tryCatch for Shiny app