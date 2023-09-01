library(table1)
library(tableone)
library(finalfit)
library(dplyr)
library(knitr)
library(sjPlot)
library(stringr)

data <- ABCD5_core

vars <- data %>%
  select(where(is.numeric)) %>%
  colnames() %>%
  str_c('"', ., '"') %>% 
  str_c(collapse = " + ") %>% 
  cat()


x <- names(ABCD5_core)
vars <- paste(x, collapse = " + ")
vars <- noquote(vars)


#make table 1
my.render.cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=4), c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}
my.render.cat <- function(x) {c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))}
my.render.NP_cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=0), c("","Median (IQR)"=sprintf("%s (&plusmn; %s)", MEDIAN, IQR)))}

table <- table1(~  age + sex + reshist_addr1_adi_wsum + reshist_addr1_adi_perc + household.income.3level + household.income.10level + race.6level.p + race.4level.p + race.6level.y + race.4level.y + race_ethnicity + household.educ + married + married.or.livingtogether + rel_family_id + rel_birth_id + demo_roster_v2 + sleepdisturb1_p + sds_p_ss_total + anthro_bmi_calc + BMIz + physical_activity1_y + screentime_total_p + pds_total_py_male + pds_total_py_female|eventname ,  data=data, overall=TRUE, render.continuous=my.render.cont, render.categorical=my.render.cat)

#stratify by var3
table1(~ var1 + var2|var3, data=dat, overall=FALSE, render.continuous=my.render.cont, render.categorical=my.render.cat)

write.csv(table, file = "checkABCD3.csv")
