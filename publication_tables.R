# examples of code for stepwise logistic regression and making table 1 and regression tables for publication

#useful publication packages
library(lme4) # regression functions
library(table1) # for making table 1
library(tableone) # for getting p-values for univariate analysis table 1
library(sjPlot) # plots
library(psych) # descriptive statistics
library(ggpubr) #ggplot for scientific publications


# this is nicely formated table1, add pvalues from tableone function below
table1(~ sex + age + townsend + birth_latitude + #demography
         BMI_cat +  healthy + sleep_cat + chronotype + child_obesity + #physiology
         alcohol_intake + time_outdoors_summer +  #lifestype
         smoker + smoke_start + smoke_20yr  + #smoking
         early_NSW_YN + early_SW_YN + NSW_YN + SW_YN # shiftwork 15-20
       | NDD, data=UKB_masterSW[UKB_masterSW$agebracket=="15-20",], overall=FALSE, render.missing = NULL, render.continuous=my.render.cont, render.categorical=my.render.cat)


# Create Table 1 stratified by MS to get p-values
tableOne <- CreateTableOne(vars = c('sex' , 'age' , 'townsend' , 'birth_latitude' , #demography
                                    'BMI_cat' ,  'healthy' , 'sleep_cat' , 'chronotype' , 'child_obesity' , #physiology
                                    'alcohol_intake' , 'time_outdoors_summer' ,  #lifestype
                                    'smoker' , 'smoke_start' , 'smoke_20yr'  , #smoking
                                    'early_NSW_YN' , 'early_SW_YN' , 'NSW_YN' , 'SW_YN' ),
                           strata = c("MS_YN"), 
                           data = UKB_masterSW[UKB_masterSW$agebracket=="15-20",], 
                           #test = FALSE, 
                           factorVars = c())

#make table one
my.render.cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=4), c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}
my.render.cat <- function(x) {c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))}
my.render.NP_cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=0), c("","Median (IQR)"=sprintf("%s (&plusmn; %s)", MEDIAN, IQR)))}

#template table <- table1(~  age + sex | eventname ,  data=data, overall=TRUE, render.continuous=my.render.cont, render.categorical=my.render.cat)

# format continuous vars with 95% CI
my.render.cont = function(x) {c("", "mean (95% CI)"=sprintf("%s (%s, %s)",
                                                            round(stats.default(x)$MEAN,2),
                                                            round(stats.default(x)$MEAN-qt(0.975,stats.default(x)$N-1)*stats.default(x)$SD/sqrt(stats.default(x)$N),2),
                                                            round(stats.default(x)$MEAN+qt(0.975,stats.default(x)$N-1)*stats.default(x)$SD/sqrt(stats.default(x)$N),2)
))
}

#check 95% CI
t.test(tabledata[tabledata$MS==1,]$age)$conf.int
sample_mean <- mean(tabledata[tabledata$MS==1,]$age)
sample_sd <- sd(tabledata[tabledata$MS==1,]$age)
sample_size <- length(tabledata[tabledata$MS==1,]$age) 
df <- sample_size - 1
se <- sample_sd / sqrt(sample_size)
margin_of_error <- qt(0.975, df) * se
lower_ci <- sample_mean - margin_of_error
upper_ci <- sample_mean + margin_of_error



# make regression tables
# MS           
MSyr_model1 <- glm(MS_YN ~ age + sex + townsend + centre,
                   data = shiftwork_years, family = "binomial")
MSyr_model2 <- glm(MS_YN ~ age + sex + townsend + centre 
                   + child_obesity + early_NSW_YN + chronotype,
                   data = shiftwork_years, family = "binomial")
MSyr_model3 <- glm(MS_YN ~ age + sex + townsend + centre 
                   + child_obesity  +early_SW_YN+ chronotype
                   + alcohol_intake + smoker + time_outdoors_summer + SW_dose_yr,
                   data = shiftwork_cum_pre2015, family = "binomial")
tab_model(MSyr_model1, MSyr_model2, MSyr_model3 ,show.intercept=FALSE)
summary(MSyr_model3 )