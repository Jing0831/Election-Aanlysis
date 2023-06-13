data<-`county_statistics.(1)`[1:3111,]
data$perc_donny<-data$percentage20_Donald_Trump*100
data$perc_joe<-data$percentage20_Joe_Biden*100
Covid_lm_rep_20<-lm(votes20_Donald_Trump~cases+deaths, data=data)
Covid_lm_dem_20<-lm(votes20_Joe_Biden~cases+deaths, data=data)
summary(Covid_lm_rep_20)#suggests that higher numbers of cases result in more votes, but higher numbers of deaths result in less votes
summary(Covid_lm_dem_20)#suggests tha higher numbers of cases result in greater voter turnout, number of deaths was not a significant factor
#It is likely that these models are skewed by correlations with other variables and that high number of cases just resulted in larger voter turnout in general

gender_lm_rep<-lm(votes20_Donald_Trump~Men+Women, data=data)
summary(gender_lm_rep)#Donald Trump seems to be much more favorable with male voters than female. 
gender_lm_dem<-lm(votes20_Joe_Biden~Men+Women, data=data)
summary(gender_lm_dem)#both male and female voters are positively reltated to total Biden votes, suggesting this is more to do with overall voter turnout.  
per_gen_rep_lm<-lm(perc_donny~perc_female+perc_male, data=data)
summary(per_gen_rep_lm)
per_gen_dem_lm<-lm(perc_joe~perc_female+perc_male, data=data)
summary(per_gen_dem_lm)

race_lm_rep<-lm(perc_donny~Hispanic+White+Black+Native+Asian+Pacific, data=data)
summary(race_lm_rep)#An adjusted r squared vaklue of only .22 suggests that this model is not a good representation of the variance in total Trump Votes
race_lm_dem<-lm(perc_joe~Hispanic+White+Black+Native+Asian+Pacific, data=data)
summary(race_lm_dem)#Adjusted r squared in very low with a value of only .28, do not recommend the model for use. 

Financial_lm_rep<-lm(votes20_Donald_Trump~IncomePerCap+Poverty+ChildPoverty, data=data)
summary(Financial_lm_rep)
Financial_lm_dem<-lm(votes20_Joe_Biden~IncomePerCap+Poverty+ChildPoverty, data=data)
summary(Financial_lm_dem)
data$perc_male<-data$men/data$both_genders

Job_lm_rep<-lm(perc_donny~Professional+Service+Office+Construction+Production,data=data)
summary(Job_lm_rep)
Job_lm_dem<-lm(perc_joe~Professional+Service+Office+Construction+Production,data=data)
summary(Job_lm_dem)

Work_Type_lm_rep<-lm(perc_donny~PrivateWork+PublicWork+SelfEmployed,data=data)
summary(Work_Type_lm_rep)
Work_Type_lm_dem<-lm(perc_joe~PrivateWork+PublicWork+SelfEmployed,data=data)
summary(Work_Type_lm_dem)

Transport_lm_rep<-lm(perc_donny~Drive+Carpool+Transit+Walk+OtherTransp+WorkAtHome,data=data)
summary(Transport_lm_rep)
Transport_lm_dem<-lm(perc_joe~Drive+Carpool+Transit+Walk+OtherTransp+WorkAtHome,data=data)
summary(Transport_lm_dem)

data$both_genders<-data$Men+data$Women
data$perc_male<-(data$Men/data$both_genders)*100
data$perc_female<-(data$Women/data$both_genders)*100

Complete_lm_rep<-lm(perc_donny~perc_cases+perc_female+perc_male+Hispanic+White+Black+Native+Asian+Pacific+Poverty+ChildPoverty+Professional+Service+Office+Construction+Production+PrivateWork+PublicWork+SelfEmployed+Drive+Carpool+Transit+Walk+OtherTransp+WorkAtHome,data=data)
summary(Complete_lm_rep)
back_sel_rep<-step(Complete_lm_rep, direction = 'backward', trace=F)
summary(back_sel_rep)
step_sel_rep<-step(Complete_lm_rep, direction = 'both', trace=F)
summary(step_sel_rep)
edited_lm_rep<-lm(perc_donny~perc_female+perc_male+Hispanic+White+Black+Native+Asian+Pacific+Poverty+ChildPoverty+Construction+PrivateWork+PublicWork+SelfEmployed+Drive+Carpool+Transit+Walk+WorkAtHome,data=data)
edit_sel_rep<-step(edited_lm_rep, direction = 'backward',trace=F)
summary(edited_step_lm)
Complete_lm_dem<-lm(perc_joe~perc_cases+perc_female+perc_male+Hispanic+White+Black+Native+Asian+Pacific+Poverty+ChildPoverty+Construction+PrivateWork+PublicWork+SelfEmployed+Drive+Carpool+Transit+Walk+OtherTransp+WorkAtHome,data=data)
summary(Complete_lm_dem)
back_sel_dem<-step(Complete_lm_dem, direction = 'backward', trace=F)
summary(back_sel_dem)
edited_lm_dem<-Complete_lm_dem<-lm(perc_joe~perc_female+perc_male+Hispanic+White+Black+Native+Asian+Pacific+Poverty+ChildPoverty+Construction+PrivateWork+PublicWork+SelfEmployed+Drive+Carpool+Transit+Walk+WorkAtHome,data=data)
summary(edited_lm_dem)
edit_sel_dem<-step(edited_lm_dem, direction='backward', trace=F)
summary(edit_sel_dem)
sum(data$cases)
data$total_cases<-sum(data$cases)
data$perc_cases<-(data$cases/data$total_cases)*100
data<-na.omit(data)
library(car)
summary(edit_sel_rep)
summary(edit_sel_dem)
vif(edit_sel_rep)
vif(edit_sel_dem)
cor(data$PrivateWork)
edited_lm_rep2<-lm(perc_donny~perc_female+PublicWork+PrivateWork+perc_male+Hispanic+White+Black+Native+Asian+Pacific+Poverty+ChildPoverty+Construction+Drive+Carpool+Transit+Walk+WorkAtHome,data=data)
edit_sel_rep2<-step(edited_lm_rep2, direction = 'backward',trace=F)
summary(edit_sel_rep2)
qqnorm(resid(edit_sel_rep))
bptest(edit_sel_dem)
install.packages("lmtest")
library(lmtest)
