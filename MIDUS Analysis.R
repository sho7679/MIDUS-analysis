##Analysis of relationships between mid-life variables and depression in MIDUS study

setwd("/Users/Sam1/Downloads/QTM lab")
getwd()
MIDUS <-read.csv("midus_p4.csv")
head(MIDUS)

#Recode response Depression variable to better name
MIDUS$Depression<-MIDUS$B4QCESD
head(MIDUS$Depression)  
summary(MIDUS$Depression)

#Cleaning variables
#Recoding and cleaning Income
MIDUS$Income<-MIDUS$B1STINC1
head(MIDUS$Income)
MIDUS$Income2<-MIDUS$Income
MIDUS$Income==9999998
MIDUS$Income2[MIDUS$Income2==9999998]<-NA
summary(MIDUS$Income2) #worked, max is 300000
hist(MIDUS$Income2)

#Recoding and cleaning sleep quality 
MIDUS$Sleep<-MIDUS$B4SSQ_GS
head(MIDUS$Sleep)
MIDUS$Sleep2<-MIDUS$Sleep
MIDUS$Sleep2[MIDUS$Sleep2==98]<-NA
hist(MIDUS$Sleep2)
summary(MIDUS$Sleep2)

#Recoding and cleaning self-rated health
MIDUS$SelfHealth<-MIDUS$B1PA1
head(MIDUS$SelfHealth)
MIDUS$SelfHealth2<-MIDUS$SelfHealth
MIDUS$SelfHealth2[MIDUS$SelfHealth2==7]<-NA
summary(MIDUS$SelfHealth2)
#Changing to factor variables
MIDUS$SelfHealthF <- factor(MIDUS$SelfHealth2, labels=c("Excellent","Very good", "Good", "Fair", "Poor"))
table(MIDUS$SelfHealth2, MIDUS$SelfHealthF)
summary(MIDUS$SelfHealthF)

#Recoding and cleaning Sarcastic comments
MIDUS$Sarcasm <-MIDUS$B4Q5K
head(MIDUS$Sarcasm)
#Create new empty variable and rename 
MIDUS$SarcasmNew <- factor(NA,levels=c("almost never","sometimes","often","almost always"))
MIDUS$SarcasmNew[MIDUS$Sarcasm=="(1) ALMOST NEVER"] <-"almost never"
MIDUS$SarcasmNew[MIDUS$Sarcasm=="(2) SOMETIMES"] <- "sometimes"
MIDUS$SarcasmNew[MIDUS$Sarcasm=="(3) OFTEN"] <- "often"
MIDUS$SarcasmNew[MIDUS$Sarcasm=="(4) ALMOST ALWAYS"] <- "almost always"
MIDUS$SarcasmNew[MIDUS$Sarcasm=="8"] <-NA
table(MIDUS$Sarcasm, MIDUS$SarcasmNew)
summary(MIDUS$SarcasmNew)

#New clean dataset - omit NAs
midus_clean<-na.omit(MIDUS)
summary(midus_clean)

#Recode response variable to dichotomous 
#Create empty shell
midus_clean$DepressionCategory <- factor(NA, levels=c("Depressive symptoms","No significance"))
#Assign values 
midus_clean$DepressionCategory[midus_clean$Depression>=16]<-"Depressive symptoms"
midus_clean$DepressionCategory[midus_clean$Depression<16]<-"No significance"
variable<-factor(midus_clean$Depression, levels=c("Depressive symptoms","No significance"))
table(midus_clean$Depression,midus_clean$DepressionCategory)

#Recode one categorical variable to dichotomous 
#Create new variable for sarcasm
midus_clean$SarcasmCategory<-factor(NA, levels=c("no sarcasm","some sarcasm"))
midus_clean$SarcasmCategory[midus_clean$SarcasmNew=="almost never"]<-"no sarcasm"
midus_clean$SarcasmCategory[midus_clean$SarcasmNew=="sometimes"|midus_clean$SarcasmNew=="often"|midus_clean$SarcasmNew=="almost always"]<-"some sarcasm"
table(midus_clean$SarcasmNew,midus_clean$SarcasmCategory)

#Recoding Household income into levels
midus_clean$IncomeCategory<-factor(NA, levels=c("Low income","Middle income","High income"))
midus_clean$IncomeCategory[midus_clean$Income2>=42000]<-"Middle income"
midus_clean$IncomeCategory[midus_clean$Income2<=125000]<-"Middle income"
midus_clean$IncomeCategory[midus_clean$Income2>125000]<-"High income"
midus_clean$IncomeCategory[midus_clean$Income2<42000]<-"Low income"
variable2<-factor(midus_clean$Income2,levels = c("Low income","Middle income", "High income"))
table(midus_clean$Income2,midus_clean$IncomeCategory)
summary(midus_clean$IncomeCategory)

#Visualize relationships between  response variable and explanatory variables.
#Depression vs. self-rated health (categorical vs. categorical - bar plot)
Dep.Health.tab <- table(midus_clean$DepressionCategory,midus_clean$SelfHealthF)
addmargins(Dep.Health.tab)
propDH <- prop.table(Dep.Health.tab,margin = 2)
propDH
barplot(propDH, beside=F, legend.text = T,
        xlab = "Self-Rated Health",
        ylab = "Proportion of CES-D Score")

#Depression score vs. Sarcastic Comment Category
boxplot(midus_clean$Depression~midus_clean$SarcasmCategory,
        xlab = "Sarcastic Comment Category",
        ylab = "CES-D Score")

#Depression score vs. Household income category
boxplot(midus_clean$Depression~midus_clean$IncomeCategory,
        xlab="Household Income Level",
        ylab="CES-D Score")

#Depression score vs. PSQI score
plot(midus_clean$Sleep2,midus_clean$Depression,
     ylab = "CES-DScore",
     xlab = "PSQI Score")

##### Statistical Testing ######

##### Table 1:descriptive stats
#CES-D score
summary(midus_clean$Depression)
sd(midus_clean$Depression)

#Self rated health   
summary(midus_clean$SelfHealthF)
SelfHealth.table<-table(midus_clean$SelfHealthF)
addmargins(SelfHealth.table)
prop.table(SelfHealth.table)
#association between self rated health and depression category 
anova.health2<-aov(midus_clean$Depression~midus_clean$SelfHealthF)
summary(anova.health2)

#PSQI score  
summary(midus_clean$Sleep2)  
sd(midus_clean$Sleep2)


#Descriptive stats for level of income
addmargins(table(midus_clean$IncomeCategory))
prop.table(table(midus_clean$IncomeCategory))

#Sarcastic comment category - "some" or "none"
summary(midus_clean$SarcasmNew)
SarcasmCat.table<-table(midus_clean$SarcasmCategory)
addmargins(SarcasmCat.table)
prop.table(SarcasmCat.table)

##### Table 2: CES-D dichotomous vs self-rated health --> chi squared, but cell count <5 so fisher test
#Overall
Dep.cat.table<-table(midus_clean$DepressionCategory)
addmargins(Dep.cat.table)
prop.table(Dep.cat.table) #depressive = 12.87%, no sig = 87.13%

#Dichotomous CES-D category vs. self-rated health 
table(midus_clean$DepressionCategory,midus_clean$SelfHealthF)
Dep.selfhealth.table<-table(midus_clean$DepressionCategory,midus_clean$SelfHealthF)
addmargins(Dep.selfhealth.table)
prop.table(Dep.selfhealth.table)  

#Fisher test of assocation
Dep.Hlth.test<-chisq.test(midus_clean$DepressionCategory,midus_clean$SelfHealthF,correct=F)
Dep.Hlth.test$expected #cell count <5, so use fisher test
fisher.test(midus_clean$DepressionCategory,midus_clean$SelfHealthF,simulate.p.value = TRUE) #p=0.0004998
#p value<0.05, so reject Ho


##### Table 3: CES-D score vs. dichotomous sarcastic comments --> two sample t test 
#Check skew
hist(midus_clean$Depression[midus_clean$SarcasmCategory=="no sarcasm"],main="No sarcastic comments",xlab = "CES-D Score")
#right skewed, but n>30 so ok 
hist(midus_clean$Depression[midus_clean$SarcasmCategory=="some sarcasm"],main = "Sarcastic comments",xlab = "CES-D score")
#Right skewed, but n>30 so ok

#Equal variance? 
sd(midus_clean$Depression[midus_clean$SarcasmCategory=="no sarcasm"])
#"no sarcasm" mean=6.53, sd=7.22
sd(midus_clean$Depression[midus_clean$SarcasmCategory=="some sarcasm"])
#"some sarcasm" mean = 8.92, sd=7.94
#equal variance

#Conduct 2 sample t test w equal variance
t.test(midus_clean$Depression~midus_clean$SarcasmCategory,var.equal=TRUE)
#p=2.641e-06 --> reject Ho


##### Table 4: ANOVA for CES-D score vs. household income category

#Assumptions: distribution of CES-D score should be normal in each group and equal variance
boxplot(midus_clean$Depression~midus_clean$IncomeCategory, 
        xlab = "Income Level",
        ylab = "CES-D Score") #don't seem to be different, similar variance
library(mosaic)
favstats(midus_clean$Depression~midus_clean$IncomeCategory)
par(mfrow=c(1,3),pty="s")
hist(midus_clean$Depression[midus_clean$IncomeCategory=="Low income"],xlab = "CES-D Score",main = "Low income")
hist(midus_clean$Depression[midus_clean$IncomeCategory=="Middle income"],xlab = "CES-D Score",main="Middle income")
hist(midus_clean$Depression[midus_clean$IncomeCategory=="High income"],xlab = "CES-D Score", main = "High income")
dev.off()   
#similar spread, similar stdev in each group
#all right skewed, but n>30 so okay

#ANOVA test
anova.income<-aov(midus_clean$Depression~midus_clean$IncomeCategory)
summary(anova.income)
#p=0.00867 -> reject Ho, at least one mean differs significantly

#Pairwise comparisons
TukeyHSD(anova.income)
plot(TukeyHSD(anova.income))
#Only middle-low income has significant difference, p=0.01
#Middle has significantly lower CES-D score than low


###### Table 5: CES-D score vs. sleep quailty score -> linear regression
#Assumptions for linear regression
plot(midus_clean$Depression,midus_clean$Sleep2) #no clear trend
#Estimate correlation
cor(midus_clean$Depression,midus_clean$Sleep2) #0.4667691 -> moderate positive correlation
#Significantly different from 0?
cor.test(midus_clean$Depression,midus_clean$Sleep2)
#CI does not contain 0, p<0.05 so signficantly different

#Simple linear regression
plot(midus_clean$Depression,midus_clean$Sleep2)
m1<-lm(midus_clean$Depression~midus_clean$Sleep2)
summary(m1) #y=1.81400+1.05179x
abline(m1)
confint(m1) 

#Check residuals 
m1$residuals
rstandard(m1) #standardized residuals
hist(rstandard(m1))
qqnorm(rstandard(m1))
qqline(rstandard(m1))
favstats(rstandard(m1))
sd(rstandard(m1))
mean(rstandard(m1))
#slightly right skewed, mean close to 0, sd=1
#normality ok because n>30

#Plot residuals vs. fitted values
plot(predict(m1),rstandard(m1),xlab = "Fitted Values",ylab = "Standardized Residuals")
abline(h=0,lty=2) #fanned around y=0, constant variance not satisfied

