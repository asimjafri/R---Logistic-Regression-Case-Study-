
data <- read.csv("C:/Users/aa/OneDrive/Documents/Data science 360/Assignments & Projects/R case study/5. R - Logistic Regression case study/Proactive Attrition Management-Logistic Regression Case Study.csv")
View(data)

### Checking the data type ###
View(data.frame(sapply(data,class)))

### Removing insignificant variables ###
data$CSA <- NULL
data$CUSTOMER <- NULL


### Creating UDF for descriptive analysis ###
stats <- function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, var=var, range=range, pctl=pctl))
}

analysis <- data.frame(t(apply(data, 2, stats)))
View(analysis)

data$CALIBRAT <- NULL
data$CHURNDEP <- NULL
data$CALLFWDV <- NULL

### Splitting the dataset ###
numerical <- subset(data, select = c( REVENUE, MOU, RECCHRGE, DIRECTAS, OVERAGE, ROAM, CHANGEM, CHANGER, DROPVCE, 
                                      BLCKVCE, UNANSVCE, CUSTCARE, THREEWAY, MOUREC, OUTCALLS, INCALLS,  PEAKVCE, 
                                      OPEAKVCE, DROPBLK, CALLWAIT, MONTHS, UNIQSUBS, ACTVSUBS, PHONES,  
                                      MODELS, EQPDAYS, AGE1, AGE2,  RETCALLS, RETACCPT,  REFER, INCOME,CREDITAD, SETPRC))

categorical <- subset(data, select = - c( REVENUE, MOU, RECCHRGE, DIRECTAS, OVERAGE, ROAM, CHANGEM, CHANGER, DROPVCE, 
                                          BLCKVCE, UNANSVCE, CUSTCARE, THREEWAY, MOUREC, OUTCALLS, INCALLS,  PEAKVCE, 
                                          OPEAKVCE, DROPBLK, CALLWAIT, MONTHS, UNIQSUBS, ACTVSUBS, PHONES,  
                                          MODELS, EQPDAYS, AGE1, AGE2,RETCALLS, RETACCPT,REFER, INCOME,CREDITAD, SETPRC))

### Missing value treatment ###
missing_value_treat <- function(x){
  x[is.na(x)] = mean(x, na.rm = T)
  return(x)
}

data_missing_treated <- data.frame(apply(numerical, 2, FUN = missing_value_treat))
missing_treated <- data.frame(t(apply(data_missing_treated, 2, stats)))
View(missing_treated)


### Outlier Treatment ###
outlier_treat <- function(x){
  UC = quantile(x, p = 0.99, na.rm = T)
  LC = quantile(x, p = 0.01, na.rm = T)
  x = ifelse(x>UC, UC, x)
  x = ifelse(x<LC, LC, x)
  return(x)
}

data_outlier_treated <- data.frame(apply(data_missing_treated, 2, FUN = outlier_treat))
data_treated <- cbind(data_outlier_treated,categorical)

analysis_treated <- data.frame(t(apply(data_treated, 2, stats)))
View(analysis_treated)


### Correlation ###
correlation <- data.frame(cor(data_treated))
View(correlation)

require(writexl)
writexl::write_xlsx(correlation, 'correlation_data.xlsx')


### Significance Test - Categorical Variables ###

colnames(categorical)

significance_test = xtabs(~CHILDREN + CHURN, data = data_treated) # not significant         
chisq.test(significance_test)

significance_test = xtabs(~CREDITA + CHURN, data = data_treated)                  
chisq.test(significance_test)

significance_test = xtabs(~CREDITAA + CHURN, data = data_treated)                  
chisq.test(tab)

significance_test = xtabs(~CREDITB + CHURN, data = data_treated)                  
chisq.test(significance_test)

significance_test = xtabs(~CREDITC + CHURN, data = data_treated)                  
chisq.test(significance_test)

significance_test = xtabs(~CREDITDE + CHURN, data = data_treated)                  
chisq.test(significance_test)

significance_test = xtabs(~CREDITGY + CHURN, data = data_treated) # not significant         
chisq.test(significance_test)

significance_test = xtabs(~CREDITZ + CHURN, data = data_treated) # not significant         
chisq.test(significance_test)

significance_test = xtabs(~PRIZMRUR + CHURN, data = data_treated)                  
chisq.test(significance_test)

significance_test = xtabs(~PRIZMUB + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~PRIZMTWN + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~REFURB + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~WEBCAP + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~TRUCK + CHURN, data = data_treated) # not significant     
chisq.test(significance_test)

significance_test = xtabs(~RV + CHURN, data = data_treated) # not significant
chisq.test(significance_test)

significance_test = xtabs(~OCCPROF + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~OCCCLER + CHURN, data = data_treated) # not significant      
chisq.test(significance_test)

significance_test = xtabs(~OCCCRFT + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~OCCSTUD + CHURN, data = data_treated) # not significant    
chisq.test(significance_test)

significance_test = xtabs(~OCCHMKR + CHURN, data = data_treated) # not significant     
chisq.test(significance_test)

significance_test = xtabs(~OCCRET + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~OCCSELF + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~OWNRENT + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~MARRYUN + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~MARRYYES + CHURN, data = data_treated)                
chisq.test(significance_test)

significance_test = xtabs(~MARRYNO + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~MAILORD + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~MAILRES + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~MAILFLAG + CHURN, data = data_treated) #Not Significant  
chisq.test(significance_test)

significance_test = xtabs(~TRAVEL + CHURN, data = data_treated) # Not Significant
chisq.test(significance_test)

significance_test = xtabs(~PCOWN + CHURN, data = data_treated) # Not Significant
chisq.test(significance_test)

significance_test = xtabs(~CREDITCD + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~NEWCELLY + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~NEWCELLN + CHURN, data = data_treated) # Not Significant  
chisq.test(significance_test)

significance_test = xtabs(~INCMISS + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~RETCALL + CHURN, data = data_treated)                 
chisq.test(significance_test)

significance_test = xtabs(~MCYCLE + CHURN, data = data_treated) # Not Significant
chisq.test(significance_test)

significance_test = xtabs(~SETPRCM + CHURN, data = data_treated)                
chisq.test(significance_test)


data_treated$CHILDREN<-NULL
data_treated$CREDITGY<-NULL
data_treated$CREDITZ<-NULL
data_treated$TRUCK<-NULL
data_treated$RV<-NULL
data_treated$OCCCLER<-NULL
data_treated$OCCSTUD<-NULL
data_treated$OCCHMKR<-NULL
data_treated$MAILFLAG<-NULL
data_treated$TRAVEL<-NULL
data_treated$PCOWN<-NULL
data_treated$NEWCELLN<-NULL
data_treated$MCYCLE<-NULL


##### Stepwise Regression #####

# Creating a full and an empty model
# Full Model
modelF <- lm(CHURN~.,data=data_treated)
modelF

# Empty Model
modelnull <- lm(CHURN~1,data=data_treated)
modelnull

# Running the Stepwise Regression to find the imp variables
stepwise.model <- step(modelnull, scope = list(upper = modelF),   data = data_treated, direction = "both")

#  EQPDAYS + RETCALLS + AGE1 + CREDITDE + REFURB + MONTHS + 
#  OVERAGE + MOU + CHANGEM + UNIQSUBS + ACTVSUBS + PHONES + 
#  CHANGER + ROAM + DROPBLK + PEAKVCE + CREDITC + WEBCAP + MAILRES + 
#  SETPRC + THREEWAY + RETACCPT + PRIZMRUR + DROPVCE + CREDITAD + 
#  CREDITCD + MARRYNO + PRIZMUB + RECCHRGE + CUSTCARE + REVENUE + 
#  SETPRCM + CREDITA + NEWCELLY + PRIZMTWN + REFER + UNANSVCE + 
#  INCALLS + CALLWAIT

selected_Vars <- c(
  "EQPDAYS" , "RETCALLS" , "AGE1" , "CREDITDE" , "REFURB" , "MONTHS" , 
    "OVERAGE" , "MOU" , "CHANGEM" , "UNIQSUBS" , "ACTVSUBS" , "PHONES" , 
    "CHANGER" , "ROAM" , "DROPBLK" , "PEAKVCE" , "CREDITC" , "WEBCAP" , "MAILRES" ,
    "SETPRC" , "THREEWAY" , "RETACCPT" , "PRIZMRUR" , "DROPVCE" , "CREDITAD" , 
    "CREDITCD" , "MARRYNO" , "PRIZMUB" , "RECCHRGE" , "CUSTCARE" , "REVENUE" , 
    "SETPRCM" , "CREDITA" , "NEWCELLY" , "PRIZMTWN" , "REFER" , "UNANSVCE" , 
    "INCALLS" , "CALLWAIT","CHURN"
)

length(selected_Vars)

df <- data_treated[selected_Vars]
View(df)


### Numerical Variable ###
install.packages("skimr")
library(skimr)
data_skimr <- skim(df)
View(data_skimr)


df$MOU <- sqrt(df$MOU)
hist(df$MOU)

df$RECCHRGE  <- sqrt(df$RECCHRGE)
hist(df$RECCHRGE )

df$MONTHS  <- log(df$MONTHS)
hist(df$MONTHS )

df$EQPDAYS  <- sqrt(df$EQPDAYS)
hist(df$EQPDAYS )

df$CALLWAIT  <- log(df$CALLWAIT)
hist(df$CALLWAIT )

df$INCALLS  <- log(df$INCALLS)
hist(df$INCALLS )

df$THREEWAY  <- log(df$THREEWAY)
hist(df$THREEWAY )

df$CUSTCARE  <- log(df$CUSTCARE)
hist(df$CUSTCARE )

df$DROPVCE  <- log(df$DROPVCE)
hist(df$DROPVCE )

df$ROAM  <- log(df$ROAM)
hist(df$ROAM )

df$AGE1 <- log(df$AGE1)
hist(df$AGE1  )

df$OVERAGE  <- log(data_treated$OVERAGE)
hist(data_treated$OVERAGE)


#### Splitting data into training and testing ####
samp <- sample(1:nrow(df), size = floor(0.70 * nrow(df)))

training <- df[samp,]
testing <- df[-samp,]


#### Logistic Regression Model ####

# Running Iterations
model1 <- glm(CHURN~
                EQPDAYS+                                         
                RETCALLS+                                        
                AGE1+                                            
                CREDITDE+                                        
                REFURB+
                MONTHS+
                OVERAGE+
                MOU+
                CHANGEM+
                UNIQSUBS+
                ACTVSUBS+
                PHONES+
                CHANGER+
                ROAM+
                DROPBLK+
                PEAKVCE+
                CREDITC+
                WEBCAP+
                MAILRES+
                SETPRC+
                THREEWAY+
                RETACCPT+
                PRIZMRUR+
                DROPVCE+
                CREDITCD+
                MARRYNO+
                PRIZMUB+
                RECCHRGE+
                CUSTCARE+
                CREDITA,
              data = training,
              family = binomial(logit))

summary(model1)


#### Model Evaluation ####
# Assuming the input is a stored binomial GLM object
Concordance = function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  ones_and_zeros = data.frame(ones, zeros)
  
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0}
    
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
   
     # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
    }
  }
  
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  Somers_D<-conc_rate - disc_rate
  gamma<- (conc_rate - disc_rate)/(conc_rate + disc_rate)
  
  #k_tau_a<-2*(sum(conc)-sum(disc))/(N*(N-1)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties),
              somers_D=Somers_D, Gamma=gamma))
  
}

Concordance(model1) 
#Output - 0.6205674


## Training(Development) ##
dev1= cbind(training, Prob=predict(model1, type="response")) 
View(dev1)

# Creating Deciles
decLocations = quantile(dev1$Prob, probs = seq(0.1,0.9,by=0.1))
dev1$decile = findInterval(dev1$Prob,c(-Inf,decLocations, Inf))
View(dev1)

require(dplyr)
dev1$decile=factor(dev1$decile)
str(dev1)

decile_grp = group_by(dev1,decile)
decile_summ_dev = summarize(decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), default_cnt = sum(CHURN), non_default_cnt=total_cnt -default_cnt)
decile_summ_dev = arrange(decile_summ_dev, desc(decile))

View(decile_summ_dev)


# Decile Analysis
require(sqldf)
fit_dev_DA = sqldf("select decile, min(Prob) as Min_prob,
                      max(Prob) as max_prob,
                      sum(CHURN) as default_cnt
                      from dev1
                      group by decile
                      order by decile desc")
View(fit_dev_DA)

## Testing(Validation) ## 
val1 = cbind(testing,prob=predict(model1,testing,type = "response"))
View(val1)

# Decile Analysis
delocations1 = quantile(val1$prob, probs = seq(0.1,0.9,by=0.1), na.rm = TRUE)
val1$decile = findInterval(val1$prob, c(-Inf,delocations1,Inf))

View(val1)

val1$decile = factor(val1$decile)
str(val1)

decile_grp = group_by(val1,decile)
decile_summ1 = summarize(decile_grp, total_cnt = n(), min_prob = min(prob), max_prob = max(prob), default_cnt=sum(CHURN), non_default_cnt=total_cnt - default_cnt)
View(decile_summ1)

fit_test = sqldf("select decile, min(prob) as min_prob,count(prob) as total,
                   max(prob) as max_prob,
                   sum(CHURN) as default_cnt
                   from val1
                   group by decile
                   order by decile desc")
View(fit_test)
###############################################################################################################

### Q1.Data cleaning including missing values, outliers and multi-collinierity. Describe your predictive churn model. How did you select variables to be included in the model?###

 # Ans. I selected variables in the model by performing various Data preparation steps like first 
 # splitting data into numerical and categorical data then performing different treatments like missing
 # values and outliers then using various techniques like Chi Square & Stepwise Regression to select
 # variables that are significant for the model.


### Q2. Demonstrate the predictive performance of the model. ###
 
 # Ans. The predictive performance of the model is 62.05% on the both the traning and testing dataset.  


### Q3. What are the key factors that predict customer churn? Do these factors make sense? ###

 # Ans. The factors for the model are : 
 #       Numbers of calls previously made retention team
 #       Number of the days of the current equipment
 #       Age of first HH members    
 #       Low credit rating 
 #       Months in Service
 #       Mean overage minutes of use
 #       Mean monthly minutes of use
 #       % Change in minutes of use
 #       Number of unique subs
 #       Number of active subs
 #       Handsets issued
 #       % Change in revenue
 #       Mean number of roaming calls
 #       Mean number of dropped or blocked calls
 #       Mean number of in and out peak voice calls
 #       Medium credit rating
 #       Handset is web capable
 #       Responds to mail offers
 #       Handset price
 #       Mean number of threeway calls
 #       Number of previous retention offers accepted
 #       Prizm code is rural
 #       Mean number of dropped voice calls
 #       Possesses a credit card
 #       Not married
 #       Prizm code suburban
 #       Mean total recurring charge
 #       Mean number of customer care calls
 #       Highest credit rating
 # Yes, these factors do make sense


### Q4. What offers should be made to which customers to encourage them to remain with Cell2Cell? Assume that your objective is to generate net positive cash flow, i.e., generate additional customer revenues after subtracting out the cost of the incentive. ###

 # Ans. The company must respond quickly and effectively to the customer's concern. The wireless-telecom sector has a 
       # slew of issues, including sluggish networks, billing problems, and accessibility. Cell2Cell 
       # may set up a system where a client can learn how Cell2Cell resolves their issues, the estimated 
       # time of resolution, and how to offer the contact information for the appropriate person for
       # further enquiry. Company would like customers to sign up for more products, assuring a rise in revenue as well
       # as customer retention, because. customer's retention rate in the telecom sector is directly 
       # related to the number of products they purchase from a firm.


### Q5. Assuming these actions were implemented, how would you determine whether they had worked? ###
 
 # Ans. Personalized e-mails or SMS marketing campaigns can  be used to track whether the actions 
        # implemented has worked or not.