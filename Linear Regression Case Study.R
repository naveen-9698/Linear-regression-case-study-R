#Linear Regression Case Study - Credit Card Spend Prediction

data = read.csv('Linear Regression Case.csv')
summary(data)

#1) Creating a new variable called TotalSpend by adding card1spend and card2spend

data$TotalSpend = data$cardspent + data$card2spent
data$TotalSpend

#2) Removing the log variables and categorical features which are numerical features, and removing cardspent and card2spent

data_new = subset(data, select = -c(cardspent,card2spent,lninc,lncreddebt,lnothdebt,lnlongmon,lnlongten,lntollmon,lntollten,lnequipmon,lnequipten,lncardmon,lncardten,lnwiremon,lnwireten,carditems,card2items,custid,birthmonth))
data_new

#3) Separating Numerical columns
#a) Continuous variables

cont_var = subset(data_new,select = -c(region,townsize,gender,agecat,edcat,jobcat,union,
                                       empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                       homeown,hometype,address,addresscat,cars,carown,cartype,
                                       carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                                       commutemotorcycle,commutecarpool,commutebus,commuterail,
                                       commutepublic,commutebike,commutewalk,commutenonmotor,
                                       telecommute,reason,polview,polparty,polcontrib,vote,card,
                                       cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                                       card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                                       active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                       voice,pager,internet,callid,callwait,forward,confer,ebill,
                                       owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                       news,response_01,response_02,response_03))

#b) Coverting to numeric type
cont_var= as.data.frame(lapply(cont_var,as.numeric))
str(cont_var)

#c) 
#i)Outlier analysis

outlier = function(x){
  UC = quantile(x, p=0.95,na.rm=T)
  LC = quantile(x, p=0.05,na.rm=T)
  x[x>UC]=UC
  x[x<LC]=LC
  
  return(x)
}

cont_var = data.frame(apply(cont_var, 2, FUN=outlier))

cont_var

#ii)Missing Value treatment
sapply(cont_var, function(x) sum(is.na(x)))
#Replacing those with their means
cont_var = sapply(cont_var, function(x) {x = replace(x, is.na(x), mean(x, na.rm=TRUE))})
apply(cont_var, 2, function(x) sum(is.na(x)))


#d) Categorical variables
cat_var= subset(data_new,select = c(region,townsize,gender,agecat,edcat,jobcat,union,
                                    empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                    homeown,hometype,address,addresscat,cars,carown,cartype,
                                    carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                                    commutemotorcycle,commutecarpool,commutebus,commuterail,
                                    commutepublic,commutebike,commutewalk,commutenonmotor,
                                    telecommute,reason,polview,polparty,polcontrib,vote,card,
                                    cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                                    card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                                    active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                    voice,pager,internet,callid,callwait,forward,confer,ebill,
                                    owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                    news,response_01,response_02,response_03))

#e) Converting to factor
cat_var= as.data.frame(lapply(cat_var,as.factor))
str(cat_var)

#f)Missing value treatment
sapply(cat_var, function(x) sum(is.na(x)))
#Replacing those with their modes
cat_var = sapply(cat_var, function(x) {x = replace(x, is.na(x), which.max(prop.table(table(x))))})
apply(cat_var, 2, function(x) sum(is.na(x)))

#4)Combining the categorical and continuous columns as one dataframe
data_final = cbind.data.frame(cont_var, cat_var)
summary(data_final)


#5)a) Checking if the distribution of TotalSpend follows normal distribution
hist(data_final$TotalSpend, col = "red")

#b)Since, it does not follow, applying log transformation to it
data_final$ln_TotalSpend=log(data_final$TotalSpend)
hist(data_final$ln_TotalSpend, col= "red")

#6) Applying correlation
corr= cor(cont_var)
View(corr)

#7) Perform Anova with 95% confidence level
#a) Categorical variables 
cats=aov(ln_TotalSpend~ region+townsize+gender+agecat+edcat+jobcat+union+empcat+retire+inccat+default+jobsat+marital+spousedcat+homeown+hometype+address+addresscat+cars+carown+cartype+carcatvalue+carbought+carbuy+commute+commutecat+commutecar+commutemotorcycle+commutecarpool+commutebus+commuterail+commutepublic+commutebike+commutewalk+commutenonmotor+telecommute+reason+polview+polparty+polcontrib+vote+card+cardtype+cardbenefit+cardfee+cardtenure+cardtenurecat+card2+card2type+card2benefit+card2fee+card2tenure+card2tenurecat+active+bfast+churn+tollfree+equip+callcard+wireless+multline+voice+pager+internet+callid+callwait+forward+confer+ebill+owntv+ownvcr+owndvd+owncd+ownpda+ownpc+ownipod+owngame+ownfax+news+response_01+response_02+response_03,
         data = data_final) 

summary(cats)

library(MASS)
step= stepAIC(cats,direction = "both")
ls(step)
step$anova

#b) Numerical variables
nums= subset(data_final,select = -c(region,townsize,gender,agecat,edcat,jobcat,union,
                                    empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                    homeown,hometype,address,addresscat,cars,carown,cartype,
                                    carcatvalue,carbought,carbuy,commute,commutecat,commutecar,
                                    commutemotorcycle,commutecarpool,commutebus,commuterail,
                                    commutepublic,commutebike,commutewalk,commutenonmotor,
                                    telecommute,reason,polview,polparty,polcontrib,vote,card,
                                    cardtype,cardbenefit,cardfee,cardtenure,cardtenurecat,card2,
                                    card2type,card2benefit,card2fee,card2tenure,card2tenurecat,
                                    active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                    voice,pager,internet,callid,callwait,forward,confer,ebill,
                                    owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                    news,response_01,response_02,response_03))
nums_new=lm(ln_TotalSpend~. ,data = nums)
summary(nums_new)

step= stepAIC(nums_new,direction = "both")
ls(step)
step$anova

#c) Store the final categorical and final numerical variables in separate variables
num_final = subset(data_final,select = c(ln_TotalSpend, age,pets_dogs,pets_birds,commutetime,longmon,longten,tollmon,tollten,wireten,hourstv,TotalSpend))
cat_final = subset(data_final,select = c(region,gender,edcat,retire,inccat,default,carown,reason,card,card2,pager,internet,ownvcr,owndvd,response_03,cardtenurecat))
str(num_final)
str(cat_final)

cat_final= as.data.frame(lapply(cat_final,as.factor))
str(cat_final)

#d) Merging both the dataframes
data_final_new= cbind.data.frame(num_final,cat_final)
data_final_new

#8) Model
#a) Splitting into training and testing
train_set <- sample(1:nrow(data_final_new), size = floor(0.70 * nrow(data_final_new)))

train<-data_final_new[train_set,]
test<-data_final_new[-train_set,]
train
test

#b) Training data
train_fit= lm(ln_TotalSpend ~ .,data = train)
summary(train_fit)

#c) Step wise regression AIC
#library(MASS)
#step= stepAIC(train_fit,direction="both")
#ls(step)
#step$anova

#train_fit_1 = lm(ln_TotalSpend ~ pets_dogs + commutetime + longmon + longten + 
#                   wireten + TotalSpend + edcat + retire + inccat + card2 + 
#                   owndvd + response_03 + cardtenurecat, data = train)
#summary(train_fit_1)

#d) Dropping the insignificant variables
fit_sign<- lm(ln_TotalSpend ~ pets_dogs + inccat + card2 + owndvd, data = train)
summary(fit_sign)

#e) Checking Multicollinearity
library(car)
vif(fit_sign)
#Since the VIF of longmon,longten are greater than 2.5, we discard them

train_fit_2 = lm(ln_TotalSpend ~ pets_dogs+inccat+card2+owndvd, data = train)
summary(train_fit_2)

#9) Trying to remove the influential outliers by performing Cook's D method
#Dividing by total number of rows
train$cooks= cooks.distance(train_fit_2)
train_new= subset(train,cooks<(4/3500))

final = lm(ln_TotalSpend ~ pets_dogs+inccat+card2+owndvd, data = train_new)
summary(final)

#9) Predicting the values
pred1=exp(predict(final,newdata = train))
train_fin= cbind(train,pred_spend=pred1)

pred2= exp(predict(final,newdata = test))
test_fin= cbind(test,pred_spend=pred2)

#10) Correlation between actual and predicted values
cor_train = cor(train_fin$pred_spend,train_fin$TotalSpend)
cor_train

cor_test = cor(test_fin$pred_spend,test_fin$TotalSpend)
cor_test






