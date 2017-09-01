#Setting working directory
setwd("C:\\Users\\Admin\\Desktop\\R project")
R_train = read.csv("housing_train.csv", stringsAsFactors = F) #Loading train data
str(R_train) #Looking at train dataset
R_test =  read.csv("housing_test.csv", stringsAsFactors = F) #Loading test data
str(R_test) #Looking at test dataset
R_test$Price = rep(0,1885) #Adding target variable as 0 in test data
str(R_test) #Looking if the price variable got added

library(dplyr) #Loading library dplyr
#Joining both datasets
#Remember 7536 rows belongs to train dataset
R = bind_rows(R_train,R_test)
str(R)

#Checking if we have any NA values in the data set 
apply(R, 2, function(x) any(is.na(x)))

library(mice)  #Let us call library mice
R_alter = R[-c(1,2,3,4,5,6,7,8,9)]  #Droping columns without missing values
R_alter$CouncilArea = NULL 

colnames(R_alter) #Checking remaining columns names
md.pattern(R_alter) #Looking at pattern of missing datapoints

#Let see pattern graphically
library(VIM)
aggr_plot <- aggr(R_alter, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(R_alter), cex.axis=.5, cex.numbers=.5, gap=1, 
                  ylab=c("Histogram of missing data","Pattern"))

R_impute <- mice(R_alter,m=5,maxit=50,meth='pmm',seed=500) #Imputing values
summary(R_impute) #Looking at summary of imputed values
library(lattice) #calling library lattice
stripplot(R_impute, pch = 20, cex = 1.2)
R_complete = complete(R_impute,4) #Choosing 4th set of imputation
str(R_complete)

#Creating a data frame of left over columns
R_final =data.frame(R_complete$Bedroom2, R_complete$Bathroom,
                    R_complete$Car, R_complete$Landsize, R_complete$BuildingArea,
                    R_complete$YearBuilt,R$Suburb, R$Address, R$Rooms, 
                    R$Type, R$Price, R$Method, R$SellerG, R$Distance,
                    R$Postcode, R$CouncilArea, stringsAsFactors = F)

#Bucketing method into categories
table(R_final$R.Method)

R_final<- R_final%>%
  mutate(M_1= as.numeric(R.Method %in% c("S")),
         M_2= as.numeric(R.Method %in% c("PI")),
         M_3=as.numeric(R.Method %in% c("SP")))

R_final<-R_final %>% select(-R.Method)#Drop

#Conveting type into numeric
table(R_final$R.Type)
R_final<- R_final%>%
  mutate(T_1= as.numeric(R.Type %in% c("h")),
         T_2= as.numeric(R.Type %in% c("u")))
R_final<-R_final %>% select(-R.Type)#Drop

str(R_final) #View dataset

#Let us convert CouncilArea into numeric
table(R_final$R.CouncilArea)
unique(R_final$R.CouncilArea)

R_final<- R_final%>%
  mutate(A_1= as.numeric(R.CouncilArea %in% c("Banyule")),
         A_2= as.numeric(R.CouncilArea %in% c("Bayside")),
         A_3= as.numeric(R.CouncilArea %in% c("Boroondara")),
         A_4= as.numeric(R.CouncilArea %in% c("Brimbank")),
         A_5= as.numeric(R.CouncilArea %in% c("Darebin")),
         A_6= as.numeric(R.CouncilArea %in% c("Glen Eira")),
         A_7= as.numeric(R.CouncilArea %in% c("Hobsons Bay")),
         A_8= as.numeric(R.CouncilArea %in% c("Hume")),
         A_9= as.numeric(R.CouncilArea %in% c("Kingston")),
         A_10= as.numeric(R.CouncilArea %in% c("Manningham")),
         A_11= as.numeric(R.CouncilArea %in% c("Maribyrnong")),
         A_12= as.numeric(R.CouncilArea %in% c("Melbourne")),
         A_13= as.numeric(R.CouncilArea %in% c("Monash")),
         A_14= as.numeric(R.CouncilArea %in% c("Moonee Valley")),
         A_15= as.numeric(R.CouncilArea %in% c("Moreland")),
         A_16= as.numeric(R.CouncilArea %in% c("Port Phillip")),
         A_17= as.numeric(R.CouncilArea %in% c("Stonnington")),
         A_18= as.numeric(R.CouncilArea %in% c("Whitehorse")),
         A_19= as.numeric(R.CouncilArea %in% c("Yarra")))
R_final<-R_final %>% select(-R.CouncilArea)#Drop

#Converting Suburb into numeric
sort(table(R$Suburb), decreasing = T)

R_final=R_final%>%
  mutate(S_1= as.numeric(R.Suburb %in% c("Reservoir","Bentleigh East", "Richmond")),
         S_2= as.numeric(R.Suburb %in% c("Preston","St Kilda","Brunswick",
                                       "South Yarra","Essendon")),
         S_3= as.numeric(R.Suburb %in% c("Glen Iris","Brighton","Glenrow","Kew")),
         S_4= as.numeric(R.Suburb %in% c("Northcote","Coburg","Balwyn North","Hawthorn")),
         S_5= as.numeric(R.Suburb %in% c("Brighton East","Pascoe Vale","Benntleigh",
                                       "Port Melbourne","Carnegie")),
         S_6= as.numeric(R.Suburb %in% c("Camberwell","Malvern East","Balwyn","Hampton"
                                       ,"Yarraville","Moonee Ponds","Thornbury")),
         S_7= as.numeric(R.Suburb %in% c("Ascot Vale","Footscary","Doncaster","Newport",
                                       "Maribyrnong","Elwood","Kensington","Sunshine")))
R_final <- R_final %>% select(-R.Suburb) #Drop Suburb

#Checking levels into address
sort(table(R_final$R.Address), decreasing = T)
R_final<- R_final %>% select(-R.Address) #Drop Address

#Converting SellerG into numeric
sort(table(R_final$R.SellerG),decreasing = T)
R_final=R_final%>%
  mutate(Se_1 = as.numeric(R.SellerG %in% c("Nelson")),
         Se_2 = as.numeric(R.SellerG %in% c("Jellis")),
         Se_3 = as.numeric(R.SellerG %in% c("hockingstuart")),
         Se_4 = as.numeric(R.SellerG %in% c("Barry","Marshall")),
         Se_5 = as.numeric(R.SellerG %in% c("Buxton","Ray","Biggin")),
         Se_6 = as.numeric(R.SellerG %in% c("Biggin","Brad","Woodards","Fletchers")),
         Se_7 = as.numeric(R.SellerG %in% c("RT","Sweeney","Greg","Miles","Jas")),
         Se_8 = as.numeric(R.SellerG %in% c("Gary","Noel","McGrath",
                                          "Hodges","Kay","Stockdale")))
R_final <- R_final %>% select(-R.SellerG) #Drop SellerG

#Splitting the dataset back 
R_train <- R_final[1:7536,]
R_test <- R_final[7537:9421,]

#Fitting linear regression
R_fit=lm(R.Price ~ . , data=R_train)
summary(R_fit)

#Looking for vif
library(car)
R_vif <- vif(R_fit) 
sort(R_vif, decreasing = T) 
#Since all features have vif less than vif we will not drop any feature

#Fitting linear regression again
R_fit=lm(R.Price ~ . , data=R_train)
summary(R_fit)
#Drop S_4 and fit lnear regression again 
R_fit=lm(R.Price ~ .-S_4 , data=R_train)
summary(R_fit)

#Drop S_7 after S_4
R_fit=lm(R.Price ~ . -S_7 -S_4, data=R_train)
summary(R_fit)


#Drop Se_1 after S_7 and S_4
R_fit=lm(R.Price ~ .-Se_1 -S_7 -S_4 , data=R_train)
summary(R_fit)


#Drop A_6 after Se_1, S_7, S_4
R_fit=lm(R.Price ~ .-A_6 -Se_1 -S_7 -S_4 , data=R_train)
summary(R_fit)


#Drop A_12 after A_6, Se_1 S_7, A_12, S_4
R_fit=lm(R.Price ~ .-A_6 -Se_1 -S_7 -S_4 -A_12 , data=R_train)
summary(R_fit)


#Drop Se_8 after A_6, A_12, Se_1, S_7, Se_1, S_4
R_fit=lm(R.Price ~ .-A_6 -Se_8 -A_12 -S_7 -S_4 -Se_1 , data=R_train)
summary(R_fit)

#Drop Se_6 after A_6, A_12, Se_1, S_7, Se_1, S_4
R_fit=lm(R.Price ~ .-A_6 -Se_8 -Se_6 -S_7 -A_12 -S_4 -Se_1 , data=R_train)
summary(R_fit)

#Drop M_3 after A_6, A_12, Se_1, S_7, Se_1, S_4, Se_6
R_fit=lm(R.Price ~ .-M_3 -A_6 -Se_8 -Se_6 -S_7 -A_12 -S_4 -Se_1 , data=R_train)
summary(R_fit)

#Drop A_14 after A_6, A_12, Se_1, S_7, Se_1, S_4, Se_6, M_3
R_fit=lm(R.Price ~ . -A_14 -A_6 -M_3 -Se_8 -Se_6 -S_7 -A_12 -S_4 -Se_1 , data=R_train)
summary(R_fit)

#Drop A_10 after A_6, A_12, Se_1, S_7, Se_1, S_4, Se_6, M_3 A_14
R_fit=lm(R.Price ~ . -A_10 -A_14 -A_6 -M_3 -Se_8 -Se_6 -S_7 -A_12 -S_4 -Se_1 , data=R_train)
summary(R_fit)

#Drop A_13 after A_6, A_12, Se_1, S_7, Se_1, S_4, Se_6, M_3 A_10
R_fit=lm(R.Price ~ . -A_13 -A_10 -A_14 -A_6 -M_3 -Se_8 -Se_6 -S_7 -A_12 -S_4 -Se_1 , data=R_train)
summary(R_fit)

R_train$yes<- predict(R_fit,R_train) #Getting predictions
str(R_train)

#Getting residuals
R_train$Residuals <- R_train$R.Price - R_train$yes #Actual - Predicted = errors
str(R_train)
#Putting actual, predicted and errors in a data frame
RMSE <- sqrt(mean(R_train$Residuals^2))
RMSE
212467/RMSE
#Making prediction on the test data
Price<- predict(R_fit,R_test) #Getting predictions
str(Price)

#Submitting the file 
submit = data.frame (Price= R_test$Price)
#Giving name to the file
write.csv(submit, file = "Atul_Kumar_P1_part2.csv", row.names = FALSE)

