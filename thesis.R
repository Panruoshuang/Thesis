# Read data and handle missing values
data1<-IDC_WW_Mobile_Phone_Tracker_2004_2019_Sep2020_Part_I
data2<-IDC_WW_Mobile_Phone_Tracker_2004_2019_Sep2020_Part_II
data1 <- as.data.frame(data1)
data2 <- as.data.frame(data2)
identical(colnames(data1), colnames(data2))
data <- rbind(data1, data2)
# Handle blank (or empty) values
data[data == ""] <- NA
data <- data[complete.cases(data1),]
str(data)
# Select character variables and convert them to factor type
convert <- c(1:11,13:23,25:30,32,34:35,37,38)
for(i in convert){
  data[,i] <- as.factor(data[,i])
}
# Eliminate redundant variables
useless <- c()
for(i in convert){
  if (length(levels(data[,i])) < 2){
    useless <- c(useless, i)
  }
}
data <- data[, -useless]
# Remove N/A
str(data)
data <- data[data$`Screen Resolution` != 'N/A',]
data <- data[data$`Biometric Authentication` != 'None',]
data <- data[data$GPS != 'N/A',]
data <- data[data$`Senior Device` != '[N/A]',]
data <- data[data$NFC != 'N/A',]
data <- data[data$Waterproofing != 'N/A',]
write.table(data,'data.txt')
data <- read.csv("data.txt", sep="")
factor <- c(1:11,13:21,23:27,29,31,33,34)
for(i in factor){
  data[,i] <- as.factor(data[,i])
}


# Description Analysis

# Create a list to store the subsets for each year
yearly_subsets <- list()

# Define the years from 2004 to 2019 as character strings
years <- as.character(2004:2019)

# Loop through each year and create the subsets
for (year in years) {
  subset <- data[data$Year == year, ]
  yearly_subsets[[year]] <- subset
}

df_RAM<-data.frame(RAM = c(mean(sub2014$`RAM (GB)`),mean(sub2015$`RAM (GB)`),mean(sub2016$`RAM (GB)`),mean(sub2017$`RAM (GB)`),mean(sub2018$`RAM (GB)`),mean(sub2019$`RAM (GB)`)),Year = c(2014,2015,2016,2017,2018,2019))
df_Storage<-data.frame(Storage= c(mean(sub2014$`Storage (GB)`),mean(sub2015$`Storage (GB)`),mean(sub2016$`Storage (GB)`),mean(sub2017$`Storage (GB)`),mean(sub2018$`Storage (GB)`),mean(sub2019$`Storage (GB)`)),Year = c(2014,2015,2016,2017,2018,2019))
df_SZ<-data.frame(ScreenSize = c(mean(sub2014$`Screen Size`),mean(sub2015$`Screen Size`),mean(sub2016$`Screen Size`),mean(sub2017$`Screen Size`),mean(sub2018$`Screen Size`),mean(sub2019$`Screen Size`)),Year = c(2014,2015,2016,2017,2018,2019))
df_ProcessorSpeed<-data.frame(ProcessorSpeed= c(mean(sub2014$`Processor Speed (GHz)`),mean(sub2015$`Processor Speed (GHz)`),mean(sub2016$`Processor Speed (GHz)`),mean(sub2017$`Processor Speed (GHz)`),mean(sub2018$`Processor Speed (GHz)`),mean(sub2019$`Processor Speed (GHz)`)),Year = c(2014,2015,2016,2017,2018,2019))
df_ASP<-data.frame(ASP= c(mean(sub2014$`ASP (USD)`),mean(sub2015$`ASP (USD)`),mean(sub2016$`ASP (USD)`),mean(sub2017$`ASP (USD)`),mean(sub2018$`ASP (USD)`),mean(sub2019$`ASP (USD)`)),Year = c(2014,2015,2016,2017,2018,2019))
df_ASP<-data.frame(ASP= c(mean(sub2014$`ASP (USD)`),mean(sub2015$`ASP (USD)`),mean(sub2016$`ASP (USD)`),mean(sub2017$`ASP (USD)`),mean(sub2018$`ASP (USD)`),mean(sub2019$`ASP (USD)`)),Year = c(2014,2015,2016,2017,2018,2019))
ggplot(df_RAM, aes(x = Year, y = RAM)) +
  geom_line() +
  labs(title = "Trend of RAM Changes",
       x = "Year",
       y = "RAM") +
  theme_minimal()+geom_line()

ggplot(df_Storage, aes(x = Year, y = Storage)) +
  geom_line() +
  labs(title = "Trend of Storage Changes",
       x = "Year",
       y = "Storage") +
  theme_minimal()+geom_line()

ggplot(df_SZ, aes(x = Year, y = ScreenSize)) +
  geom_line() +
  labs(title = "Trend of Screen Size Changes",
       x = "Year",
       y = "ScreenSize") +
  theme_minimal()+geom_line()

ggplot(df_ProcessorSpeed, aes(x = Year, y = ProcessorSpeed)) +
  geom_line() +
  labs(title = "Trend of Processor Speed Changes",
       x = "Year",
       y = "ProcessorSpeed") +
  theme_minimal()+geom_line()
    
ggplot(df_ASP, aes(x = Year, y = ASP)) +
  geom_line() +
  labs(title = "Trend of ASP Changes",
       x = "Year",
       y = "ASP") +
  theme_minimal()+geom_line()
                 
# Feature selection based on Boruta
library(Boruta)
set.seed(123)
boruta.train <- Boruta(data[,1:36],data[,37] ,doTrace = 2, maxRuns = 15)
print(boruta.train)
save(boruta.train,file = 'boruta.Rdata')
# Visualization
pdf('Boruta.pdf', height = 6, width = 10)
par(mar=c(8,4,2,2) + 0.1) 
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
dev.off()
colnames(data)
# Selection based on Boruta
data <- data[,-c(3, 10, 24, 31)]
# lasso feature selection
library(glmnet)
y <- data.matrix(data$Value..USD.)
x <- data.matrix(data[,1:32])
mice::md.pattern(x) #  Missing values
f1 = glmnet(x, y, family="gaussian", nlambda=100, alpha=1)
plot(f1,xvar="lambda",lable=T)
cvfit=cv.glmnet(x,y,family="gaussian",alpha=1)
plot(cvfit)
set.seed(220)
# Determine the value of λ that minimizes the mean squared error
cvfit$lambda.min 
# Find the λ value that is one standard error away from the minimum mean squared error
cvfit$lambda.1se
# Selection based on coef1
coef1<-coef(cvfit$glmnet.fit,s = 0.1400297,exact = F)
data <- data[,-c(1,3,4,7,8,9,12,13,15,16,17,20:22,24,29,30)]
write.table(data,'data.txt')
# SVM model
library(janitor)
library(caret)
library(e1071)
library(ranger)
library(nnet)
# read data
dat <- read.table("data.txt", sep = " ", row.names = 1)
factor <- c(1:3,5:11,13)
for(i in factor){
  dat[,i] <- as.factor(dat[,i])
}
dat <- clean_names(dat)
dat[,c(1:3,5:11,13)] <- lapply(dat[,c(1:3,5:11,13)], function(x){as.factor(x)})

# Delete variables with too many levels
dat <- dat[,-c(1,5,7,9,11,13)]
# Transfer dummies
dmy <- dummyVars(~.,data = dat, fullRank = TRUE)
dat1 <- data.frame(predict(dmy, newdata = dat))
# Feature importance ranking
dat2<-dat1[,-20]
library(rminer)
model <- fit(value_usd~., dat2,model = "svm")
variable.importance <- Importance(model,dat2,method = "sensv")
L=list(runs=1,sen=t(variable.importance$imp),sresponses=variable.importance$sresponses)
mgraph(L,graph = "IMP",leg = names(dat1),col = "gray", Grid = 4,)
# create 5 folds CV
set.seed(1)
folds <- createFolds(y = dat1$value_usd, k = 5)
# SVM models with 5 - folds CV 
# Tuning of the parameters kernel and type
# model 1
svm.pred1 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod1 <- svm(value_usd~., train, cost = 1, gamma = 0.1,type = "eps-regression", kernel = "radial")
  svm.pred1[folds[[i]]] <- predict(svm.mod1, test)
}
mae1 <- mean(abs(svm.pred1 - dat1$value_usd))
# 5.375121
# model 2 
svm2<-svm(value_usd~., train, cost = 10, gamma = 0.1,type = "eps-regression", kernel = "linear")
svm.pred2 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod2 <- svm2
  svm.pred2[folds[[i]]] <- predict(svm.mod2, test)
}
mae2 <- mean(abs(svm.pred2 - dat1$value_usd))
# 6.219601
# model 3
svm3<-svm(value_usd~., train, cost = 1, gamma = 0.1,type = "eps-regression", kernel = "sigmoid")
svm.pred3 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod3 <- svm3
  svm.pred3[folds[[i]]] <- predict(svm.mod3, test)
}
mae3 <- mean(abs(svm.pred3 - dat1$value_usd))
# 4.399212
# model 4
svm4<-svm(value_usd~., train, cost = 1, gamma = 0.1,type = "eps-regression", kernel = "polynomial")
svm.pred4 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod4 <- svm4
  svm.pred4[folds[[i]]] <- predict(svm.mod4, test)
}
mae4 <- mean(abs(svm.pred4 - dat1$value_usd))
# 4.399212
# model 5
svm5<-svm(value_usd~., train, cost = 1, gamma = 0.1,type = "nu-regression", kernel = "radial")
svm.pred5 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod5 <- svm5
  svm.pred5[folds[[i]]] <- predict(svm.mod5, test)
}
mae5 <- mean(abs(svm.pred5 - dat1$value_usd))
# 1.663775
# model 6
svm6<-svm(value_usd~., train, cost = 1, gamma = 0.1,type = "nu-regression", kernel = "linear")
svm.pred6 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod6 <- svm6
  svm.pred6[folds[[i]]] <- predict(svm.mod6, test)
}
mae6 <- mean(abs(svm.pred6 - dat1$value_usd))
# 3.501186
# model 7
svm7<-svm(value_usd~., train, cost = 1, gamma = 0.1,type = "nu-regression", kernel = "polynomial")
svm.pred7 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod7 <- svm7
  svm.pred7[folds[[i]]] <- predict(svm.mod7, test)
}
mae7 <- mean(abs(svm.pred7 - dat1$value_usd))
# 2.418181
# model 8
svm8<-svm(value_usd~., train, cost = 1, gamma = 0.1,type = "nu-regression", kernel = "sigmoid")
svm.pred8 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod8 <- svm8
  svm.pred8[folds[[i]]] <- predict(svm.mod8, test)
}
mae8 <- mean(abs(svm.pred8 - dat1$value_usd))
#33574.32

# Determine of gamma and cost

# model 9 
svm9<-svm(value_usd~., train, cost = 0.1, gamma = 0.001,type = "nu-regression", kernel = "radial")
svm.pred9 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod9 <- svm9
  svm.pred9[folds[[i]]] <- predict(svm.mod9, test)
}
mae9 <- mean(abs(svm.pred9 - dat1$value_usd))
# 4.51195
# model 10
svm10<-svm(value_usd~., train, cost = 0.1, gamma = 0.01,type = "nu-regression", kernel = "radial")
svm.pred10 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod10 <- svm10
  svm.pred10[folds[[i]]] <- predict(svm.mod10, test)
}
mae10 <- mean(abs(svm.pred10 - dat1$value_usd))
# 3.503295
# model 11
svm11<-svm(value_usd~., train, cost = 0.1, gamma = 0.0476,type = "nu-regression", kernel = "radial")
svm.pred11 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod11 <- svm11
  svm.pred11[folds[[i]]] <- predict(svm.mod11, test)
}
mae11 <- mean(abs(svm.pred11 - dat1$value_usd))
# 3.380208
# model 12
svm12<-svm(value_usd~., train, cost = 0.1, gamma = 0.1,type = "nu-regression", kernel = "radial")
svm.pred12 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod12 <- svm12
  svm.pred12[folds[[i]]] <- predict(svm.mod12, test)
}
mae12 <- mean(abs(svm.pred12 - dat1$value_usd))
# 4.217795
# model 13
svm13<-svm(value_usd~., train, cost = 1, gamma = 0.001,type = "nu-regression", kernel = "radial")
svm.pred13 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod13 <- svm13
  svm.pred13[folds[[i]]] <- predict(svm.mod13, test)
}
mae13 <- mean(abs(svm.pred13 - dat1$value_usd))
# 3.286068
# model 14
svm14<-svm(value_usd~., train, cost = 1, gamma = 0.01,type = "nu-regression", kernel = "radial")
svm.pred14 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod14 <- svm14
  svm.pred14[folds[[i]]] <- predict(svm.mod14, test)
}
mae14 <- mean(abs(svm.pred14 - dat1$value_usd))
# 0.8398657
# model 15
svm15<-svm(value_usd~., train, cost = 1, gamma = 0.0476,type = "nu-regression", kernel = "radial")
svm.pred15 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod15 <- svm15
  svm.pred15[folds[[i]]] <- predict(svm.mod15, test)
}
mae15 <- mean(abs(svm.pred15 - dat1$value_usd))
# 1.225685
# model 5 
# 1.663775
# model 16
svm16<-svm(value_usd~., train, cost = 10, gamma = 0.001,type = "nu-regression", kernel = "radial")
svm.pred16 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod16 <- svm16
  svm.pred16[folds[[i]]] <- predict(svm.mod16, test)
}
mae16 <- mean(abs(svm.pred16 - dat1$value_usd))
# 1.674995
# model 17
svm.pred17 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod17 <- svm(value_usd~., train, cost = 10, gamma = 0.01,type = "nu-regression", kernel = "radial")
  svm.pred17[folds[[i]]] <- predict(svm.mod17, test)
}
mae17 <- mean(abs(svm.pred17 - dat1$value_usd))
# 0.2360964
# model 18
svm18<-svm(value_usd~., train, cost = 10, gamma = 0.0476,type = "nu-regression", kernel = "radial")
svm.pred18 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod18 <- svm18
  svm.pred18[folds[[i]]] <- predict(svm.mod18, test)
}
mae18 <- mean(abs(svm.pred18 - dat1$value_usd))
# 0.2674571
# model 19
svm19<-svm(value_usd~., train, cost = 10, gamma = 0.1,type = "nu-regression", kernel = "radial")
svm.pred19 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  svm.mod19 <- svm19
  svm.pred19[folds[[i]]] <- predict(svm.mod19, test)
}
mae19 <- mean(abs(svm.pred19 - dat1$value_usd))
# 0.3903175

# Final svm model : svm 17 
summary(svm17)
RMSE.svm <- RMSE(svm.pred17, dat1$value_usd) # 13.64072
weight_rank <- as.data.frame(t(svm.mod17$coefs) %*% svm.mod17$SV)

# Visualization
library(ggplot2)
model_svm <- cbind(
  Truth = dat1$value_usd,
  Predicted = svm.pred17
)
model_svm <- as.data.frame(model_svm)
pdf('svm_model.pdf', width = 4.3, height = 4)
ggplot(model_svm,aes(Predicted, Truth)) + 
  geom_point(size=2,color="black",alpha=0.5) + 
  geom_smooth(method = "lm") + 
  theme_bw() +
  labs(title = paste("svm model")) +
  theme(plot.title = element_text(size=18, face="bold", hjust=0.5))
dev.off()

# RF model with 5-fold CV
set.seed(1)
rf1 <- train(value_usd ~.,data = train, method = 'ranger', importance = 'impurity')
rf1$results
rf.pred <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  rf.mod <- rf1
  rf.pred[folds[[i]]] <- predict(rf1, test)
}
mae_rf <- mean(abs(rf.pred - dat1$value_usd)) # 0.226377
RMSE.rf <- RMSE(rf.pred, dat1$value_usd)
# 5.775122

# Visualization
model_RF <- cbind(
  Truth = dat1$value_usd,
  Predicted = rf.pred
)
model_RF <- as.data.frame(model_RF)
pdf('RF_model.pdf', width = 4.3, height = 4)
ggplot(model_RF,aes(Predicted, Truth)) + 
  geom_point(size=2,color="black",alpha=0.5) + 
  geom_smooth(method = "lm", col = 'red') + 
  theme_bw() +
  labs(title = paste("RF model")) +
  theme(plot.title = element_text(size=18, face="bold", hjust=0.5))
dev.off()
x_axis <- seq(1, 53442, length=53442)
model_RF$Index = rownames(model_RF)
plot(Truth~Index, data=model_RF ,cex=0, ylab = 'Value')
lines(x_axis, model_RF$Truth, col='blue')
lines(x_axis, model_RF$Predicted, col='red')


# Neural Network model with 5 folds CV 
set.seed(1)
# size = 5
net.pred5 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod5 <- nnet(value_usd/max.usd~., train, size = 5, linout = TRUE, trace = FALSE, decay = 5e-4, maxit = 200)
  net.pred5[folds[[i]]] <- predict(net.mod5, test)*max.usd
}

mae_5 <- mean(abs(net.pred5 - dat1$value_usd))
# size = 6
net.pred6 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod6 <- nnet(value_usd/max.usd~., train, size = 6, linout = TRUE, trace = FALSE, decay = 5e-4, maxit = 200)
  net.pred6[folds[[i]]] <- predict(net.mod6, test)*max.usd
}

mae_6 <- mean(abs(net.pred6 - dat1$value_usd))
# size = 7
net.pred7 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod7 <- nnet(value_usd/max.usd~., train, size = 7, linout = TRUE, trace = FALSE, decay = 5e-4, maxit = 200)
  net.pred7[folds[[i]]] <- predict(net.mod7, test)*max.usd
}

mae_7 <- mean(abs(net.pred7 - dat1$value_usd))
# size = 8
net.pred8 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod8 <- nnet(value_usd/max.usd~., train, size = 8, linout = TRUE, trace = FALSE, decay = 5e-4, maxit = 200)
  net.pred8[folds[[i]]] <- predict(net.mod8, test)*max.usd
}

mae_8 <- mean(abs(net.pred8 - dat1$value_usd))
# size = 9
net.pred9 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod9 <- nnet(value_usd/max.usd~., train, size = 9, linout = TRUE, trace = FALSE, decay = 5e-4 , maxit = 200)
  net.pred9[folds[[i]]] <- predict(net.mod9, test)*max.usd
}

mae_9 <- mean(abs(net.pred9 - dat1$value_usd))
# size = 10
net.pred10 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod10 <- nnet(value_usd/max.usd~., train, size = 10, linout = TRUE, trace = FALSE, decay = 5e-4, maxit = 200)
  net.pred10[folds[[i]]] <- predict(net.mod10, test)*max.usd
}

mae_10 <- mean(abs(net.pred10 - dat1$value_usd))
# size = 11
net.pred11 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod11 <- nnet(value_usd/max.usd~., train, size = 11, linout = TRUE, trace = FALSE, decay= 5e-4, maxit = 200)
  net.pred11[folds[[i]]] <- predict(net.mod11, test)*max.usd
}

mae_11 <- mean(abs(net.pred11 - dat1$value_usd))
# size = 12
net.pred12 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod12 <- nnet(value_usd/max.usd~., train, size = 12, linout = TRUE, trace = FALSE,decay=5e-4,maxit = 200)
  net.pred12[folds[[i]]] <- predict(net.mod12, test)*max.usd
}

mae_12 <- mean(abs(net.pred12 - dat1$value_usd))
# size = 13
net.pred13 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod13 <- nnet(value_usd/max.usd~., train, size = 13, linout = TRUE, trace = FALSE,decay=5e-4,maxit = 200)
  net.pred13[folds[[i]]] <- predict(net.mod12, test)*max.usd
}

mae_13 <- mean(abs(net.pred13 - dat1$value_usd))

# size = 14
net.pred14 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod14 <- nnet(value_usd/max.usd~., train, size = 14, linout = TRUE, trace = FALSE, decay = 5e-4, maxit = 200)
  net.pred14[folds[[i]]] <- predict(net.mod14, test)*max.usd
}

mae_14 <- mean(abs(net.pred14 - dat1$value_usd))
# size = 15
net.pred15 <- NULL
for(i in 1:5){
  train <- dat1[-folds[[i]], ]
  test <- dat1[folds[[i]], ]
  max.usd <- max(train$value_usd)
  net.mod15 <- nnet(value_usd/max.usd~., train, size = 15, linout = TRUE, trace = FALSE, decay = 5e-4, maxit = 200)
  net.pred15[folds[[i]]] <- predict(net.mod15, test)*max.usd
}

mae_15 <- mean(abs(net.pred15 - dat1$value_usd))

# determine optimal size
size <- c('5','6','7','8','9','10','11','12','13','14','15')
MAE <- c(mae_5,mae_6,mae_7,mae_8,mae_9,mae_10,mae_11,mae_12,mae_13,mae_14,mae_15)
df1 <- data.frame(size = size, MAE = MAE)
ggplot(data = df1, aes(x = size, y = MAE, group = 1)) +
  geom_line(color = "black") +  
  geom_text(aes(label = sprintf("%.2f", MAE)), hjust = -0.2, vjust = 0.5) 

# final model: size = 9
RMSE.net <- RMSE(net.pred9, dat1$value_usd) # 23.27903
# Visualization
model_NET <- cbind(
  Truth = dat1$value_usd,
  Predicted = net.pred9
)
model_NET <- as.data.frame(model_NET)
pdf('Net_model.pdf', width = 4.3, height = 4)
ggplot(model_NET,aes(Predicted, Truth)) + 
  geom_point(size=2,color="black",alpha=0.5) + 
  geom_smooth(method = "lm") + 
  theme_bw() +
  labs(title = paste("Net model")) +
  theme(plot.title = element_text(size=18, face="bold", hjust=0.5))
dev.off()

library(NeuralNetTools)
plotnet(net.mod9)


# Comparision
# RMSE
RMSE.svm # 13.64072
RMSE.rf # 5.775122
RMSE.net # 23.27903




