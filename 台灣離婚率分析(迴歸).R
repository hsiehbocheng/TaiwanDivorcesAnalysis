library(plyr)
library(tidyverse)
library(effects)
library(alr4)
getwd()
setwd("D:/ALR/data")
###################資料整理################################
div109 <- read.csv("109年行政區離婚統計_鄉鎮市區.csv")
#取離婚對數 設為應變數y
name <- div109[1,]
name = as.factor(name)
colnames(div109) = name
div109 <- div109[-1,c(-1,-6,-7,-8)]

h_ct <- read.csv("109年行政區家戶數統計_鄉鎮市區.csv")
#取總戶數&共同生活戶數
name <- h_ct[1,]
name = as.factor(name)
colnames(h_ct) = name
h_ct <- h_ct[-1,c(-1,-9,-7,-8)]

b_ct <- read.csv("109年行政區不動產實價登錄建物成交資訊(交易日)—按建物型態分_鄉鎮市區.csv")
#取整體中位數房價(不含車位)
b_ct <- b_ct[,c(2,3,4,31,70)]
b_ct[1,4] <- "不含車位中位數房價"
b_ct[1,5] <- "含車位中位數房價"
name <- b_ct[1,]
name = as.factor(name)
colnames(b_ct) = name
b_ct <- b_ct[-1,]
b_ct <- b_ct[,-5]

GD <- read.csv("109年行政區人口消長指標統計_鄉鎮市區.csv")
#取出生死亡率、自然&社會增加率和粗結婚率
name <- GD[1,]
name = as.factor(name)
colnames(GD) = name
GD <- GD[-1,c(-1,-5,-6,-7,-8,-10,-11,-12)]

GD108 <- read.csv("108年行政區人口消長指標統計_鄉鎮市區.csv")
#取出生死亡率、自然&社會增加率和粗結婚率
name <- GD108[1,]
name = as.factor(name)
colnames(GD108) = name
GD108 <- GD108[-1,c(-1,-11,-12)]

GD107 <- read.csv("107年行政區人口消長指標統計_鄉鎮市區.csv")
#取出生死亡率、自然&社會增加率和粗結婚率
GD107[1,c(5:10)] <- c("107粗出生率","107粗死亡率","107自然增加率","107社會增加率","107粗結婚率","107粗離婚率")
name <- GD107[1,]
name = as.factor(name)
colnames(GD107) = name
GD107 <- GD107[-1,c(-1,-11,-12)]

population <- read.csv("109年6月行政區人口指標_鄉鎮市區.csv")
#取扶養比、扶幼比和扶老比
name <- population[1,]
name = as.factor(name)
colnames(population) = name
population <- population[-1,c(2:5,8,11)]

educaiton <- read.csv("109年行政區15歲以上人口教育程度統計_鄉鎮市區.csv")
#取大學院校人數
name <- educaiton[1,]
name = as.factor(name)
colnames(educaiton) = name
educaiton <- educaiton[-1,c(2:4,7)]
Divdata <- Reduce(function(x,y) join(x,y ,by = c("縣市名稱",
                                                 "鄉鎮市區代碼",
                                                 "鄉鎮市區名稱"),type = "inner"),
                  list(div109,GD,GD107,GD108,population,educaiton,b_ct))
area <- paste(Divdata[,1],Divdata[,3])
Divdata <- Divdata[,c(-1,-2,-3)]
row.names(Divdata) <- area
i <- 1
for (i in 1:19){
  Divdata[,i] <- as.numeric(Divdata[,i])
}
str(Divdata)
Divdata <- Divdata[,c(-3,-4,-9,-10)]
write.csv(Divdata,file = "D:/ALR/data/DivData.csv",row.names = T)

Divdata <- read.csv("Divdata.csv")
rownames <- Divdata[,1]
row.names(Divdata) <- rownames
Divdata <- Divdata[,-1]
plot(Divdata)
##############################################################
#mice補值
length(which(is.na(Divdata$不含車位中位數房價) == TRUE))
narow <- rownames(Divdata[c(156 ,157 ,170 ,182, 183,
          188 ,211 ,278, 305, 306, 307 ,
          309 ,323 ,324 ,325 ,327 ,339),])
library(mice)
require(mice)
mice <- data.frame(md.pattern(Divdata, rotate.names = TRUE))
mice.data <- mice(Divdata,
                  m=3,
                  maxit = 50,
                  methods = "cart",
                  seed = 188)
Divdata_mice <- complete(mice.data , 1)
#遺失值直接刪除
Divdata <- na.omit(Divdata)
summary(Divdata)
###########################################################################################

##畫圖
boxplot(Divdata$離婚對數,
        main = "離婚對數")
plot(Divdata$離婚對數~Divdata$大學院校人口數,
     main = "離婚對數 verse 大學院校人口數 ")
hist(Divdata$離婚對數,
     main = "log(離婚對數)")
plot(Divdata$離婚對數~ Divdata$X108粗結婚率,
     main = "當年度離婚對數vs108年粗結婚率")
plot(Divdata$離婚對數~ Divdata$X108粗離婚率,
     main = "當年度離婚對數vs108年粗離婚率")
which(Divdata$X108粗離婚率 >5)
Divdata[322,]
plot(Divdata$X107自然增加率,Divdata$老化指數,
     main = "107年自然增加率vs老化指數")
###########################################
##共線性診斷
lm <- lm(Divdata$離婚對數~. ,data = Divdata)
summary(lm)
plot(lm)
pairs <- pairs(Divdata)
plot(predict(lm), residuals(lm))
par(mfrow = c(2,2))
residualPlots(lm)
library(car)
car::vif(lm)
lm <- lm(Divdata$離婚對數 ~. - X107自然增加率 - X108自然增加率,data = Divdata)
car::vif(lm)
##########################################
#變數選取
#向前選取
full <- formula(lm)
m0 <- lm(Divdata$離婚對數 ~ 1, Divdata) # the base model
m.forward <- step(m0, scope=full, direction="forward")
#向後選取
m1 <- update(m0, full) #full model
m.backward <- step(m1, direction="backward")
#stepwise
m.stepwise <- step(m1, direction="both")
lm2 <- lm(Divdata$離婚對數 ~ X107粗結婚率 + 
            X107粗離婚率 + X108社會增加率 + 
            X108粗離婚率 + 性比例 + 扶養比 + 
            大學院校人口數 + 不含車位中位數房價,data = Divdata)

summary(lm2)
plot(lm2)
residualPlots(lm2)
#############
lmm <-lm(log(Divdata$離婚對數) ~ X107粗結婚率 + 
           X107粗離婚率 + X108社會增加率 + 
           X108粗離婚率 + 性比例 + 扶養比 + 
           log(大學院校人口數) + 不含車位中位數房價,data = Divdata)
car::vif(lmm)
summary(lmm)
plot(lmm)
residualPlots(lmm) 
ks.test(scale(lmm$residuals),pnorm)
ncvTest(lmm)
#############
#####powertransorm########
library(MASS)
boxcox <- boxcox(lm)
lambda <- boxcox$x[which.max(boxcox$y)]

y <- Divdata[1]
x <- Divdata[c(5,6,8,10,11,12,14,15)]
powerTransform(x,family = "yjPower")
lmlog <- lm(log(Divdata$離婚對數) ~ X107粗結婚率 + 
              log(X107粗離婚率) + X108社會增加率 + 
              log(X108粗離婚率) + log(性比例) + I(1/(扶養比)) + 
              log(大學院校人口數) + log(不含車位中位數房價),data = Divdata)
residualPlots(lmlog)
plot(lmlog)
ks.test(scale(lmlog$residuals),pnorm)
summary(lmlog)
ncvTest(lmlog)

lmm$residuals
#WLS######
lev <- lm.influence(lmm)$hat
hc0wt <- 1/lmm$residuals^2
ss <- hccm(lmm,typr = "hc3")
hc3wt <- 1/lmm$residuals^2/(1-lev)^2
lmm_wt <- lm(log(Divdata$離婚對數) ~ X107粗結婚率 + 
               X107粗離婚率 + X108社會增加率 + 
               X108粗離婚率 + 性比例 + 扶養比 + 
               log(大學院校人口數) + 不含車位中位數房價,data = Divdata,
             weights = ss)
ncvTest(lmm_wt)
ks.test(scale(lmm_wt$residuals),pnorm)
residualPlots(lmm_wt)
car::vif(lmm_wt)
plot(lmm_wt)

summary(lmm_wt)
car::vif(lmm_wt)  
lm <- lm(log(Divdata$離婚對數) ~ X107粗結婚率 + 
           X107粗離婚率  + 
           X108粗離婚率  + 扶養比 + 
           log(大學院校人口數) + 不含車位中位數房價,data = Divdata)
levn <- lm.influence(lm)$hat
hc3wtn <- 1/lm$residuals^2/(1-levn)^2
lmnew_wt <- lm(log(Divdata$離婚對數) ~ X107粗結婚率 + 
                 X107粗離婚率 + 
                 X108粗離婚率  + 扶養比 + 
                 log(大學院校人口數) + 不含車位中位數房價,data = Divdata,
               weights = hc3wtn)
par(mfrow=c(2,2))
plot(lmnew_wt)
summary(lmnew_wt)
ncvTest(lmnew_wt)
ks.test(scale(lmnew_wt$residuals),pnorm)
car::vif(lmnew_wt)

