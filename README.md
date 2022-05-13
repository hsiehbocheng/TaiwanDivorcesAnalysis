# 台灣離婚對數分析
### **Data Desciption**:
以年度離婚對數作為應變數，解釋變數則以當年度之國民教育程度、房價，以
及前一年之人口消長指標等直覺認為與今年度離婚數有關之變數做挑選；相較
於１１０年，該平台於１０９年之年度行駐區資料較為完整，故選用１０９年
我行政區離婚對數作為應變數。\
空間範圍：全國\
空間統計單元：鄉鎮市區\
樣本數：358\
缺失值：17

```r
> summary(Divdata)
      X                離婚對數          粗結婚率     X107自然增加率   
 Length:358         Min.   :   4.00   Min.   :1.080   Min.   :-16.300  
 Class :character   1st Qu.:  29.25   1st Qu.:4.133   1st Qu.: -5.905  
 Mode  :character   Median :  62.50   Median :4.795   Median : -1.960  
                    Mean   : 142.68   Mean   :4.771   Mean   : -2.599  
                    3rd Qu.: 185.50   3rd Qu.:5.338   3rd Qu.:  1.030  
                    Max.   :1148.00   Max.   :9.690   Max.   :  7.090  
                                                                       
 X107社會增加率      X107粗結婚率    X107粗離婚率   X108自然增加率    
 Min.   :-18.4700   Min.   :2.250   Min.   :1.000   Min.   :-18.4100  
 1st Qu.: -5.1075   1st Qu.:4.625   1st Qu.:1.920   1st Qu.: -5.6950  
 Median : -1.5300   Median :5.340   Median :2.225   Median : -2.2850  
 Mean   :  0.7171   Mean   :5.288   Mean   :2.268   Mean   : -2.6804  
 3rd Qu.:  4.0550   3rd Qu.:5.975   3rd Qu.:2.550   3rd Qu.:  0.6825  
 Max.   : 90.9400   Max.   :9.520   Max.   :4.720   Max.   : 11.0500  
                                                                      
 X108社會增加率     X108粗結婚率    X108粗離婚率      性比例           扶養比     
 Min.   :-49.910   Min.   :2.100   Min.   :0.79   Min.   : 86.43   Min.   :28.46  
 1st Qu.: -8.678   1st Qu.:4.720   1st Qu.:1.94   1st Qu.: 99.79   1st Qu.:36.69  
 Median : -3.870   Median :5.305   Median :2.24   Median :105.08   Median :39.83  
 Mean   : -3.503   Mean   :5.277   Mean   :2.28   Mean   :105.53   Mean   :40.47  
 3rd Qu.:  0.875   3rd Qu.:5.780   3rd Qu.:2.53   3rd Qu.:111.54   3rd Qu.:44.09  
 Max.   : 43.730   Max.   :8.520   Max.   :5.77   Max.   :128.97   Max.   :58.26  
                                                                                  
    老化指數      大學院校人口數   不含車位中位數房價
 Min.   : 35.56   Min.   :   345   Min.   :  224811  
 1st Qu.:108.48   1st Qu.:  2450   1st Qu.: 3660000  
 Median :161.97   Median :  6196   Median : 5750000  
 Mean   :179.86   Mean   : 16282   Mean   : 6348747  
 3rd Qu.:232.37   3rd Qu.: 19666   3rd Qu.: 7600000  
 Max.   :595.00   Max.   :142771   Max.   :84000000  
                                   NA's   :17    
```


## **Abstract**
* 遺失值處理\
**mice**
```r
library(mice)
require(mice)
mice <- data.frame(md.pattern(Divdata, rotate.names = TRUE))
```
<img src="png/mice.png" alt="Cover" width="70%"/>

* 共線性診斷\
**vif**
```r
library(car)
car::vif(lm)
```
```r
> car::vif(lm)
          粗結婚率     X107社會增加率       X107粗結婚率       X107粗離婚率 
          2.938035           1.313099           2.809047           1.767713 
    X108社會增加率       X108粗結婚率       X108粗離婚率             性比例 
          1.369629           2.521400           1.696573           3.046758 
            扶養比           老化指數     大學院校人口數 不含車位中位數房價 
          1.879079           3.431385           1.940453           1.169135 
```
* 挑選變數(向前、後、逐步選取法)
```r
#向前選取
full <- formula(lm)
m0 <- lm(Divdata$離婚對數 ~ 1, Divdata) # the base model
m.forward <- step(m0, scope=full, direction="forward")
#向後選取
m1 <- update(m0, full) #full model
m.backward <- step(m1, direction="backward")
#stepwise
m.stepwise <- step(m1, direction="both")
```
* 殘差分析\
**Box Cox powertransform**
```r
library(MASS)
boxcox <- boxcox(lm)
lambda <- boxcox$x[which.max(boxcox$y)]
```
<img src="png/boxcox.png" alt="Cover" width="50%"/>

```r
plot(lmm)
residualPlots(lmm) 
```
<img src="png/plot.png" alt="Cover" width="70%"/>
<img src="png/resss.png" alt="Cover" width="70%"/>

\
**檢定**

```r
> ks.test(scale(lmlog$residuals),pnorm)

	One-sample Kolmogorov-Smirnov test

data:  scale(lmlog$residuals)
D = 0.06455, p-value = 0.1166
alternative hypothesis: two-sided
```















