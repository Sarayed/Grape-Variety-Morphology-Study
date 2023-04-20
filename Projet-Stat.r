#TACHE 1:
raisin <- read.csv('C:/Users/user/Downloads/Raisin.csv', sep = ",")
###TACHE 2:
#attach(raisin)
str(raisin)
#EXPLORATION + QUESTION1
summary(raisin)
boxplot(raisin$Area , data = raisin, col=c(rgb(0.4,0.4,0.4,0.4), rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5)))
boxplot(raisin$MajorAxisLength, data = raisin, col=c(rgb(0.4,0.4,0.4,0.4), rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5)))
boxplot(raisin$MinorAxisLength, data = raisin, col=c(rgb(0.4,0.4,0.4,0.4), rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5)))
boxplot(raisin$Eccentricity, data = raisin, col=c(rgb(0.4,0.4,0.4,0.4), rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5)))
boxplot(raisin$ConvexArea, data = raisin, col=c(rgb(0.4,0.4,0.4,0.4), rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5)))
boxplot(raisin$Extent, data = raisin, col=c(rgb(0.4,0.4,0.4,0.4), rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5)))
boxplot(raisin$Perimeter, data = raisin, col=c(rgb(0.4,0.4,0.4,0.4), rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.5)))
###IMPUTATION:
outlier_norm <- function(x)
  {
  qntile <- quantile(x, probs=c(.25, .75),na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qntile[1] - H)] <- NA
  x[x > (qntile[2] + H)] <- NA
  return(x)
}
raisin$Area=outlier_norm(raisin$Area)
raisin$MajorAxisLength=outlier_norm(raisin$MajorAxisLength)
raisin$MinorAxisLength=outlier_norm(raisin$MinorAxisLength)
raisin$Eccentricity=outlier_norm(raisin$Eccentricity)
raisin$ConvexArea=outlier_norm(raisin$ConvexArea)
raisin$Extent=outlier_norm(raisin$Extent)
raisin$Perimeter=outlier_norm(raisin$Perimeter)

#QUESTION2:
raisin$Class <- ifelse(test = (raisin$Class != "Besni")*(raisin$Class != "Kecimen"), yes = NA,no=raisin$Class)
raisin$Eccentricity <- ifelse(test = raisin$Eccentricity >0 , yes = raisin$Eccentricity,no=NA)
sum(is.na(raisin))/prod(dim(raisin)) # le taux des valeurs manquantes
raisin<-kNN(raisin) # imputation avec kNN
sum(is.na(raisin)) # On remarque qu'il y a 0 valeurs nulles.

#TACHE 3:
#1)
hist(raisin$Area)
hist(raisin$MajorAxisLength)
hist(raisin$MinorAxisLength)
hist(raisin$Eccentricity)
hist(raisin$Perimeter)
hist(raisin$ConvexArea)
hist(raisin$Extent)

shapiro.test(raisin$Area)
shapiro.test(raisin$MajorAxisLength)
shapiro.test(raisin$MinorAxisLength)
shapiro.test(raisin$Eccentricity)
shapiro.test(raisin$Perimeter)
shapiro.test(raisin$ConvexArea)
shapiro.test(raisin$Extent)
summary(raisin)

#2) modalité qualitative
ggplot(raisin, aes(x="", y=raisin$Class, fill=raisin$Class)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


#TACHE 4 (wilcoxon test):

#Croiser une variable qualitative et une variable quantitative
bxp <- ggboxplot(
  raisin, x = "Class", y = "Area", 
  ylab = "Area", xlab = "Class", add = "jitter"
)
bxp

stat.test <- raisin %>% 
  wilcox_test(Area ~ Class) %>%
  add_significance()
stat.test
raisin %>% wilcox_effsize(Area ~ Class)

bxp <- ggboxplot(
  raisin, x = "Class", y = "MajorAxisLength", 
  ylab = "MajorAxisLength", xlab = "Class", add = "jitter"
)
bxp

stat.test <- raisin %>% 
  wilcox_test(MajorAxisLength ~ Class) %>%
  add_significance()
stat.test
raisin %>% wilcox_effsize(MajorAxisLength ~ Class)

bxp <- ggboxplot(
  raisin, x = "Class", y = "Eccentricity", 
  ylab = "Eccentricity", xlab = "Class", add = "jitter"
)
bxp

stat.test <- raisin %>% 
  wilcox_test(Eccentricity ~ Class) %>%
  add_significance()
stat.test
raisin %>% wilcox_effsize(Eccentricity ~ Class)

stat.test <- raisin %>% 
  wilcox_test(MajorAxisLength ~ Class) %>%
  add_significance()
stat.test
raisin %>% wilcox_effsize(MajorAxisLength ~ Class)

bxp <- ggboxplot(
  raisin, x = "Class", y = "Perimeter", 
  ylab = "Perimeter", xlab = "Class", add = "jitter"
)
bxp

stat.test <- raisin %>% 
  wilcox_test(Perimeter ~ Class) %>%
  add_significance()
stat.test
raisin %>% wilcox_effsize(Perimeter ~ Class)



#wilcox.test(formula=raisin$Area ~ raisin$Class) 
#Croiser 2 variables quantitatives
ggplot(raisin, aes(x=raisin$Perimeter, y=raisin$Extent, color=raisin$Class)) + 
geom_point(size=2) 

ggplot(raisin, aes(x=raisin$Area, y=raisin$ConvexArea, color=raisin$Class)) + 
  geom_point(size=2) 

ggplot(raisin, aes(x=raisin$Area, y=raisin$MajorAxisLength, color=raisin$Class)) + 
  geom_point(size=2) 

ggplot(raisin, aes(x=raisin$ConvexArea, y=raisin$MajorAxisLength, color=raisin$Class)) + 
  geom_point(size=2) 

ggplot(raisin, aes(x=raisin$Perimeter, y=raisin$ConvexArea, color=raisin$Class)) + 
  geom_point(size=2) 

ggcorr(raisin, method = c("everything","spearman"),label=TRUE) 
##spearman yestaamelch donnée brute/ pearson yestaamel heka aleh lawlawiya lilo (lel parametrique)
##on peut verifier avec le test d'hypothese:
cor.test(raisin$Extent, raisin$Area, method = "spearman") 
##correlation faible ==> p_value > 0.05 ==>H0 verifié!
cor.test(raisin$MajorAxisLength, raisin$Area, method = "spearman")
##correlation forte ==> p_value < 0.05 (presque nulle) ==>H1 verifié!
cor.test(raisin$Area, raisin$Eccentricity, method = "spearman") 
##correlation forte ==> p_value < 0.05 (presque nulle) ==>H1 verifié!
cor.test(raisin$Extent, raisin$MajorAxisLength, method = "spearman") 
##correlation faible ==> p_value > 0.05 ==>H0 verifié!
cor.test(raisin$MinorAxisLength, raisin$Eccentricity, method = "spearman") 
##correlation faible ==> p_value > 0.05 ==>H0 verifié!
cor.test(raisin$Perimeter, raisin$ConvexArea, method = "spearman") 
##correlation forte ==> p_value < 0.05 (presque nulle) ==>H1 verifié!

#TACHE 5:
raisin$Class <- ifelse(test=(raisin$Class=="Besni"),yes=1,no=0)
#1)
raisin<-raisin[ -c(9:16) ]
ind = sample.split(Y = raisin$MinorAxisLength, SplitRatio = 0.7)
training = raisin[ind,]
testing = raisin[!ind,]

model<-lm(MinorAxisLength ~ Area+MajorAxisLength+Class+Eccentricity+Perimeter+ConvexArea+Extent, data=training)
summary(model)
#2)

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 7) # number of folds
# Features
x <- raisin %>%
  select(-MinorAxisLength) %>%
  as.data.frame()
# Target variable
y <- raisin$MinorAxisLength

# Training: 70%; Test: 30%
set.seed(2021)
inTrain <- createDataPartition(y, p = .70, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]
result_rfe1 <- rfe(x=x_train,y=y_train, 
                   sizes = c(1:6),
                   rfeControl = control)
result_rfe1

predictors(result_rfe1)
###on a éliminé Extent, Class et Perimeter avec le RFECV 
model<-lm(MinorAxisLength ~ Area+MajorAxisLength+Eccentricity+ConvexArea, data=training)
summary(model)
model<-lm(MinorAxisLength ~ Area+MajorAxisLength+Eccentricity, data=training)
summary(model)
model<-lm(MinorAxisLength ~ Area+Eccentricity, data=training)
summary(model) ##C est boooooooonn!!!!
#3)
#raisin<-raisin[ -c(5:7) ]
res.pca <- PCA(raisin, graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_cos2(res.pca, choice = "var", axes = 1:2) ##(d'aprés le critere de kaiser valeur propre>1)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
#Cercle de Correlation
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
###
raisin$Class <- ifelse(test=(raisin$Class==1),yes="Besni",no="Kecimen")
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = raisin$Class, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
raisin$Class <- ifelse(test=(raisin$Class=="Besni"),yes=1,no=0)


### LINEAR MODEL WITH PCA
set.seed(1)
ctrl <- trainControl(
  method = "cv",
  number = 30,
)
set.seed(1)
modelpca <- train(
  MinorAxisLength ~ Area+MajorAxisLength+Eccentricity+ConvexArea,
  data = training,
  method = 'lm',
  preProcess = c("scale", "pca"),
  trControl = ctrl
)
modelpca

test.features = subset(testing, select=-c(MinorAxisLength))
test.target = subset(testing, select=MinorAxisLength)[,1]

predictions = predict(modelpca, newdata = test.features)

# RMSE (WITH PCA)
sqrt(mean((test.target - predictions)^2))
#R2    (WITH PCA)
cor(test.target, predictions) ^ 2

#####################################
predictionsLM = predict(model, newdata = test.features)
# RMSE (WITHOUT PCA)
sqrt(mean((test.target - predictionsLM)^2))
#R2   (WITHOUT PCA)
cor(test.target, predictionsLM) ^ 2

############TACHE 666666
set.seed(1234)
custom <- trainControl(method = "repeatedcv",number = 10,repeats = 5,verboseIter = T)
en <- train(MinorAxisLength ~ Area+MajorAxisLength+Eccentricity+ConvexArea,training,
            method='glmnet',
            tuneGrid=expand.grid(alpha=seq(0,1,length=10),
                                 lambda=seq(0.0001,1,length=5)),trControl=custom)
predictions_en <- predict(en, newdata = test.features)
cor(fitted(en), training$MinorAxisLength)^2
cor(predictions_en, testing$MinorAxisLength)^2

gam = gam(MinorAxisLength ~ Area+MajorAxisLength+Eccentricity+ConvexArea
          ,family = "Gamma", data = training)
predictions_gam <- predict(gam, newdata = test.features)
cor(predictions_gam, testing$MinorAxisLength)^2

glm <- glm(MinorAxisLength ~ Area+MajorAxisLength+Eccentricity+ConvexArea
           ,family = "Gamma", data = training)
predictions_sbp <- predict(glm, newdata = test.features)
cor(predictions_sbp, testing$MinorAxisLength)^2


                           