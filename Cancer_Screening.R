Can decision trees help predict cancer diagnostics
========================================================
Building CART, Random Forests, and Tree models

#Loading the necessary libraries:
library(caret)
library(rpart)
library(randomForest)
library(gbm)
library(rattle)
library(pROC)
library(plyr)
library(DMwR)
library(rpart.plot)
library(randomForest)
library(reprtree)
library(reshape)




#load dataset
wdbc <- read.csv("C:/Users/Colleen/Desktop/Ryerson/Side Projects/wdbc.csv", header=F)
cancer=wdbc #rename dS
dim(cancer) #look at dS' dimensions
names(cancer) #find column names
names(cancer)=c("ID", "diagnosis", "radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension", "radius_SE", "texture_SE", "perimeter_SE", "area_SE", "smoothness_SE", "compactness_SE", "concavity_SE", "concave_points_SE", "symmetry_SE", "fractal_dimension_SE", "radius_Worst", "texture_Worst", "perimeter_Worst", "area_Worst", "smoothness_Worst", "compactness_Worst", "concavity_Worst", "concave_points_Worst", "symmetry_Worst", "fractal_dimension_Worst")
cancer_clean=cancer[,2:32] #take out first column with patient's IDs - these are unnecessary in our downstream analysis
names(cancer_clean)


#Explore the dataset a little more:

table(cancer_clean$diagnosis)

cancer_clean_long=melt(cancer_clean, id="diagnosis")
head(cancer_clean_long)
ggplot(cancer_clean_long, aes(x=diagnosis, y=value))+facet_wrap(~variable)+geom_boxplot()+theme_classic()

ggplot(cancer_clean_long, aes(x=value))+facet_wrap(~variable)+geom_bar()+theme_classic()

#There are 357 benign ("B") samples and 212 malign ("M") samples in the dataset. 
#This is a large class imbalance, which impeeds the algoithm's ability to accurately predict the minority class (in this case, to predict a malignant sample). 
#SMOTE (Synthetic Minority Oversampling TEchnique) generates a random set of minority class observations, using bootstrapping and k-nearest neighbours. In this way, the bias towards the majority class is lessened. 


cancer_clean_smote=SMOTE(diagnosis~., data=cancer_clean, k=5, perc.over=110)
cancer_clean_smote$diagnosis=as.factor(cancer_clean_smote$diagnosis)
table(cancer_clean_smote$diagnosis)


#Let's compare the number of classes before and after SMOTE

levels(cancer_clean_smote$diagnosis)=c("Benign", "Malignant")
smote_graph=ggplot(cancer_clean_smote, aes(diagnosis, fill=diagnosis))+geom_bar()+ theme_classic()+scale_fill_manual(values=c("midnightblue", "firebrick4"))+guides(fill=F)+ylim(c(0,400))+ylab("Number of samples")+xlab("")+ggtitle("SMOTEd")
smote_graph

levels(cancer_clean$diagnosis)=c("Benign", "Malignant")
graph=ggplot(cancer_clean, aes(diagnosis, fill=diagnosis))+geom_bar()+theme_classic()+scale_fill_manual(values=c("midnightblue", "firebrick4"))+guides(fill=F)+ylim(c(0, 400))+ylab("Number of samples")+xlab("")+ggtitle("raw")
graph

library(gridExtra)
grid.arrange(graph, smote_graph, nrow=2)




#Cart

#First, look at CART with raw data

tc=trainControl("cv", 10)
rpart.grid=expand.grid(.cp=0.2)
Clean_rpart=(train.rpart=train(diagnosis~., data=cancer_clean, method="rpart", trControl=tc, tuneGrid=rpart.grid))
Clean_rpart$results
confusionMatrix(Clean_rpart)

#CART with SMOTEd data
SmoteClean_rpart=(train.rpart=train(diagnosis~., data=cancer_clean_smote, method="rpart", trControl=tc, tuneGrid=rpart.grid))

SmoteClean_rpart$results
confusionMatrix(SmoteClean_rpart)


#c4.5 Decision Trees

#raw data
set.seed(1)
Clean_c45=train(diagnosis~., data=cancer_clean, method="J48", trControl=tc)
Clean_c45$results
confusionMatrix(Clean_c45)

#SMOTEd data
set.seed(1)
Clean_c45_smoted=train(diagnosis~., data=cancer_clean_smote, method="J48", trControl=tc)
Clean_c45_smoted$results
confusionMatrix(Clean_c45_smoted)



#Visualize Tree:
Clean_c45_smoted$finalModel
