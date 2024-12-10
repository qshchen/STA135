library(MASS)
library(psych)
library(klaR)
library(mlbench)
library(biotools)
library(Rtsne)
library(ggplot2)
library(reshape2)

data(Glass)

#Descriptive analysis
summary(Glass)
pairs.panels(Glass[1:9],
             gap = 0,
             bg = c("red", "green", "blue", "yellow", "grey", "black")[Glass$Type],
             pch = 21)#pairwise plot
melted_Glass <- melt(Glass, id.vars = "Type")
# 绘制不同成分在不同玻璃类型中的含量分布（箱线图）
ggplot(melted_Glass, aes(x = Type, y = value, fill = Type)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Distribution of Different Components in Various Glass Types",
       x = "Glass Type", y = "Component Content") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Box-M test
boxM_test=boxM(Glass[,-10],Glass[,10])
print(boxM_test)#QDA is needed

#Split the data
set.seed(4)
ind <- sample(2, nrow(Glass),
              replace = TRUE,
              prob = c(0.7, 0.3))

#PCA+QDA
pca_result = prcomp(Glass[,-10], center = TRUE, scale.=FALSE)#PCA
summary(pca_result)
pca_data = as.data.frame(pca_result$x)
pca_data=cbind(pca_data,Glass[,10])
names(pca_data)=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","Type")
pca_data=data.frame(pca_data[,1],pca_data[,2],pca_data[,3],pca_data[,10])
names(pca_data)=c("PC1","PC2","PC3","Type")

training1 <- pca_data[ind==1,]
testing1 <- pca_data[ind==2,]


model1=qda(Type~.,data=training1)
model1

p1_tr <- predict(model1, training1)$class
tab1_tr <- table(Predicted = p1_tr, Actual = training1$Type)
tab1_tr

sum(diag(tab1_tr))/sum(tab1_tr)#accurary of training

p1_te <- predict(model1, testing1)$class
tab1_te <- table(Predicted = p1_te, Actual = testing1$Type)
tab1_te

sum(diag(tab1_te))/sum(tab1_te)#accurary of training
partimat(Type~., data = training1, method = "qda")


#Decision boundary function Type 1 vs Type 2
means1=model1$means
prior1=model1$prior
solve(model1$scaling[,,2])
a1=(solve(model1$scaling[,,2])-solve(model1$scaling[,,1]))/2
b1=(solve(model1$scaling[,,1]) %*% means1[1,] - solve(model1$scaling[,,2]) %*% means1[2,])
c1=log(prior1[1] / prior1[2])+(means1[2,]%*%solve(model1$scaling[,,2])%*%means1[2,]-means1[1,]%*%solve(model1$scaling[,,1])%*%means1[1,])/2+(1/2)*log(det(model1$scaling[,,2])/det(model1$scaling[,,1]))
a1
b1
c1


#t-SNE+QDA
tsne_out=Rtsne(Glass[,-10],perplexity=30,check_duplicates=FALSE)
tsne_data=data.frame(tsne_out$Y,Glass[,"Type"])
names(tsne_data)=c("X1","X2","Type")

ggplot(tsne_data,aes(X1,X2))+
  geom_point(aes(X1,X2,fill = Type),shape = 21,color = "black")+
  stat_ellipse(aes(color = Type,fill = Type),
               geom = "polygon",
               alpha = 0.3,
               linetype = 2)+
  theme_classic()+
  theme(legend.position = "top")
#Type 1, 2 and 3->new 1; Type 5, 6 and 7->new 2
tsne_data1=tsne_data
tsne_data1$Type[tsne_data1$Type=="2"]="1"
tsne_data1$Type[tsne_data1$Type=="3"]="1"
tsne_data1$Type[tsne_data1$Type=="5"]="2"
tsne_data1$Type[tsne_data1$Type=="6"]="2"
tsne_data1$Type[tsne_data1$Type=="7"]="2"
tsne_data1$Type=factor(tsne_data1$Type)

boxM_test2=boxM(tsne_data1[,-3],tsne_data1[,3])
print(boxM_test2)

training2 <- tsne_data1[ind==1,]
testing2 <- tsne_data1[ind==2,]

model2 <-qda(Type~., training2)
model2

p2_tr <- predict(model2, training2)$class
tab2_tr <- table(Predicted = p2_tr, Actual = training2$Type)
tab2_tr

sum(diag(tab2_tr))/sum(tab2_tr)

p2_te <- predict(model2, testing2)$class
tab2_te <- table(Predicted = p2_te, Actual = testing2$Type)
tab2_te

sum(diag(tab2_te))/sum(tab2_te)

partimat(Type~., data = training2, method = "qda")

#Decision Boundary.new 1 vs new 2
means2=model2$means
prior2=model2$prior
a2=(solve(model2$scaling[,,2])-solve(model2$scaling[,,1]))/2
b2=(solve(model2$scaling[,,1]) %*% means2[1,] - solve(model2$scaling[,,2]) %*% means2[2,])
c2=log(prior2[1] / prior2[2])+(means2[2,]%*%solve(model2$scaling[,,2])%*%means2[2,]-means2[1,]%*%solve(model2$scaling[,,1])%*%means2[1,])/2+(1/2)*log(det(model2$scaling[,,2])/det(model2$scaling[,,1]))
a2
b2
c2

