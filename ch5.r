# example 5.2 of section 5.2.1 
# (example 5.2 of section 5.2.1)  : Choosing and evaluating models : Evaluating models : Evaluating classification models 
# Title: Spam classifications 

sample <- spamTest[c(7,35,224,327),c('spam','pred')]
print(sample)
##          spam         pred
## 115      spam 0.9903246227
## 361      spam 0.4800498077
## 2300 non-spam 0.0006846551
## 3428 non-spam 0.0001434345

# example 5.3 of section 5.2.1 
# (example 5.3 of section 5.2.1)  : Choosing and evaluating models : Evaluating models : Evaluating classification models 
# Title: Spam confusion matrix 

cM <- table(truth=spamTest$spam,prediction=spamTest$pred>0.5)
print(cM)
##          prediction
## truth      FALSE TRUE
##   non-spam   264   14
##   spam        22  158

# example 5.4 of section 5.2.1 
# (example 5.4 of section 5.2.1)  : Choosing and evaluating models : Evaluating models : Evaluating classification models 
# Title: Entering data by hand 

t <- as.table(matrix(data=c(288-1,17,1,13882-17),nrow=2,ncol=2))
rownames(t) <- rownames(cM)
colnames(t) <- colnames(cM)
print(t)
##          FALSE  TRUE
## non-spam   287     1
## spam        17 13865

# example 5.5 of section 5.2.2 
# (example 5.5 of section 5.2.2)  : Choosing and evaluating models : Evaluating models : Evaluating scoring models 
# Title: Plotting residuals 

d <- data.frame(y=(1:10)^2,x=1:10)
model <- lm(y~x,data=d)
d$prediction <- predict(model,newdata=d)
library('ggplot2')
ggplot(data=d) + geom_point(aes(x=x,y=y)) +
    geom_line(aes(x=x,y=prediction),color='blue') +
    geom_segment(aes(x=x,y=prediction,yend=y,xend=x)) +
    scale_y_continuous('')

# example 5.6 of section 5.2.3 
# (example 5.6 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Making a double density plot 

ggplot(data=spamTest) +
   geom_density(aes(x=pred,color=spam,linetype=spam))

# example 5.7 of section 5.2.3 
# (example 5.7 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Plotting the receiver operating characteristic curve 

library('ROCR')
eval <- prediction(spamTest$pred,spamTest$spam)
plot(performance(eval,"tpr","fpr"))
print(attributes(performance(eval,'auc'))$y.values[[1]])
## [1] 0.9660072

# example 5.8 of section 5.2.3 
# (example 5.8 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Calculating log likelihood 

sum(ifelse(spamTest$spam=='spam',
   log(spamTest$pred),
   log(1-spamTest$pred)))
## [1] -134.9478
sum(ifelse(spamTest$spam=='spam',
   log(spamTest$pred),
   log(1-spamTest$pred)))/dim(spamTest)[[1]]
## [1] -0.2946458

# example 5.9 of section 5.2.3 
# (example 5.9 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Computing the null model’s log likelihood 

pNull <- sum(ifelse(spamTest$spam=='spam',1,0))/dim(spamTest)[[1]]
sum(ifelse(spamTest$spam=='spam',1,0))*log(pNull) +
   sum(ifelse(spamTest$spam=='spam',0,1))*log(1-pNull)
## [1] -306.8952

# example 5.10 of section 5.2.3 
# (example 5.10 of section 5.2.3)  : Choosing and evaluating models : Evaluating models : Evaluating probability models 
# Title: Calculating entropy and conditional entropy 

entropy <- function(x) { 	# Note: 1 
    xpos <- x[x>0]
    scaled <- xpos/sum(xpos)
    sum(-scaled*log(scaled,2))
  }

print(entropy(table(spamTest$spam))) 	# Note: 2 
## [1] 0.9667165

conditionalEntropy <- function(t) { 	# Note: 3 
    (sum(t[,1])*entropy(t[,1]) + sum(t[,2])*entropy(t[,2]))/sum(t)
  }

print(conditionalEntropy(cM)) 	# Note: 4 
## [1] 0.3971897

# Note 1: 
#   Define function that computes the entropy 
#   from list of outcome counts 

# Note 2: 
#   Calculate entropy of spam/non-spam 
#   distribution 

# Note 3: 
#   Function to calculate conditional or 
#   remaining entropy of spam distribution (rows) 
#   given prediction (columns) 

# Note 4: 
#   Calculate conditional or remaining entropy 
#   of spam distribution given prediction 

# example 5.11 of section 5.2.5 
# (example 5.11 of section 5.2.5)  : Choosing and evaluating models : Evaluating models : Evaluating clustering models 
# Title: Clustering random data in the plane 

set.seed(32297)
d <- data.frame(x=runif(100),y=runif(100))
clus <- kmeans(d,centers=5)
d$cluster <- clus$cluster

# example 5.12 of section 5.2.5 
# (example 5.12 of section 5.2.5)  : Choosing and evaluating models : Evaluating models : Evaluating clustering models 
# Title: Plotting our clusters 

library('ggplot2'); library('grDevices')
h <- do.call(rbind,
   lapply(unique(clus$cluster),
      function(c) { f <- subset(d,cluster==c); f[chull(f),]}))
ggplot() +
 geom_text(data=d,aes(label=cluster,x=x,y=y,
   color=cluster),size=3)  +
 geom_polygon(data=h,aes(x=x,y=y,group=cluster,fill=as.factor(cluster)),
   alpha=0.4,linetype=0) +
 theme(legend.position = "none")

# example 5.13 of section 5.2.5 
# (example 5.13 of section 5.2.5)  : Choosing and evaluating models : Evaluating models : Evaluating clustering models 
# Title: Calculating the size of each cluster 

table(d$cluster)

##  1  2  3  4  5
## 10 27 18 17 28

# example 5.14 of section 5.2.5 
# (example 5.14 of section 5.2.5)  : Choosing and evaluating models : Evaluating models : Evaluating clustering models 
# Title: Calculating the typical distance between items in every pair of clusters 

library('reshape2')
n <- dim(d)[[1]]
pairs <- data.frame(
   ca = as.vector(outer(1:n,1:n,function(a,b) d[a,'cluster'])),
   cb = as.vector(outer(1:n,1:n,function(a,b) d[b,'cluster'])),
   dist = as.vector(outer(1:n,1:n,function(a,b)
           sqrt((d[a,'x']-d[b,'x'])^2 + (d[a,'y']-d[b,'y'])^2)))
   )
dcast(pairs,ca~cb,value.var='dist',mean)
##   ca         1         2         3         4         5
## 1  1 0.1478480 0.6524103 0.3780785 0.4404508 0.7544134
## 2  2 0.6524103 0.2794181 0.5551967 0.4990632 0.5165320
## 3  3 0.3780785 0.5551967 0.2031272 0.6122986 0.4656730
## 4  4 0.4404508 0.4990632 0.6122986 0.2048268 0.8365336
## 5  5 0.7544134 0.5165320 0.4656730 0.8365336 0.2221314

