library(readxl)
cluster<- read_excel("cluster_data.xlsx")
summary(cluster)
cluster$`Party_ selected`[cluster$`Party_ selected`== 2] = 0
cluster[,c(1,6:8)] = NULL
cluster$county = as.factor(cluster$county)
cluster$state = as.factor(cluster$state)
cluster$`Party_ selected` = as.factor(cluster$`Party_ selected`)
cluster = cluster[complete.cases(cluster),]
library(cluster) # needed for clusplot
library(fpc)  # needed for plotcluster
hc <- kmeans(cluster[,c(7:10)],3) # 5 cluster solution
hc$cluster

clusplot(cluster[,c(7:10)], hc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main="KMeans Clusters")
summary(cluster[,c(7:10)])
cluster$group = hc$cluster
cluster$group[cluster$group == '1'] = 'MoreThanAverage'
cluster$group[cluster$group == '2'] = 'Average'
cluster$group[cluster$group == '3'] = 'LessThanAverage'

VoteRatio =as.data.frame(aggregate(cluster[,c(3,4)],by=list(hc$cluster),FUN=mean))
VoteRatio
Emp_Structure =as.data.frame(aggregate(cluster[,c(7:10)],by=list(hc$cluster),FUN=mean))
Emp_Structure
Unemployment =as.data.frame(aggregate(cluster$Unemployment,by=list(hc$cluster),FUN=mean))
Unemployment
IncomePerCap =as.data.frame(aggregate(cluster$IncomePerCap,by=list(hc$cluster),FUN=mean))
IncomePerCap
cluster$group = as.factor(cluster$group)
summary(cluster)
