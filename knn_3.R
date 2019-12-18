bank<-read.csv("loan.csv")
#x<- readline(prompt="Enter Age: ")
#x <- as.integer(x)
#y <- readline(prompt="Enter Loan Amount: ")
#y <- as.integer(y)
x=100
y=2000

dis <- transform(bank, distance= sqrt((x-bank$age)^2+(y-bank$loan)^2 ) )
dis

odis<- dis[order(dis$distance),]
odis

#k<- readline(prompt="Enter k: ")
# convert character into integer

#k <- as.integer(k)
k<-3
nn<-head(odis,k)
knn<-table(nn$default)
nn
knn
t<-names(which(table(nn$default)==max(table(nn$default))))  # finding which class has got max occurences
cat("class:",t[[1]][1])
