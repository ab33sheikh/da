stud<-read.csv("student_grade.csv")

x=21
y=23

dis <- transform(stud, distance= sqrt((x-stud$marks1)^2+(y-stud$marks2)^2 ) )
dis

odis<- dis[order(dis$distance),]
odis

#k<- readline(prompt="Enter k: ")
# convert character into integer

#k <- as.integer(k)
k<-3
nn<-head(odis,k)
knn<-table(nn$grade)
nn
knn
t<-names(which(table(nn$grade)==max(table(nn$grade))))  # finding which class has got max occurences
cat("class:",t[[1]][1])
