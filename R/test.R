
#read in data.
data2=read.csv("~/Downloads/S1Data2.csv")
data1=read.csv("~/Downloads/S1Data1.csv")

data2<-data2[,2:4]
data1<-data1[,2:5]

ad.test(data1, pooled = F)
ad.test(data1, pooled = T)

ad.test(data2, pooled = F)
ad.test(data2, pooled = T)


#test data as a null, 60 random variates, all standard normal, split in 3 sets of 20
x<-matrix(rnorm(180), ncol=3) #5% cutoff approx 0.70
ad.test(x, pooled = F)
ad.test(x, pooled = T)
