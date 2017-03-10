
#read in data.
data1=read.csv("~/Dropbox/t1Data2.csv")
data1<-data1[,2:4]

#test data as a null, 60 random variates, all standard normal, split in 3 sets of 20
x<-matrix(rnorm(60), ncol=3) #5% cutoff approx 0.70

#each column (area) one at a time (5 sets of g=1)
apply(x,2,ad.test)
#all at once, does pooled s^2 and then combines areas
ad.test(x)

apply(data1,2,ad.test) # B and C are normal, A is not
ad.test(data1) #A not normal, result hard to interpret
ad.test(data1[,2:3]) #A excluded, B and C have clear heteroskedastic signature
