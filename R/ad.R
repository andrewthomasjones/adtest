#d^2 ij
d2ij<-function(y,s2){
  ((y-mean(y))^2)/s2
}

wstarij <- function(y,s2,nu){
  n=length(y)
  return((nu*n*d2ij(y,s2)/((nu+1)*(n-1)-n*d2ij(y,s2))))
}


#function to calculate aj
aij<-function(y,s2,nu){
  return(pf(wstarij(y,s2,nu),1,nu,lower.tail=F))
}


AD_main <- function(y) {

  ni<-dim(y)[1]
  g<-dim(y)[2]

  a<-array(0,dim(y))
  d<-array(0,dim(y))
  w<-array(0,dim(y))
  a_o<-array(0,dim(y))

  AD<-array(0,g)
  AD_1<-array(0,g)
  AD_2<-array(0,g)


  n<-ni*g #assume equal size groups

  nu=n-g-1

  #pooled variance
  s2=sum((ni-1)*apply(y,2,var)/(n-g))

  for(i in 1:g){
    d[,i]<-d2ij(y[,i],s2)
    w[,i]<-wstarij(y[,i],s2,nu)
    a[,i]<-aij(y[,i],s2,nu)
    a_o[,i]<-a[order(a[,i]),i]

    AD[i]<-(-ni)

    for(j in 1:ni){
      AD_1[i]<- AD_1[i] + -(3/ni)^(1/2)*(2*a_o[j,i]-1)
      AD_2[i]<- AD_2[i] + -(5/ni)^(1/2)*(1/2)*(3*(2*a_o[j,i]-1)^2-1)
      AD[i]<- AD[i] -(1/ni)*(2*j-1)*(log(a_o[j,i])+log(1-a_o[(ni-j+1),i]))
    }

  }

  return(list(AD=AD,AD_1=AD_1, AD_2=AD_2))
}



ad.test<-function(y){
  y<-as.matrix(y)
  g<-dim(y)[2]

  AD_areas = AD_main(y)

  #For each value of g, it outputs . A, A_1, and A_2 (first two components of A) ,
  #and the value of A and A-1,A-2 for the areas combined if g>1

  if(g==1){
    AD_combined=NULL
  }else{
    AD_combined=AD_main(matrix(y,ncol=1))
  }

  out_list<-c(AD = AD_areas ,AD_combined=AD_combined)

  return(out_list)
}


