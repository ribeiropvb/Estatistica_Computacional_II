lambda= 3 # parameter of the distribution 
n=1000 # sample size
set.seed(1001)
U<-runif(n) # sample from uniform distribution

# Finding distribution function values for values of X
X<-c(0:20)
p<-c()
p[1]=exp(-lambda)
F<-c()
F[1]<-p[1]
for(i in 2:length(X)){
  p[i]=(lambda*p[i-1])/X[i]
  F[i]<-F[i-1]+p[i]
}
# distribution function of X
F


x<-c()
for(i in 1:n){
  x[i]<-min(which(U[i]<F))-1
}
# Frequency distribution of the generated distribution 
table(x)