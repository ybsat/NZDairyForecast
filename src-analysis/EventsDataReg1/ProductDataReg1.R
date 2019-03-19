events <- read.csv("events.csv")
### Data Cleaning
clean <- events[,c(-7,-5,-6)]
names(clean)[1] <- "Price"
clean$EventDate <- as.POSIXlt(as.Date(
  clean$EventDate,format = "%m/%d/%y"))
clean$EventDate$year <- clean$EventDate$year + 2000
clean$EventDuration <- strptime(clean$EventDuration, 
                                format = "%H:%M")
clean$EventDuration <- clean$EventDuration$hour*60 +
                      clean$EventDuration$min
clean$EventMonth <- clean$EventDate$mo + 1
cln <- clean[,-3]

### analysis
pairs(cln)
names(cln)[1] <- "Price"
lm0 <- lm(Price ~1,cln)
lmall <- lm(Price~.,cln)
step(lm0, scope=list(lower=formula(lm0), 
                     upper = formula(lmall)), 
     direction="both", trace=1)
modelop <- lm(Price ~ ParticipatingBidders + EventMonth + ChangeInPriceIndex
   + WinningBidders + TwelveMonthQtySold + QualifiedBidders, cln)
summary(modelop)
# the summary shows that the EventMonth is an insignificant factor
# modelop2 omits that factor for comparison
modelop2 <- lm(Price ~ ParticipatingBidders + ChangeInPriceIndex
               + WinningBidders + TwelveMonthQtySold + QualifiedBidders, cln)
summary(modelop2)

# Cross Validation (CV) to compare two methods
CVInd <- function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part   
  m<-floor(n/K)  #approximate size of each part   
  r<-n-m*K     
  I<-sample(n,n)  #random reordering of the indices   
  Ind<-list()  #will be list of indices for all K parts   
  length(Ind)<-K   
  for (k in 1:K) {      
    if (k <= r) 
      kpart <- ((m+1)*(k-1)+1):((m+1)*k)           
    else 
      kpart<- ((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))      
    Ind[[k]] <- I[kpart]
  }
  Ind}

Nrep<-20 #number of replicates of 
CVK<-10  #K-fold CV on each replicate
n.models = 2 #number of different models to fit
n=nrow(cln)
y<-cln$Price
yhat=matrix(0,n,n.models)
MSE<-matrix(0,Nrep,n.models)
for (j in 1:Nrep) {  
  Ind<-CVInd(n,K)  
  for (k in 1:K) {
    out<-lm(Price ~ ParticipatingBidders + EventMonth + ChangeInPriceIndex
            + WinningBidders + TwelveMonthQtySold + QualifiedBidders, cln[-Ind[[k]],])
    yhat[Ind[[k]],1]<-as.numeric(predict(out,cln[Ind[[k]],]))
    out<-lm(Price ~ ParticipatingBidders + ChangeInPriceIndex
            + WinningBidders + TwelveMonthQtySold + QualifiedBidders, cln[-Ind[[k]],])
    yhat[Ind[[k]],2]<-as.numeric(predict(out,cln[Ind[[k]],]))
  } #end of k loop  
  MSE[j,]=apply(yhat,2,function(x) 
    sum((y-x)^2))/n
} #end of j loop
MSE
MSEAve<- apply(MSE,2,mean); MSEAve #averaged mean square CV error
MSEsd <- apply(MSE,2,sd); MSEsd   #SD of mean square CV error
r2<-1-MSEAve/var(y); r2  #CV r^2