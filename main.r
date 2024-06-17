# The R code for Algorithms published in Samavat et al, Neural Computation, 2024. 


rm(list=ls(all=TRUE))

# Installing
#install.packages("readr")
# Loading
#library("readr")
# https://cran.r-project.org/web/packages/readr/index.html

library(xlsx)

# Setting the working directory 


setwd("")

# when rounding the values, up-to how many significant digit you want the values be rounded to.

signif_digit=2

# Functions

# Calculating the coefficient of variation for pair (a,b)

COOV <-  function(a, b ){ 
  D=c(a,b)
  
  if (a==0 && b==0){CV=0}
  else{CV=(sqrt((a-mean(D))^2+(b-mean(D))^2))/mean(D)}
  CV}


# Entropy Function 
entropyy <-  function(freqs){ 
  H <- -sum(freqs * log2(freqs))
  H 
}

# Entropy function test, entropy equals 1 for binary random variable with pr=0.5
Entropy_Test=c(1/2,1/2)

entropyy(Entropy_Test)


#  Loading the pairs for measuring precision level

Pairs_Set <- read.xlsx("Pairs.xlsx", sheetName = "Sheet1")


#  Loading the complete set of data


Complete_Feature_set <- read.xlsx("Complete_Set.xlsx", sheetName = "Sheet1")

# Check the column names of the "pairs" dataset are correct 
colnames(Pairs_Set)

# Check the column names of the full sample set are correct 
colnames(Complete_Feature_set)

######################################################################################


# Precision Analysis: 

SDSA_Pairs <- list(Pairs_Set$G1_Value,Pairs_Set$G2_Value)


SDSA_Pairs_CVs=NULL
N=NULL
Mean_of_SDSA_Pairs=NULL
M=NULL
D=NULL

for (i in 1:length(Pairs_Set[,1])){
  
  N=COOV(SDSA_Pairs[[1]][[i]],SDSA_Pairs[[2]][[i]])
  
  a=SDSA_Pairs[[1]][[i]]
  b=SDSA_Pairs[[2]][[i]]
  

  
  M=c(SDSA_Pairs[[1]][[i]],SDSA_Pairs[[2]][[i]])
  
  
  Mean_of_SDSA_Pairs=rbind(Mean_of_SDSA_Pairs,mean(M))
  
  SDSA_Pairs_CVs=rbind(SDSA_Pairs_CVs,N)
  N=NULL
  M=NULL
  a=b=NULL
  d=NULL
}


median(SDSA_Pairs_CVs)

# Precision level: 
Precision_Level=median(SDSA_Pairs_CVs)

Precision_Level

Mean_of_SDSA_Pairs

SDSA_Pairs_CVs

################################### Standard error of median (SEM) #####################


# SEM designed from: 

# Efron, B., & Hastie, T. (2021). Computer age statistical inference, student edition: Algorithms, evidence, and data science (Vol. 6). Cambridge University Press.

# and communicated with Dr. Wenxin Zhou. 

median(SDSA_Pairs_CVs)


bootstrap_standard_error_of_median <- function(X, B)
{
  # Bootstrap variance estimate for the median estimator
  # X: Data
  # B: Number of Monte Carlo samples
  n <- length(X)
  mhat.boot <- numeric(B)
  for (j in 1:B){
    X.boot <- sample(X,n,replace=TRUE)# Sample bootstrap data from Fn
    mhat.boot[j] <- median(X.boot) # Median bootstrap sample
  }
  sd.boot <- sd(mhat.boot) # bootstrap variance estimate
  return(list(sd.boot = sd.boot, mhat.boot = mhat.boot))
}

B <- 10000
X <- SDSA_Pairs_CVs[,1]

results.DG_30MIN_Control = bootstrap_standard_error_of_median(X,B)
#  standard error of median bootstrap estimate
sd.boot.DG_30MIN_Control = results.DG_30MIN_Control$sd.boot
sd.boot.DG_30MIN_Control
##########################################################################################
# By "for loop"

B <- 10000
X <- SDSA_Pairs_CVs[,1]


Sample_Dummy_CV_SDSA=NULL


for (i in 1:B) {
  
  Sample_Dummy_CV_SDSA=NULL
  Sample_Dummy_CV_SDSA=sample(X,length(X), replace=T)

  d=median(Sample_Dummy_CV_SDSA)
  
  D=rbind(D,d)
  

  

  
} # if

Standard_Error_DG_30MIN_Control_CV=sd(D)

Standard_Error_DG_30MIN_Control_CV



####################################################################################

#  Constructing Ns distinguishable sizes by applying algorithm 2 and utilizing the Precision_Level and complete set 

Complete_Feature_set=Complete_Feature_set[order(Complete_Feature_set$Feature_Value, decreasing = FALSE),]

Complete_Feature_set


Feature_set=Complete_Feature_set$Feature_Value


nrow(Feature_set)

List_of_shcluster=NULL
CV_of_Clusters=NULL
List_Bins_Content<- list()

j=1;
I=NULL

i=1

Cl=I=NULL
Pr=NULL



# Here we add 1e-06 to the precision level if in case the precision level equals zero, the code still will work

Precision_Level=Precision_Level+1e-06

Precision_Level

Full_Sample=Complete_Feature_set$Feature_Value


Full_Sample=as.data.frame(Full_Sample)


while (nrow(Full_Sample)!=0)
{
  
  # II[,i]=append(II[,i],I)
  
  Cl=I=NULL
  K=NULL
  for (i in 1:nrow(Full_Sample)) {
    
    C= COOV(Full_Sample[j,1], Full_Sample[i,1])
    
    if ( C< Precision_Level){
      
      Cl<- rbind(Cl,Full_Sample[i,1])    # Add

      I=rbind(I,i)

    } # if
    
    
  } #for
  
  K=nrow(Cl)/nrow(Complete_Feature_set)
  Pr=rbind(Pr,K)
  
  Cl<-sort(Cl, decreasing = FALSE)
  
  List_Bins_Content[[length(List_Bins_Content) + 1]] <- Cl
  
  
  Full_Sample<-as.data.frame(Full_Sample[-I,1])    # Deleting
  
}

# measuring the entropy 
entropyy(Pr)

ENT=entropyy(Pr)

print(ENT)


# measuring the maximum entropy 


Maximum_entropy=log2(length(List_Bins_Content))

print(Maximum_entropy)


List_Bins_Content

print(List_Bins_Content)

length(List_Bins_Content)

Eta=(ENT/log2(length(List_Bins_Content)))*100
print(Eta)

Efficiency_ratio=paste0(signif(Eta,signif_digit),"%")
Efficiency_ratio
 



#################################################################
# Generating Bins that are CV width bins. Example [a,b) is a bin with start value a (included) and b value as the end point (excluded). Coefficient of variation of CV(a,b)=Precision_Level
Precision_Level
PR=Precision_Level


B=List_Bins_Content

New_Bins=NULL 

for (i in 1:length(B)){
  
  
  C=c(B[[i]][[1]],B[[i]][[1]]*((sqrt(2)+PR)/(sqrt(2)-PR)))
  
  New_Bins=rbind(New_Bins,C)
  
}


print(New_Bins)

# Save the start and end value [a,b] of the distinguishable bins

write.csv(New_Bins, "Bins.csv", row.names = FALSE)

# Save the feature values found in each of the distinguishable bins. 

capture.output(List_Bins_Content, file = "List_Bins_Content.txt") 

#  Output saved as a CSV file containing the precision level, entropy, and Eta (efficiency ratio)

SISC_Output <- data.frame(Precision_Level=signif(Precision_Level,signif_digit),
                             Entropy_bits=signif(ENT,signif_digit),
                             Maximum_entropy=signif(Maximum_entropy,signif_digit),
                          
                             Efficiency_ratio=Efficiency_ratio)
print(SISC_Output)


# Save SISC_Output
write.csv(SISC_Output, "SISC_Output.csv", row.names = FALSE)




