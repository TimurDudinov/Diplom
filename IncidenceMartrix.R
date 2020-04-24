library(Matrix)
library(installr)
#N - number of vertices = number of rows in incidence matrix
#M - number of edges = number of columns in incidence matrix
#OutOfDegree -number of outcoming edges from every vertices
N<-
OutOfDegree<-
AmountEnds<-sample(1:(N-2),1)
NumberEnds<-c(sample(2:N,AmountEnds))
NumberEnds<-NumberEnds[order(NumberEnds)]#order in increasing
# get matrix where some end vertices 
GenIncidenMatrix <- function(N, OutOfDegree) { 
    
    
    M <- N * OutOfDegree
    
    A <- matrix(0, nrow = N, ncol = M)
    
    edgesIndex <- 1
    for (i in 1:(N-AmountEnds))
    {
        
        A[i,edgesIndex]<-1
        A[i+1,edgesIndex]<- -1
        edgesIndex<- edgesIndex+1
        
    }
    targets <- c(1:N)
    for (i in 1:(N-AmountEnds))
    { targets<-targets[-1]
    for (j in 1:(OutOfDegree-1))
    {
        if(is.empty(targets)!=T)
        {
            A[i,edgesIndex]<-1
            target<-targets[sample(1:length(targets),1)]
            A[target,edgesIndex]<- -1
            edgesIndex<- edgesIndex+1
        }
    }
    }
    #удаление нулевых столбцов
    B<-A
    k <- 0
    for (i in 1:length(A[1,])){
        if (sum(A[, i] == 0) == length(A[,1])) {
            B <- B[,-i + k]
            k <- k + 1
        }
    }
    A<-B
    rm(B)
    rm(k)
    Vertices<-c(1:N)
    Ends<-Vertices[-(1:(N-AmountEnds))]
    
    #replace end rows    
    tch<-c()
    tch1<-c()
    for ( i in 1:AmountEnds)
    {
        if ((Ends[i] %in% NumberEnds) == F)
        {
            tch[i]<-Ends[i]
        }
        if ((NumberEnds[i] %in% Ends)==F)
        {
            tch1[i]<-NumberEnds[i] 
        }
    }
    if (length(tch) !=0)
    {tch<-tch[!is.na(tch)]
    tch1<-tch1[!is.na(tch1)]
    
    for (i in 1:AmountEnds)
    { 
        if (is.na(tch[i]))
        {break}
        copy<-A[tch[i],]
        A[tch[i],]<-A[tch1[i],]
        A[tch1[i],]<-copy
    }
    }
    return(A)
}








































