library(Matrix)
library(installr)
#N - number of vertices = number of rows in incidence matrix
#M - number of edges = number of columns in incidence matrix
#OutOfDegree -number of outcoming edges from every vertices
N<-
OutOfDegree<-
AmountEnds<-sample(1:(N-2),1)
Vertices<-c(1:N)
#Ends<-Vertices[-(1:(N-AmountEnds))]
#NumberEnds<-c(sample(2:N,AmountEnds))
#NumberEnds<-NumberEnds[order(NumberEnds)]#упорядочили по возрастанию
NumberEnds<-Vertices[-(1:(N-AmountEnds))]
DelZeroColumn<-function(A){
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
    return(A)
}


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
    #удаляем нулевые столбцы
    A<-DelZeroColumn(A)
    return(A)
}








































