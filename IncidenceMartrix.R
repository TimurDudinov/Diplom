library(Matrix)
library(installr)
#N - number of vertices = number of rows in incidence matrix
#M - number of edges = number of columns in incidence matrix
#plus -number of outcoming edges from every vertices
N<-
plus<-
Vertices<-c(1:N)
AmountEnds<-sample(1:(N-1),1)#amount end vertices
NumberEnds<-sample(2:N,AmountEnds)#numbers of end vertices
CountNotEnds<-length(Vertices[-(NumberEnds)])
# matrix there are some end vertices
GenIncidenMatrix <- function(N, plus) { 
    M <- N * plus
    A <- matrix(0, nrow = N, ncol = M)
    edgesIndex <- 1
   
    for (i in 1:CountNotEnds){
        if (Vertices[-(NumberEnds)][i]!=N)
        {
        A[Vertices[-(NumberEnds)][i], edgesIndex] <- 1
        A[Vertices[-(NumberEnds)][i]+1, edgesIndex] <- -1
        edgesIndex <- edgesIndex + 1
        }
    }
    targets <- c()
    for (j in 1:N) {
        targets[j] <- j
    }
    for (i in 1:CountNotEnds) {
        targets <- targets[-1]
        for (j in 1:(plus - 1)) {
            if (is.empty(targets) != T) {
                A[Vertices[-(NumberEnds)][i], edgesIndex] <- 1
                target <- targets[sample(1:length(targets), 1)]
                A[target, edgesIndex] <- -1
                edgesIndex <- edgesIndex + 1
            }
        }
    }
    #delete zero columns
    B <- A
    k <- 0
    for (i in 1:length(A[1, ])) {
        if (sum(A[, i] == 0) == length(A[, 1])) {
            B <- B[, - i + k]
            k <- k + 1
        }
    }
    A <- B
    return(A)
}
