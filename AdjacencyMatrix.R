library(Matrix)
library(installr)
library(matlab)
library(MASS)
library(igraph)
library(ggplot2)
N <- #number of vertices
plus <- # number of outcoming edges
IncidMatr <- GenIncidenMatrix(N, plus) #generate incidence matrix
AdjMatr <- function(N) {
    Adj <- matrix(0, nrow = N, ncol = N)
    return(Adj)
}
#vector of values from incidence matrix to adjacency matrix
ZeroVector <- function(N) {
    value <- c()
    for (i in 1:N) {
        value[i] <- 0
    }
    return(value)
}
#filling adjancency matrix
FillingAdjMatr <- function(IncidMatr) {
    M <- dim(IncidMatr) #dimension of incidence matrix
    edgeRow <- 1
    while (edgeRow != N) {
        j <- 1
        while (j != M[2] + 1) {
            if (IncidMatr[edgeRow, j] == 1) {
                for (i in 1:N) {
                    if (IncidMatr[i, j] == -1) {
                        value[i] <- value[i] + 1
                    }

                }
            }
            j <- j + 1
        }
        Adj[edgeRow,] <- value
        value <- ZeroVector(N)
        edgeRow <- edgeRow + 1
    }
    return(Adj)
}
#inverse matrix
E <- eye(N)
Path <- ginv(E - Adj)
#plotting graph
dag1 <- graph_from_adjacency_matrix(Adj, mode = "directed", diag = FALSE)
tkplot(dag1)
is.dag(dag1) # check is this DAG
