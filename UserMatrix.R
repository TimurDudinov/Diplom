library(Matrix)
library(installr)
library(matlab)
library(MASS)
library(igraph)
library(ggplot2)
library(TAM)
N<-
OutOfDegree<-
IncidMatr<-GenIncidenMatrix(N,OutOfDegree)

NumberPeople<-10

NameRows<-function(NumberPeople)
{
  np<-c(1:NumberPeople)
  persons<-paste0("person",np)
  return(persons)
}

a<-c(rnorm(N))
b<-c(rnorm(N))
teta<-c(rnorm(NumberPeople,mean = 0,sd=1))
z<-c(a*teta+b)
p<-c(exp(z[1:OutOfDegree])/sum(exp(z[1:OutOfDegree])))
GenUserMatrix<-function(IncidMatr,NumberPeople)
{
# for each item
copy<-NumberEnds
nr<-NameRows(NumberPeople)
M<-dim(IncidMatr)[2]
UserMatrix<-matrix(0,ncol = M,nrow = NumberPeople,dimnames = list(nr,NULL))
NumberEdge<-c()
fire<-c()
for (j in 1:M)
{
  if (IncidMatr[1,j] == 1)
  { 
  fire[j]<-j #random edge from first vetex
  }
}
fire<-fire[!is.na(fire)]
for (pr in 1:NumberPeople)
{

  re<-sample(fire,1,prob = p)#random edge
  UserMatrix[pr,re]<-1
  NumberEnds<-copy
while (length(NumberEnds) == AmountEnds)
  { i<-1
    while(i !=N+1)
    {
    if (IncidMatr[i,re] == -1)
    {
      
      j<-1
      while (j!=M+1)
      {
        
        if (IncidMatr[i,j] == 1)
        {
          NumberEdge[j]<-j# сюда положили номера столбцов из матрицы инцидентности, где есть 1
          
        }
        j<-j+1
      }
      NumberEdge<-NumberEdge[!is.na(NumberEdge)]#delete NA
      if (is.null(NumberEdge) != T)
      {
        re<-sample(NumberEdge,1,prob = p)#next random edge
        UserMatrix[pr,re]<-1
      }else{#we are in end vertex
        NumberEnds<-NumberEnds[-(which(NumberEnds==i))]# delete this vertex
      }
    }
    NumberEdge<-c()
    i<-i+1
  }
  }
}
return(UserMatrix)
}















  


























































