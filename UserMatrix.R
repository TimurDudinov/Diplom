library(Matrix)
library(installr)
library(matlab)
library(MASS)
library(igraph)
library(ggplot2)
N<-
OutOfDegree<-
IncidMatr<-GenIncidenMatrix(N,OutOfDegree)

teta<-c(rnorm(1,mean = 0,sd=1))
a<-c(rnorm(N))
#b<-c(rnorm(N))
z<-c(a*teta)
p<-c(exp(z[1:OutOfDegree])/sum(exp(z[1:OutOfDegree])))# for each item

NameEdges<-function(IncidMatr)
{
  ColumnIndex<-vector(mode ="integer", length = M)
  nv<-vector(mode ="integer", length = M)
  ne<-c(1:M)
  for (j in 1:M)
  { i<-1
  while(i !=N+1)
  {
    if (IncidMatr[i,j]==1)
    {
      nv[j]<-i
    }
    i<-i+1
  }
  }
  ColumnIndex<-paste0("e",nv,ne)
  return(ColumnIndex)
}

NameRows<-function(NumberPeople)
{
  np<-c(1:NumberPeople)
  persons<-paste0("person",np)
  return(persons)
}

M<-dim(IncidMatr)[2]
NumberPeople<-10

GenUserMatrix<-function(IncidMatr,NumberPeople)
{
nr<-NameRows(NumberPeople)
ne<-NameEdges(IncidMatr)
UserMatrix<-matrix(0,nrow = NumberPeople,ncol = M,dimnames = list(nr,ne))
NumberVertex<-0       
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
  re<-sample(fire,1,prob=p)#random edge
  UserMatrix[pr,re]<-1
  i<-1
  while (i != N+1)#go on vertex in incidence matrix
  {
    if (IncidMatr[i,re] == -1)
    {
      NumberVertex<-i
      j<-1
      while (j!=M+1)
      {
        
        if (IncidMatr[i,j] == 1)
        {
          NumberEdge[j]<-j# numbers of columns where 1
          
        }
        j<-j+1
      }
      NumberEdge<-NumberEdge[!is.na(NumberEdge)]
      if (is.null(NumberEdge) != T)
      {
        re<-sample(NumberEdge,1,prob=p)#next random edge
        UserMatrix[pr,re]<-1
      }
    }
    NumberEdge<-c()
    i<-i+1
  }
}
return(UserMatrix)
}















  


























































