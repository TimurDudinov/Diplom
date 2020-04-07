library(Matrix)
library(installr)
library(matlab)
library(MASS)
library(igraph)
library(ggplot2)
N<-7 
plus<-6
Vertices<-c(1:N)
AmountEnds<-sample(1:(N-1),1)

NumberEnds<-sample(2:N,AmountEnds)

CountNotEnds<-length(Vertices[-(NumberEnds)])

IncidMatr<-GenIncidenMatrix(N,plus)

M<-dim(IncidMatr)[2]
NameEdges<-function(IncidMatr)
{
  ColumnIndex<-c()
  for (i in 1:M)
  {
    ColumnIndex[i]<-0
  }
  Numbers<-c()
  k2<-0
  for (i in 1:(N-1))
  {
    j<-1
    while(j!=M+1)
    {
      
      if (IncidMatr[i,j] == 1)
      { k2<-k2+1
      Numbers[k2]<-j
      ColumnIndex[k2]<-paste0("e",i,Numbers[k2])
      }
      j<-j+1
    }
  }
  return(ColumnIndex)
}






















NumberPeople<-100

UserMatrix<-matrix(0,nrow = NumberPeople,ncol = M)
NumberVertex<-0       
NumberEdge<-c()
fire<-c()
k0<-0
for (j in 1:M)
{
  if (IncidMatr[1,j] == 1)
  { k0<-k0+1
  fire[k0]<-j #first random edge
  }
}
for (pr in 1:NumberPeople)
{
re<-sample(fire,1)#random edge
UserMatrix[pr,re]<-1
i<-1
while (i != N+1)#идем по вершинам в матрице инцидентности
{
  if (IncidMatr[i,re] == -1)
  {
    NumberVertex<-i
    k1<-0
    j<-1
    while (j!=M+1)
    {
      k1<-k1+1
    if (IncidMatr[i,j] == 1)
      {
      NumberEdge[k1]<-j# сюда положили номера столбцов из матрицы инцидентности, где есть 1
     
    }
      j<-j+1
    }
    NumberEdge<-NumberEdge[!is.na(NumberEdge)]
    if (is.null(NumberEdge) != T)
    {
    re<-sample(NumberEdge,1)#next random edge
    UserMatrix[pr,re]<-1
    }
  }
  NumberEdge<-c()
  i<-i+1
}
}















  


























































