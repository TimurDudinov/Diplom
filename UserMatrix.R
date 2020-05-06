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

NameRows<-function(NumberPeople) #
{
  np<-c(1:NumberPeople)
  persons<-paste0("person",np)
  return(persons)
}

a<-c(rnorm(N))
b<-c(rnorm(N))

GenUserMatrix<-function(IncidMatr,NumberPeople)
{
  copy<-NumberEnds
  nr<-NameRows(NumberPeople)
  UserMatrix<-matrix(0,ncol = dim(IncidMatr)[2],nrow = NumberPeople,dimnames = list(nr,NULL))
  M<-dim(IncidMatr)[2]
  NumberEdge<-c()
  fire<-c()
  for (j in 1:M)
  {
    if (IncidMatr[1,j] == 1)
    { 
      fire[j]<-j #все ребра выходящие из первой вершины
    }
  }
  fire<-fire[!is.na(fire)]
  for (pr in 1:NumberPeople)
  {
    t<-c(rnorm(NumberPeople,mean = 0,sd=1))
    teta<-t[pr]
    z<-c(a*teta+b)
    p<-c(exp(z[1:OutOfDegree])/sum(exp(z[1:OutOfDegree])))
    print(p)
    print("z:")
    print(z)
    re<-sample(fire,1,prob = p)#чтобы мы всегда выходили из первой вершины
    UserMatrix[pr,re]<-1
    NumberEnds<-copy#так как будут удаляться терминальные вершины, то для каждого нового человека
    # вектор конечных вершин заново обновляется
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
            NumberEdge[j]<-j# номера столбцов,где есть 1 в матрице инцидентности
            
          }
          j<-j+1
        }
        NumberEdge<-NumberEdge[!is.na(NumberEdge)]
        if (is.null(NumberEdge) != T)
        {
          re<-sample(NumberEdge,1,prob = p)#следующее ребро
          UserMatrix[pr,re]<-1
        }else{#если NumberEdge нулевой,то мы находимя в термиенальной вершине
          NumberEnds<-NumberEnds[-(which(NumberEnds==i))]# удаляем ее из NumberEnds
        }
      }
      NumberEdge<-c()
      i<-i+1
    }
    }
  }
  return(UserMatrix)
}






















































  


























































