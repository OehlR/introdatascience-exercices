### 
### Lundi 03 Décembre 2018
### Résultat de l'évaluation du TP2 (olsen)
### Promotion 2018-2019
### 


### Chargement des librairies
library(data.table)
library(stringr)
library(readr)

# > Changer la working directory si necessaire
setwd(getwd()) 

# > Préparation du parametre en entrée des fonctions
p <- "data/olsen1.csv"


### Fonction de Matthias (bench)
olsenGroupe0 <- function(path) {
  
  getJum <- function(x) {
    ids <- as.numeric(which(x==0))
    if(length(ids)==2) {
      return(ids)
    } else {
      return(NULL)
    }
  }
  
  olsentmp <- function(df) {
    cols <- 2:(length(df))
    d <- as.matrix(dist(df[,cols], method="manhattan"))  
    resultat <- data.frame(do.call("rbind",apply(d,1,getJum)))
    resultat <- unique(resultat)
    return(resultat)
  }
  
  df<- read.delim(path,sep=",", header=T)
  return(olsentmp(df))
  
}


### Groupe 1
### Timing: OK
### Fail/Success
olsenGroupe1 <- function(fichier){
  df <- read.table(file = fichier, sep = ",", header= T)
  duplicated2 <- function(x){
    if (sum(dup <- duplicated(x))==0)
      return(dup)
    if (class(x) %in% c("data.frame","matrix"))
      duplicated(rbind(x[dup,],x))[-(1:sum(dup))]
    else duplicated(c(x[dup],x))[-(1:sum(dup))]
  }
  dup <- duplicated2(df[,-1])
  base.dup <- df[dup,]
  jumeaux <- list()
  for(i in 1:nrow(base.dup)){
    indices_jum <- numeric()
    k=1
    for(j in 1:nrow(base.dup)){
      if(isTRUE(all.equal(target = base.dup[i,-1], current = base.dup[j,-1], check.attributes = F))){
        indices_jum[k] <- base.dup[j,1]
        k = k+1
      }
    }
    jumeaux[[i]] <- indices_jum
  }
  res <- unique(do.call("rbind", lapply(jumeaux, function(x){if(length(x)==2){return(x)}})))
  colnames(res) <- c("jum1.id", "jum2.id")
  return(as.data.frame(res))
}

### Groupe 2
### Timing: OK
### Fail/Success
olsenGroupe2 <- function(chemin){
  path <- read.table(file = chemin,h=T,sep=",")
  r <- nrow(path)
  c <- ncol(path)
  res <- data.frame(matrix(NA,ncol=2))
  colnames(res) <- c("jum1.id","jum2.id")
  compteur <- 0
  
  fonc <- function(x){
    t <- vect == x
    if ( sum(t%in%TRUE)==(c-1) ) {
      nb <<- nb+1
      temp <<- c(temp,x[1])
    }
  }
  
  for (k in 1:(r-1)){
    temp <<- c()
    vect <- path[k,]
    nb <- 0
    #
    apply(path[(k+1):r,],1,fonc)
    if (nb==1) {
      compteur <- compteur+1
      res[compteur,1]=path[k,1]
      res[compteur,2]=temp[1]
    }
    if (nb>1){
      path <- path[,-temp]
    }
  }
  return(res)
}

### Groupe 3 
### Timing: OK
### Fail/Success
olsenGroupe3 <- function(path){
  data<-read.csv(path,header=T)
  distance <- function(id1,id2){
    if(id1!=id2){
      diff <- data[id1,2:ncol(data)]-data[id2,2:ncol(data)]
      if(sum(diff %in% rep(0,ncol(data)-1))==(ncol(data)-1)){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    else{
      return(FALSE)
    }
  }
  
  a <- duplicated(data[,2:ncol(data)])
  resultat <- as.data.frame(setNames(replicate(2,numeric(0), simplify = F),c("jum1.id", "jum2.id")))
  if(ncol(data)-1<=16){
    for(i in data[a==TRUE,1]){
      jumeaux <- list()
      jumeaux[[1]] <- i
      sum=0
      for(j in data$id){
        if(distance(i,j)==TRUE){
          jumeaux[[length(jumeaux)+1]] <- j
          sum=sum+1
        }
      }
      if(sum==1){
        resultat[nrow(resultat)+1,] <- unlist(jumeaux)
      }
    }
  }
  resultat
}

### Groupe 4 
### Timing: KO
### Fail/Success
olsenGroupe4 <- function(path){
  return(F)
}

### Groupe 5
### Timing: OK
### Fail/Success
olsenGroupe5 <- function(path){
  data<-read.csv(path,header=T)
  Resultat <- matrix(ncol=2,nrow=0)
  cah <- hclust(dist(data[,2:9]), method = "ward.D2")
  clusCSP <- cutree(cah, h=0)
  matrice <- data.frame(val=clusCSP,ind=rep(1:nrow(data)))
  tab <- as.matrix(table(matrice$val,matrice$ind))
  for (i in 1:nrow(tab)){
    if (sum(tab[i,])==2){
      Resultat <- rbind(Resultat,colnames(tab)[tab[i,]==1])
    }
  }
  Resultat
}

### Groupe 6
### Timing: OK 
### Success
olsenGroupe6 <- function(chemin){
  data<-read.csv(chemin,header=T)
  nbDescripteur <- ncol(data)-1
  if (nbDescripteur>16){
    print("trop de descripteurs")
  }else{
    distance<-dist(data[,2:(nbDescripteur+1)])
    distance_m <- as.matrix(distance)
    indiceJ <- sapply(1:ncol(distance_m),FUN=function(x) sum(distance_m[,x]==0))
    ind<- which(indiceJ==2)
    res <- t(rbind(sapply(ind, FUN=function(x) which(distance_m[,x]==0))))
    colnames(res)<-c("jum1.id","jum2.id")
    doublons <- which(duplicated(res))
    resu<-res[-doublons,]
    if (length(resu)==2){
      return(data.frame(t(resu)))
    }
    else {
      return(data.frame(resu))
    }
  }
}

### Groupe 7
### Timing: OK
### Fail/Success
olsenGroupe7 <- function(path){
  data<-read.csv(path,header=T)
  p = ncol(data)-1
  n = nrow(data)
  res = data.frame()
  if (p > 16){
    print("ERREUR TROP DE DESCRIPTEURS")
  }
  else{
    
    
    score_individu <- function(vecteur){
      return(sum(vecteur*2^(0:(p-1))))
    }
    
    vect_score = numeric(n)
    
    vect_score = apply(data[,-1], MARGIN = 1, FUN = score_individu)
    facteur = factor(vect_score)
    table = table(facteur)
    score_jumeaux = levels(facteur)[which(table == 2)]
    
    # indice_score = function(score){
    #   return(data[which(vect_score == score), 1])
    # }
    
    # vapply(X = as.numeric(score_jumeaux), FUN = indice_score,FUN.VALUE = numeric(2))
    # print(res)
    
    ajout_pair = function(score){
      return(rbind(res,data[which(vect_score == score), 1]))
    }
    
    res = sapply(X = score_jumeaux , FUN = ajout_pair)
    temp=length(res[1,])
    
    return(as.data.frame(t(res[,order(as.numeric(res[1,]))])))
  }
}

### Groupe 8
### Timing: KO
### Fail/Success
olsenGroupe8 <- function(df){
  data2 = read.csv(df,header = T, sep = ",")
  caracteristique = data2[,-1]
  
  
  
  sibling = which(duplicated(caracteristique))
  all1 = which(duplicated(caracteristique))
  ind = all1[!all1%in%sibling]
  
  table_distance = matrix(0,nrow = length(ind),ncol = length(sibling))
  colnames(table_distance) = as.character(sibling)
  rownames(table_distance) = as.character(ind)
  
  for (i in 1:nrow(table_distance)){
    for  (j in 1:ncol(table_distance)){
      table_distance[i,j] = egal(caracteristique[as.numeric(rownames(table_distance)[i]),],
                                 caracteristique[as.numeric(colnames(table_distance)[j]),])
    }
  }
  
  
  indFin = which(apply(table_distance,1,sum)==1)
  
  
  jumeaux = data.frame(jum1.id = rownames(as.data.frame(indFin)),
                       jum2.id = rep(0,length(indFin)))
  for (i in 1:length(indFin)){
    jumeaux$jum2.id[i] = rownames(as.data.frame(which(table_distance[rownames(as.data.frame(indFin))[i],]==1)))
  }
  return(jumeaux)
}

### Groupe 9
### Timing: KO
### Fail/Success
olsenGroupe9 <- function(path){
  donnees<-read.csv(path,header=T)
  donnees <- donnees[,-1]
  a <- split(donnees,apply(donnees, 1,sum))
  resultat <- data.frame()
  res <- c()
  for (i in 1:length(a)){
    extrait <- data.frame(a[i])
    for (j in 1:(nrow(extrait))){
      for (k in j:nrow(extrait)){
        pareil=TRUE
        l=1
        while(l<=ncol(extrait) & pareil==TRUE){
          if (extrait[j,l] != extrait[k,l]){
            pareil=FALSE
          }
          else{
            l=l+1
          }
        }
        if (pareil==TRUE & j!=k){
          resultat <- rbind(resultat,c(as.numeric(rownames(extrait[j,])),as.numeric(rownames(extrait[k,]))))
          res <- c(res,rownames(extrait[j,]),rownames(extrait[k,]))
          names(resultat)=c("Individu 1","Individu 2")
        }
      }
    }
  }
  dd <- data.frame(table(res))
  doublons <- as.character(subset(dd,dd$Freq>1)[,1])
  resultatfinal <- subset(resultat, as.character(resultat$`Individu 1`)%in%doublons==FALSE | as.character(resultat$`Individu 2`)%in%doublons==FALSE)
  print(data.frame(resultatfinal))
}

### Groupe 10
### Timing: OK
### Fail/Success
olsenGroupe10 <- function(path){
  base<-read.csv(path,header=T)
  if(ncol(base)> 17){ 
    return(print("Le nombre de descripteur dépasse 16. Impossible de traiter."))
  }
  else{
    if(sum(is.na(base))!=0){
      return(print("La base de données contient des NA. Impossible de traiter."))
    }
    else{
      
      paire <- combn(nrow(base),2)
      
      data_sec <- vector("list",ncol(paire))
      
      data_sec <- lapply(as.list(1:ncol(paire)),function(i) base[c(paire[,i][1],paire[,i][2]),])
      
      resultat <- data.frame(jum1.id=NA, jum2.id=NA)
      
      for (j in 1:ncol(paire)){
        if (dist(data_sec[[j]][,-c(1)], method = "euclidian")[1]==0){
          resultat[nrow(resultat)+1,c("jum1.id","jum2.id")] <- paire[,j]
        }
      }
    }
  }
  resultat <- resultat[-c(1),]
  return(resultat)
}

### Groupe 11 
### Timing: OK
### Fail/Success
olsenGroupe11 <- function(path){
  distanceEntre2Individus<-function(ind1,ind2){
    return(sum(ind1[,2:ncol(ind1)]!=ind2[,2:ncol(ind1)]))
  }
  data<-read.csv(path,header=T)
  data_jumeaux<-NULL
  if(nrow(data)<= 10^4 & ncol(data) <= 17){
    for(i in 1:(nrow(data)-1)){
      ind1<-as.data.frame(data[i,])
      id_jumeau1<-ind1$id
      nb_jumeaux<-0
      for(j in (i+1):nrow(data)){
        ind2<-as.data.frame(data[j,])
        dist<-distanceEntre2Individus(ind2,ind1)
        if(dist==0){
          id_jumeau2<-ind2$id
          data_jumeaux<-rbind(data_jumeaux,c(id_jumeau1,id_jumeau2))
          nb_jumeaux<-nb_jumeaux+1
        }
      }
      data_jumeaux<-as.data.frame(data_jumeaux)
      if(nb_jumeaux>1){
        data_jumeaux<-data_jumeaux[-((nrow(data_jumeaux)-nb_jumeaux+1):nrow(data_jumeaux)),]
      }
    }
  }
  colnames(data_jumeaux)<-c("jum1.id","jum2.id")
  return(as.data.frame(data_jumeaux))
}


######
###### NOTATION
######

nb.groups <- 11

# Prepa des function calls
funcList <- as.list(paste("olsenGroupe",1:nb.groups,sep=""))

# Prepa de la matrice de resultat
df.notation <- data.frame("Groupe" = c(paste("Groupe0", 1:9,sep=""),paste("Groupe",10:nb.groups, sep="")))

# Si à l'heure = T
df.notation$ontime <- c(T,T,T,F,T,T,T,F,F,T,T)

df.notation$times <- 0
df.notation$equals <- F

res00 <- olsenGroupe0(p)
for (i in 1:nb.groups) {
  res <- NULL
  f <- get(funcList[[i]])
  tryCatch({
    df.notation$times[i] <- system.time(res <- f(p))[[3]]
    df.notation$equals[i] <- isTRUE(all.equal(dim(res00),dim(res), check.attributes = F))
  }, warning = function(wrn) {
    df.notation$times[i] <- system.time(res <- f(p))[[3]]
    df.notation$equals[i] <- 1
  }, error = function(err) {
    print("err")
    df.notation$times[i] <- 0
    df.notation$equals[i] <- F
  })
}


library(ggplot2)
df.notation$Groupe <- factor(df.notation$Groupe, levels = df.notation$Groupe[order(df.notation$Groupe,decreasing = T)])
ggplot(df.notation, aes(x=Groupe, y=times, label=NA)) + 
  geom_point(stat='identity', aes(col=`equals`), size=3)  +
  geom_segment(aes(y = 0, x = Groupe, yend = times, xend = Groupe),color = "black") +
  scale_color_manual(name="Reussite", 
                     labels = c("Echec", "Succes"), 
                     values = c("TRUE"="#00ba38", "FALSE"="#f8766d")) + 
  labs(title="Temps d'execution par Groupe", 
       subtitle="Temps d'execution par groupe pour l'exercice Olsen") +
  coord_flip()

