### 
### Lundi 03 Décembre 2018
### Résultat de l'évaluation du TP1 (cbverif)
### Promotion 2018-2019
### 


### Chargement des librairies
library(data.table)
library(stringr)
library(readr)

# > Changer la working directory si necessaire
setwd(getwd()) 

# > Préparation du parametre en entrée des fonctions
p <- as.list(c("data/cbverif_f1.txt","data/cbverif_f2.txt","data/cbverif_f3.txt"))


### Fonction de Matthias (bench)
cbverifGroupe0 <- function(path=list()) {
	getres <- function(filep) {
		cbverif <- fread(filep, header=F, sep=";", stringsAsFactors = F)
		res <- apply(cbverif,1, function(x) {
				u <- as.numeric(strsplit(gsub(" ","",x),split="")[[1]])
				l <- length(u)
				if (l >= 12 & l <= 19){
				  s <- seq(length(u)-1,1,by=-2)
				  u[s] <- u[s]*2
				  u[u>9] <- sapply(which(u>9), function(y) sum(as.numeric(strsplit(as.character(u[y]),split="")[[1]]))) 
				  return(ifelse(sum(unlist(u))%%10==0,1,0))
				} else {
				  return(0)
				}
			}
		)
		return(res)
	}	

	res.all <- lapply(path,function(x) getres(x))
	if(length(path)==1) { res.all <- unlist(res.all) }
	return(res.all)

}


### Groupe 1
### Timing: OK
### Success
cbverifGroupe1 <- function(path){
  
  manip.fichier <- function(fichier) {
    donnees <- read.table(file = fichier, sep = ",", col.names = "CB")
    donnees$CB <- gsub(" ", "", donnees$CB)
    CB.liste <- strsplit(donnees$CB, split= "", fixed=T)
    nblignes <- nrow(donnees)
    
    manip.CB <- function(x){
      last.element <- length(x)
      if (last.element >= 12 & last.element <= 19){
        verif <- character()
        for (j in seq(last.element, 1, -1)){
          if((last.element - j)%%2 != 0){
            verif[j] <- as.numeric(x[j]) * 2
            if(!(verif[j] %in% seq(0,9))){
              verif[j] <- as.numeric(substr(verif[j], 1, 1)) + as.numeric(substr(verif[j], 2, 2))
            }
          } else {
            verif[j] <- as.numeric(x[j])
          }
        }
        res <- ifelse(sum(as.numeric(verif))%%10 == 0, 1, 0)
      }
      else{
        res <- 0
      }
      return(res)
    }
    
    return(unlist(lapply(CB.liste, manip.CB)))
  }
  
  return(lapply(path,manip.fichier))
}

### Groupe 2
### Timing: KO
### Fail : Code source non envoyé
cbverifGroupe2 <- function(path){
  return(F)  
}

### Groupe 3 
### Timing: OK
### Success
cbverifGroupe3 <- function(path){
  ##Preparation donnees
  preparation <- function(data){
    vector <- list()
    for(i in 1:nrow(data)){
      a<-unlist(strsplit(data[i,]," "))
      b <- c()
      for(j in 1:length(a)){
        a1<-unlist(strsplit(a[j],""))
        c <- as.numeric(a1)
        b <- c(b,c)
      }
      vector[[i]] <- rev(b)
    }
    return(vector)
  }
  
  ##Calcul 
  calcul <- function(vector){
    resultat <- c()
    for(i1 in 1:length(vector)){
      length <- length(vector[[i1]])
      if(length<12 || length>19){
        resultat[i1] <- 0
      }
      else{
        sum <- 0
        seq_impair <- seq(1,length,2)
        seq_pair <- seq(2,length,2)
        for(j1 in seq_impair){
          sum <- sum+vector[[i1]][j1]
        }
        for(k1 in seq_pair){
          double <- 2*vector[[i1]][k1]
          if(double > 9){
            double <- strsplit(as.character(double),"")
            double <- as.numeric(double[[1]][1]) + as.numeric(double[[1]][2])
            sum <- sum+double
          }
          else{
            sum <- sum+double
          }
          
        }
        reste <- sum%%10
        if(reste!=0){
          resultat[i1] <- 0
        }
        else{
          resultat[i1] <- 1
        }
      }
      
    }
    return(resultat)
  }
  
  ##Resultat
  res <- list()
  for(i2 in 1:length(path)){
    data <- read.table(path[[i2]], sep=",",stringsAsFactors = F)
    res[[i2]] <- calcul(preparation(data))
  }
  return(res)
}

### Groupe 4 
### Timing: KO
### Fail : Code source non envoyé
cbverifGroupe4 <- function(path){
  return(F)
}

### Groupe 5
### Timing: OK 
### Fail : utilisation du "ou" plutot que "et"
cbverifGroupe5 <- function(path){
  
  validite_carte <- function(chemin){
    
    # Chargement du fichier
    
    cartes_bancaires <- read_delim(chemin, 
                                   ".", escape_double = FALSE, col_names = FALSE, 
                                   trim_ws = TRUE)
    colnames(cartes_bancaires)[1] <- "num"
    
    # Creation de la fonction
    
    validite <- function(numero){
      retour <- 0
      numero <- str_replace_all(numero, " ", "")
      n <- str_length(numero)
      if (n>11 || n <20){
        mat <- matrix(nrow=1,ncol=n)
        for (i in 1:n){
          mat[1,i] <- as.numeric(str_sub(numero,i,i))
        }
        indice <- n-1
        while(indice>0){
          var <- mat[1,indice]*2
          if (var>9){
            var <- as.numeric(str_sub(as.character(var),1,1)) + as.numeric(str_sub(as.character(var),2,2))
          }
          mat[1,indice] <- var
          indice = indice - 2
        }
        res <- sum(mat[1,])
        if (res %% 10 == 0){
          retour <- 1
        }
      }
      retour
    }
    
    # Application de la fonction 
    
    liste <- as.list(cartes_bancaires$num)
    Resultat <- lapply(liste,FUN=validite)
    Resultat <- unlist(Resultat)
    Resultat
  }
  
  
  renvoit <- lapply(path,FUN= validite_carte)
  renvoit
}

### Groupe 6
### Timing: OK 
### Success
cbverifGroupe6 <- function(path){
  library(stringr)
  taille_fichier <- length(path)
  res <- vector("list",taille_fichier)
  for(file in 1:taille_fichier){
    cb <- read_csv(path[[file]], col_names = FALSE)
    vec <- numeric(nrow(cb))
    for(i in 1:nrow(cb)){
      chaine <- gsub(" ","",cb[i,])
      taille <- str_length(gsub(" ","",chaine))
      if (taille < 12 || taille > 19){
        vec[i]<-0
      }else{
        nombre <- lapply(seq(taille-1,1,-2),FUN=function(x) as.numeric(str_sub(chaine,x,x))*2)
        somme = sum(unlist(lapply(nombre,FUN=function(x) as.integer((x > 9))*(x%%10 + (x - x%%10)/10) + as.integer((x <= 9))*x))) + sum(unlist(lapply(seq(taille,1,-2),FUN=function(x) as.numeric(str_sub(chaine,x,x)))))
        vec[i] <- as.integer(somme%%10==0)
      }}
    res[[file]]<-vec
  }
  return(res)
  
}

### Groupe 7
### Timing: OK  
### Success
cbverifGroupe7 <- function(path){
  library(stringr)
  
  resultat <- function(numero){
    code = str_replace_all(string=numero, pattern=" ", repl="")
    code = strsplit(code, "")[[1]]
    somme = 0
    n = length(code)
    if (n < 12 | n > 19){
      return (0)
    }
    else{
      indices = seq(from = n-1, by = -2, to = 1)
      for (i in indices){
        terme = as.numeric(code[i])*2
        if (terme >= 10){
          somme = somme + 1 + terme%%10
          
        }
        else{
          somme = somme + terme
        }
      }
      
      somme = somme + sum(as.numeric(code[-indices]))
      if (somme %% 10 == 0){
        return(1)
      }
      else{
        return(0)
      }
    }
  }
  
  uneliste = function(nomfichier){
    fichier = readLines(nomfichier)
    resultats = as.numeric(vapply(X = fichier, FUN = resultat, FUN.VALUE = numeric(1)))
  }    
  
  liste_resultats = lapply(X = path, FUN = uneliste)
  
  return(liste_resultats)
}

### Groupe 8
### Timing: OK
### Fail : pas de controle sur le nombre de digit
cbverifGroupe8 <- function(path){
  
  carte_verif <- function(fichier){
    
    numero = read.csv(fichier,header = F,quote = "")
    
    liste_no_carte = list()
    for (i in 1:nrow(numero)){
      a  = unlist(strsplit(as.character(numero[i, ]),split = ""))
      liste_no_carte[[i]] = as.numeric(a[-which(a==" ")])
    }
    
    
    
    Luhn = function(vec){
      vec_tmp_1 = vec
      pos = length(vec) -1
      while(pos > 0 ){
        vec_tmp_1[pos] = vec[pos]*2
        if (vec_tmp_1[pos]>9){
          vec_tmp_1[pos] = 1+(vec_tmp_1[pos]-10)
        }
        pos = pos -2
      }
      somme = sum(vec_tmp_1)
      # if (somme%%10==0){
      #   print("Le numero de carte est valide")
      # }
      # else
      #   print("Le numero de carte n'est pas valide")
      return (1*(somme%%10 ==0))
    }
    
    vapply(liste_no_carte, Luhn, FUN.VALUE = numeric(1))
  }
  lapply(path, carte_verif)
  
}

### Groupe 9
### Timing: OK
### Fail : au niveau de path[i] (besoin à minima d'un "unlist")
cbverifGroupe9 <- function(path){
  
  sommeChiffre <- function(x){
    if (x>9){
      return (1+x%%10)
    }
    else{
      return (x)
    }
  }
  
  verif <- function(ligne){
    chaine_splitee <- as.numeric(as.vector(strsplit(str_replace_all(ligne," ",""),split = ""))[[1]])
    
    if (length(chaine_splitee)%%2==0){
      a <- replace(chaine_splitee,seq(from=1,to=length(chaine_splitee),by=2),chaine_splitee[seq(from=1,to=length(chaine_splitee),by=2)]*2)
    }
    else{
      a <- replace(chaine_splitee,seq(from=2,to=length(chaine_splitee),by=2),chaine_splitee[seq(from=2,to=length(chaine_splitee),by=2)]*2)
    }
    if (sum(sapply(a, FUN=sommeChiffre))%%10==0){
      return(0)
    }
    else{
      return(1)
      
    }
  }
  
  res <- c()
  for (i in 1: length(path)){
    texte <- read.csv(path[i],sep = "\t", header=FALSE)
    res <- c(res,apply(texte,FUN = verif,1))
  }
  return(res)
  
}

### Groupe 10
### Timing: OK
### Fail
cbverifGroupe10 <- function(path){
  library(stringr)
  
  modif <- as.list(path)
  
  resultat <- numeric(nrow(path))
  
  for(j in 1:nrow(path)){
    
    modif$V1[j]
    
    line <- as.vector(modif$V1[j])
    
    line <- gsub(" ", "", line, fixed = TRUE)
    
    line2 <- as.numeric(unlist(strsplit(line, "")))
    
    
    
    if ((length(line2)/2)==0){
      seqcarte <- seq(from = 1, to = length(line2)-1, by = 2)
    }
    else{
      seqcarte <- seq(from = 2, to = length(line2)-1, by = 2)
    }
    line2[seqcarte] <- 2*line2[seqcarte]
    
    
    amultiplier <- which(line2[1:length(line2)]>9)
    for(i in amultiplier){
      line2[i] <- as.character(line2[i])
      x <- as.numeric(unlist(strsplit(line2[i], "")))
      x <- as.character(x[1]+x[2])
      line2[i] <- x
    }
    
    line2 <- as.numeric(line2)
    sum(line2)
    if((sum(line2)%%10)!=0) resultat[j] <- 1
    
  }
  return(resultat)
}

### Groupe 11 : 
### Timing: OK  
### Fail
cbverifGroupe11 <- function(cheminsFichiers){
  verifNumero<-function(numeroCompte){
    res<-0
    if(str_length(numeroCompte)>=12 && str_length(numeroCompte)<=19){
      num<-as.numeric(str_split(numeroCompte,"")[[1]])
      c<-str_length(numeroCompte)-1
      while(c>=1){
        num[c]<-2*num[c]
        if(num[c]>9){
          num[c]<-as.numeric(str_split(toString(num[c]),"")[[1]][1])+as.numeric(str_split(toString(num[c]),"")[[1]][2])
        }
        c<-c-2
      }
      s<-sum(num)
      if(s%%10==0){
        res<-1
      }
    }
    return(res)
  }
  nbFichiers=length(cheminsFichiers)
  res<-list()
  for(i in 1:nbFichiers){
    data<-read.table(file=cheminsFichiers[[1]],header=FALSE,sep=";")
    data<-as.data.frame(apply(data,1,str_replace_all," ",""))
    colnames(data)<-c("NumeroCarte")
    resFichier<-apply(data,1,verifNumero)
    res[[i]]<-resFichier
  }
  return(res)
  
  
}


######
###### NOTATION
######

nb.groups <- 11

# Prepa des function calls
funcList <- as.list(paste("cbverifGroupe",1:nb.groups,sep=""))

# Prepa de la matrice de resultat
df.notation <- data.frame("Groupe" = c(paste("Groupe0", 1:9,sep=""),paste("Groupe",10:nb.groups, sep="")))

# Si à l'heure = T
df.notation$ontime <- c(T,T,T,T,T,T,T,T,T,T,T)

df.notation$times <- 0
df.notation$equals <- F

res00 <- cbverifGroupe0(p)

for (i in 1:nb.groups) {
  res <- NULL
  f <- get(funcList[[i]])
  tryCatch({
    df.notation$times[i] <- system.time(res <- f(p))[[3]]
    df.notation$equals[i] <- isTRUE(all.equal(res00,res, check.attributes = F))
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
       subtitle="Temps d'execution par groupe pour l'exercice CBverif") +
  coord_flip()

