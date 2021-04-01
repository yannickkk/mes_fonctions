#####################FUNCTIONS

####### Passer les premières lettres d'une colonne de df en Majuscule ou toutes les colonnes whole=TRUE
to_upper_first <- function(df,co, whole=FALSE){
if (whole){ 
 for (i in 1:dim(df)[2]) {
   df[,colnames(i)]<- tolower(df[,colnames(i)])
   df[,colnames(i)]<- paste0(toupper(substr(df[,colnames(i)], 1, 1)), substr(df[,colnames(i)], 2, nchar(as.character(df[,colnames(i)]))))
}} else {
cat("seulement les colonnes: ",co," seront concernées pour affecter toutes les colonnes utiliser whole= TRUE");
  if (length(co) == 1) { df[,co]<-paste0(toupper(substr(df[,co], 1, 1)), substr(tolower(df[,co]), 2, nchar(as.character(df[,co]))))} else
  for (i in 1:dim(df[,co])[2]) {
    df[,co[i]]<-paste0(toupper(substr(df[,co[i]], 1, 1)), substr(tolower(df[,co[i]]), 2, nchar(as.character(df[,co[i]]))))}
}
return(df)
}

####### Rendre la casse compatible avec UTF-8 (pour un dataframe)
####### exemple
##dist<-utf8(dbGetQuery(con,"SELECT distinct lfa_remarque FROM public.t_locfaons_lfa where lfa_remarque ~* 'sibbling';"))

utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  return(x)
}


########pour changer les codes echantillons cefs du format francais vers anglais from x_ddmmyyyy_ani_etiq to x_yyyymmdd_ani_etiq
library(stringr)
dmy2ymd<- function(x) {
    invers<- sapply(str_split(x, "_"),"[[",2)
    invers<- paste0(str_sub(invers,5,8),str_sub(invers,3,4),str_sub(invers,1,2))
    invers<-paste0(sapply(str_split(x, "_"),"[[",1),"_",invers,"_",sapply(str_split(x, "_"),"[[",3))
    return(invers)
}

########v2db ###permet de formater un vecteur R pour le rentrer dans une close sql column in ('xx','xx','xx')
########remplace les NA par NULL
########utiliser le ~ pour echapper un ' (~' sera changer en '' pour pouvoir être accepté par postgres
v2db<- function(x) {
if (length(x[which(is.na(x))]) != 0) {x[which(is.na(x))]<-"NA"} else {x<-x}
x<-as.vector(apply(as.data.frame(x),2,as.character))
x<- gsub("'","~'",x)
x[grep("^NA$",as.character(x))]<- ""
x<-paste0("('",paste(x,collapse ="','"),"')")
x<- gsub("''","NULL", x)
x<- gsub("~'","''", x)
return(x)
}

########v2dbn ###permet de formater un vecteur R pour le rentrer dans une close sql column in (xx,xx,xx)
v2dbn<- function(x) {
  x<-paste0("(",paste(x,collapse =","),")")
  return(x)
}

 ###permet de formater un vecteur R pour le rentrer dans une close sql column in ('xx','xx','xx') met en y ajoutant le excluded devant
#####lorsque le vecteur des champs contient insert_source et insert_timestamp ils sont remplacés par update_source et update_timestamp
##### x noms des champs et y noms des champs forment la contrainte unique
maj <- function(x,y) {
  x<-x[-match(y,x)]
  if (length(grep("_insert_",x))!=0){x<-x[-grep("_insert_",x)]} else {x<-x}
  updated<-v2dbn(x)
  alias<-strsplit(unlist(strsplit(gsub("','",",",gsub("\')","",gsub("\\('","",x))),","))[1],"_")[[1]][1]
  x<-gsub(alias,paste0("excluded.",alias),x)
  x<-paste0("(",paste(x,collapse =","),")")
  x<-paste(updated," = ",x)
  return(x)
}



###########################enlever les accents
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}
####################enlever les espaces dans un chaine
wsr<-function(x) {
  gsub("[[:space:]]","",x)
}


##########################détruit toute connection au serveur
mortauxcons <- function () {
  
  all_cons <- dbListConnections(PostgreSQL())
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}
########################usage ######
###########fermer toutes les connections en fin de session shiny
# close_session<-function(){
#   mortauxcons()
#   #stopApp()
# }
# session$onSessionEnded(close_session)
