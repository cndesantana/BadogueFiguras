library(shiny)
library(plotrix)
library(stringr)
library(quanteda)
library(readtext)
library(tm)
library(stringr)
library(dplyr)
library(wordcloud)
library(scales)
library(jpeg)
library(qdapRegex)
library(grDevices)

corpositivo <- "#20B2AA";
cornegativo <- "#c00000";
corneutro <- "#FFA500";
badwords <- c("boa","scontent.xx.fbcdn.net","https","oh","oe","pra","v","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que","q","é","sr","senhor","comentário","perfil","mais","com","está","por","uma","tem","vai","pelo","meu","sobre","não","já","nos","sem","quando","xed","xbd","ser","xbe","xa0","x8f","xb9","xb2","xb0","xb1","xb8","x8c","xa3","xbc","xaa","www.youtube.com","scontent.xx.fbcdn.net","https","oh","oe","pra","v","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que","q","é","sr","senhor","comentário","perfil","r","que","nao","sim","comentário","feito","comentario","imagem","comentario feito no perfil de secretaria","secretaria","foi","photos","http","bit.ly","sou","mais","bahia","vídeo","timeline","video","er","enem","vçpt","vç","x","vc", "aqui", "você", "tá", "dia", "amanhã", "ba","aqui")


cleaningRelNI1Temas <- function(relNI){

	relNI$Temas <- toupper(relNI$Temas)
   relNI$Temas <- str_replace_all(relNI$Temas, "DITRIBUIÇÃO DE ÁGUA","DISTRIBUIÇÃO DE ÁGUA")
      
   relNI$Temas <- str_replace_all(relNI$Temas, "\'","\"")
   relNI$Temas <- str_replace_all(relNI$Temas, "\\] \\[",",")
   relNI$Temas <- str_replace_all(relNI$Temas, "\\]  \\[",",")
   relNI$Temas <- str_replace_all(relNI$Temas, " , ",",")
	relNI$Temas <- str_replace_all(relNI$Temas, ", ",",")
	    
	relNI$Temas <- str_replace_all(relNI$Temas, "\"]","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\"] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \"]","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \"] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ]","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" ]","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" ] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\\]","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\\] ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \\]","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \\] ","")

	relNI$Temas <- str_replace_all(relNI$Temas, " \"\\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \"\\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\"\\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\"\\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" \\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" \\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" \\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" \\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\\[","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \\[ ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \\[","")
	
	relNI$Temas <- str_replace_all(relNI$Temas, "[\\[\\]]","")
										       
	relNI$Temas <- str_replace_all(relNI$Temas, "\",",",")
	relNI$Temas <- str_replace_all(relNI$Temas, " \",",",")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ,",",")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" ,",",")
	relNI$Temas <- str_replace_all(relNI$Temas, "\"","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \"","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ","")
	relNI$Temas <- str_replace_all(relNI$Temas, " \" ","")
	relNI$Temas <- str_replace_all(relNI$Temas, "\" ","\"")
	
	relNI <- relNI %>%
	   mutate(Temas = strsplit(as.character(Temas), ", ")) %>% 
	   unnest(Temas) 

	relNI <- relNI %>%
	   mutate(Temas = strsplit(as.character(Temas), ",")) %>% 
		unnest(Temas)
															         
	relNI$Temas <- str_replace_all(relNI$Temas, "SAÚDE ","SAÚDE")
	relNI$Temas <- str_replace_all(relNI$Temas, "SEGURANÇA ","SEGURANÇA")
	relNI$Temas <- str_replace_all(relNI$Temas, "OBRAS DA INFRAESTRUTURA ","OBRAS DA INFRAESTRUTURA")
	relNI$Temas <- str_replace_all(relNI$Temas, "INCENTIVO AO ESPORTE E LAZER ","INCENTIVO AO ESPORTE E LAZER")
	relNI$Temas <- str_replace_all(relNI$Temas, "TRANSPORTE PÚBLICO/MOBILIDADE ","TRANSPORTE PÚBLICO/MOBILIDADE")
	relNI$Temas <- str_replace_all(relNI$Temas, "HABITAÇÃO/MORADIA ","HABITAÇÃO/MORADIA")
	relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGOS ","GERAÇÃO DE EMPREGO")
	relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGO E RENDA ","GERAÇÃO DE EMPREGO")
	relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGO E RENDA","GERAÇÃO DE EMPREGO")
	relNI$Temas <- str_replace_all(relNI$Temas, "GERAÇÃO DE EMPREGOS","GERAÇÃO DE EMPREGO")
	relNI$Temas <- str_replace_all(relNI$Temas, "APOIO À AGRICULTURA ","APOIO À AGRICULTURA")
	relNI$Temas <- str_replace_all(relNI$Temas, "APOIO À CULTURA ","APOIO À CULTURA")
	relNI$Temas <- str_replace_all(relNI$Temas, "APOIO AO TURISMO ","APOIO AO TURISMO")
	relNI$Temas <- str_replace_all(relNI$Temas, "DISTRIBUIÇÃO DE ÁGUA ","DISTRIBUIÇÃO DE ÁGUA")
	relNI$Temas <- str_replace_all(relNI$Temas, "ESTRADAS E RODOVIAS ","ESTRADAS E RODOVIAS")
	relNI <- relNI %>% filter(!is.na(Temas))
	
	return(relNI)
}

cleaningRelNI1Grupos <- function(relNI1){
	relNI1$Grupos <- str_replace_all(relNI1$Grupos, " \"","")
   relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\"]","")
   relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\\[","")
   relNI1$Grupos <- str_replace_all(relNI1$Grupos, "\" ,",",")
	relNI1$Grupos <- str_replace_all(relNI1$Grupos, "Região 2 - Re","Região 4 - Re")
	    
   relNI1 <-  relNI1 %>%
             mutate(Grupos = strsplit(as.character(Grupos), ", ")) %>% 
   	        unnest(Grupos)
   relNI1 <-  relNI1 %>%
	         mutate(Grupos = strsplit(as.character(Grupos), ",")) %>% 
		       unnest(Grupos)
		          
    relNI1 <- relNI1 %>% 
        mutate(Grupos = stringr::str_replace_all(Grupos, "[\\[\\]]","")) %>% 
	     mutate(Grupos = strsplit(as.character(Grupos), ",")) %>% 
	     unnest(Grupos) %>% 
		  filter(Grupos != "CPBA_3629944") %>%
		  filter(Grupos != "Rui Correria_9356576")  %>%
		  filter(Grupos != "Neutro")%>%
		  filter(Grupos != "Carnaval_974981")%>%
		  filter(Grupos != "Carnaval") %>%
		  filter(Grupos != "Notícias Carnaval") %>%
		  filter(Grupos != "Rui Correria") %>%
		  filter(Grupos != "teste_6503868") %>%
		  filter(Grupos != "TESTE Região 11 Caetité")
    return(relNI1)
}

cleaningRelNI1Data <- function(relNI1){

	   ################### CLEANING GRUPOS
	   relNI1 <- cleaningRelNI1Grupos(relNI1)
      
   ################### CLEANING 'TEMAS'
   
   relNI1 <- cleaningRelNI1Temas(relNI1)
      
      relNI1$Temas <- str_replace_all(relNI1$Temas, '_.*', '')
      relNI1$Grupos <- str_replace_all(relNI1$Grupos, '_.*', '')
         
   return(relNI1)
}

getScreenshot <- function(link1, filename1){
   workdir <- "figures_badogue"
   if(file.exists(workdir)){
      resolution <- "1366x768"
      
      outputfile1 <- file.path(workdir,filename1)
      command1 = paste("pageres ", link1, " ", resolution, " --delay=2 --format='jpg' --verbose --sector='.contentArea' --filename=",outputfile1,sep="")
      if(length(system(paste("ls ",outputfile1),intern=TRUE))==0){
         system(command1)
      }else{
         system(paste("sudo rm ",workdir,"/",outputfile1,sep=""))
         system(command1)
      }
   }
}

fa <- function(x) iconv(x, to = "ASCII//TRANSLIT")
getMatrizDeOcorrencias <- function(text){
   text <- stringi::stri_trans_tolower(text)
   temp <- fa(text)
   temp <- rm_nchar_words(temp, "1")#remove words with only one character
   # Lowercase
   # Shrink down to just one white space
   temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
   temp=str_replace_all(temp,"[^[:graph:]]", " ") 
   # Split it
   any(grepl("I_WAS_NOT_ASCII", iconv(temp, "latin1", "ASCII", sub="I_WAS_NOT_ASCII")))
   temp <- stringi::stri_trans_general(temp, "latin-ascii")
   temp <- removePunctuation(temp)
   temp <- unlist(stringr::str_split(temp, " "))
   # Get rid of trailing "" if necessary
   indexes <- which(temp == "")
   if(length(indexes) > 0){
      temp <- temp[-indexes]
   }
   docs <- Corpus(VectorSource(temp))
   docs <- tm_map(docs, removeNumbers)
   # Remove english common stopwords
   docs <- tm_map(docs, removeWords, stopwords("portuguese"))
   # Remove your own stop word
   docs <- tm_map(docs, removeWords, c("blabla1", "blabla2","que","ser","pelo","tem","o","lhe","por","pra","de","da","do","essa","esse","isso","aquele","aquilo","desse","disso","daquilo","uma","um","NA")) 
#  Remove punctuations
   docs <- tm_map(docs, stripWhitespace)
#
   dtm <- TermDocumentMatrix(docs)
   m <- as.matrix(dtm)
   v <- sort(rowSums(m),decreasing=TRUE)
   d <- data.frame(word = names(v),freq=v)
   
   return(d)
}

getIndiceDeSentimento <- function(polaridade){
   sentimentos <- toupper(polaridade)
   allsentimentos <- c("NEUTRO","NEGATIVO","POSITIVO");
   mn <- length(which(sentimentos==allsentimentos[1]));#Neutro
   mr <- length(which(sentimentos==allsentimentos[2]));#Negativo
   mp <- length(which(sentimentos==allsentimentos[3]));#Positivo
   mt <- mr + mn + mp;#Total
  
   indicesentimento <- ifelse(mn == mt, 0, as.numeric((mp-mr)/(mt-mn)))

   return(indicesentimento)
}

getPositionY <- function(test){
   labelpos <- array(NA,length(test$Polaridade))
   labelpos[ which(test$Polaridade == "POSITIVO") ] <- "0.02"
   labelpos[ which(test$Polaridade == "NEGATIVO") ] <- "0.98"
   datasb <- test$Data[which(test$Polaridade == "NEUTRO")];
   posvarb <- which(test$Polaridade == "NEUTRO");
   for(i in 1:length(datasb)){
      datasb_ <- datasb[i];
      positionobsb <- which(test$Data == datasb_ & test$Polaridade == "POSITIVO")
      obsb <- ifelse(length(positionobsb) > 0, test$freq[positionobsb], 0);
      labelpos[posvarb[i]] <- obsb + 0.02
   }
   return(as.numeric(labelpos))
}

removeUsersOutliers <- function(file,percent){
   ncommentoutliers <- as.numeric(quantile(as.numeric(table(file$`Autor ID`)),percent))
   usersoutliers <- names(table(file$`Autor ID`)[which(as.numeric(table(file$`Autor ID`)) > ncommentoutliers)]);
   file <- file %>% filter(!(`Autor ID` %in% usersoutliers))
   return(file)
}

getDFMatrix <- function(text){
   myCorpus <- corpus(text)
   metadoc(myCorpus, "language") <- "portuguese"
   tokenInfo <- summary(myCorpus)
   kwic(myCorpus, "gestor")
   myStemMat <- dfm(myCorpus, remove = stopwords("portuguese"), stem = TRUE, remove_punct = TRUE)
   mydfm <- dfm(myCorpus, remove = c(stopwords("portuguese"),badwords), remove_punct = TRUE, remove_numbers= TRUE)
   return(mydfm)
   #   ap_td <- tidy(mydfm)
   #   names(ap_td) <- c("sentimento","term","count")
   #   return(ap_td);
}

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
   if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
   hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

function(input, output) {
   
   plotMelhorPiorPosts = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      idPostsData <- file$`ID do Post`
      uniqueIdsPostsData <- unique(sort(idPostsData))
      mymatrix <- list();
      for(iposts in 1:length(uniqueIdsPostsData)){
         idPost <- uniqueIdsPostsData[iposts]
         posPosts <- which(file$`ID do Post` == idPost)
         polaridadePostData <- file$Polaridade[posPosts]
         if(length(polaridadePostData) >= 5){
            sentimento <- getIndiceDeSentimento(polaridadePostData);
            cat(paste(idPost,sentimento,length(polaridadePostData),sep=" "),sep="\n");
            mymatrix <- rbind(mymatrix,cbind(as.character(idPost),sentimento, length(polaridadePostData)))         
         }
      }
      
      mymatrix <- data.frame(mymatrix)
      colnames(mymatrix) <- c("id","sentimento","ncomentarios")
      mymatrix <- mymatrix %>% filter(sentimento < 1 & sentimento > -1)
      mymatrix$id <- unlist(mymatrix$id)
      mymatrix$sentimento <- as.numeric(unlist(mymatrix$sentimento))
      mymatrix$ncomentarios <- as.numeric(unlist(mymatrix$ncomentarios))

      subset1 <- mymatrix %>% arrange(sentimento, ncomentarios) %>% tail(1)
      subset2 <- mymatrix %>% arrange(sentimento, ncomentarios) %>% head(1)
      referencias <- c(subset1$id, subset2$id)

      plot(-14:14,axes=FALSE,xlab="",ylab="",type="n")
      gradient.rect(1,-10,3,10,reds=c(1,0), greens=c(seq(0,1,length=10),seq(1,0,length=10)),blues=c(0,1),gradient="y")
      text(x = 0, y=10,  labels = 1,pos = 4)
      text(x = -0.3, y=-10, labels = -1,pos = 4)

      posReferenciasSentimento <- c(which(mymatrix$id==unlist(referencias)[1]),
                                    which(mymatrix$id==unlist(referencias)[2])
      )
      
      pos1 <- posReferenciasSentimento[1]
      pos2 <- posReferenciasSentimento[2]
      link1 <- paste("https://pt-br.facebook.com/",mymatrix$id[pos1],sep="")
      link2 <- paste("https://pt-br.facebook.com/",mymatrix$id[pos2],sep="")
      getScreenshot(link1, "post1")
      getScreenshot(link2, "post2")

      ima <- readJPEG("./figures_badogue/post1.jpg")   
      isent <- mymatrix$sentimento[pos1]
      rasterImage(ima, xleft = 7, ybottom=10*signif(isent,2)-3, xright = 20, ytop =10*signif(isent,2)+4)
      idpostsent <- mymatrix$id[pos1]
      text(x = 3, y=10*signif(isent,2), labels = paste(intToUtf8(9664),signif(isent,2)),pos = 4)
#      text(x = 7*((is%%2)+1), y=10*signif(isent,2), labels = idpostsent,pos = 4)
      
      ima2 <- readJPEG("./figures_badogue/post2.jpg")   
      isent2 <- mymatrix$sentimento[pos2]
      rasterImage(ima2, xleft = 7, ybottom=10*signif(isent2,2)-3, xright = 20, ytop =10*signif(isent2,2)+4)
      idpostsent2 <- mymatrix$id[pos2]
      text(x = 3, y=10*signif(isent2,2), labels = paste(intToUtf8(9664),signif(isent2,2)),pos = 4)
#      text(x = 7*((is%%2)+1), y=10*signif(isent2,2), labels = idpostsent2,pos = 4)

   }
   
   plotPostsPorSentimento = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      idPostsData <- file$`ID do Post`
      uniqueIdsPostsData <- unique(sort(idPostsData))
      mymatrix <- list();
      for(iposts in 1:length(uniqueIdsPostsData)){
         idPost <- uniqueIdsPostsData[iposts]
         polaridadePostData <- file$Polaridade[which(file$`ID do Post` == idPost)]
         if(length(polaridadePostData) >= 5){
            sentimento <- getIndiceDeSentimento(polaridadePostData);
            cat(paste(idPost,sentimento,length(polaridadePostData),sep=" "),sep="\n");
            mymatrix <- rbind(mymatrix,cbind(as.character(idPost),sentimento, length(polaridadePostData)))         
         }
      }
      
      mymatrix <- data.frame(mymatrix)
      colnames(mymatrix) <- c("id","sentimento","ncomentarios")
      mymatrix$id <- unlist(mymatrix$id)
      mymatrix$sentimento <- as.numeric(unlist(mymatrix$sentimento))
      mymatrix$ncomentarios <- as.numeric(unlist(mymatrix$ncomentarios))
      mymatrix <- mymatrix %>% filter(sentimento < 1 & sentimento > -1)
      
      mymatrix %>% 
         dplyr::select(id, sentimento, ncomentarios) %>% 
         group_by(id, sentimento, ncomentarios) %>% 
         arrange(sentimento, ncomentarios) %>%
         filter(ncomentarios > 100) %>%
         tail(60) %>%
         ggplot() + 
         geom_bar(stat="identity", aes(x=reorder(as.character(id),as.numeric(sentimento)),y=as.numeric(sentimento))) + 
         scale_fill_manual(cornegativo) +
         xlab("") + ylab("Sentimento dos Posts") + 
         geom_text( aes(x=reorder(as.character(id),as.numeric(sentimento)),y=as.numeric(sentimento), label = signif(as.numeric(sentimento),2) ) , vjust = 0, hjust = ifelse(as.numeric(sentimento) > 0,0,1), size = 3 ) +
         coord_flip()
      
   }

   plotIndiceSentimentos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
#      file <- removeUsersOutliers(file,0.95);#remove outliers (95% quantile)
      allpolaridade <- toupper(file$Polaridade)
      isent <- getIndiceDeSentimento(allpolaridade);

      colfunc <- colorRampPalette(c(corpositivo, corneutro, cornegativo))
      legend_image <- as.raster(matrix(colfunc(20), ncol=1))
      plot(c(1,20),c(-10,10),type = 'n', axes = F,xlab = '', ylab = '', main = '')
      text(x=3, y = 10*signif(isent,2), labels = paste(intToUtf8(9664),signif(isent,2)),pos=4)
      text(x = 0.45, y=10,  labels = 1,pos = 4)
      text(x = 0.4, y=-10, labels = -1,pos = 4)
      rasterImage(legend_image, 1,-10,3,10)
#      plot(-10:10,axes=FALSE,xlab="",ylab="",type="n")
#      gradient.rect(1,-10,3,10,reds=c(1,0), greens=c(seq(0,1,length=10),seq(1,0,length=10)),blues=c(0,1),gradient="y")
#      text(x = 3, y=10*signif(isent,2), labels = paste(intToUtf8(9664),signif(isent,2)),pos = 4)
#      text(x = 0, y=10,  labels = 1,pos = 4)
#      text(x = 0, y=-10, labels = -1,pos = 4)
      
      #    qplot(speed, dist, data = cars)
   }

   plotVariabilidade = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      datasPosts <- as.Date(file$Data,format="%d/%m/%Y")
      uniqueDataPosts <- unique(sort(datasPosts))
      mymatrix <- list();
      for(idata in 1:length(uniqueDataPosts)){
         data <- uniqueDataPosts[idata];
         posData <- which(datasPosts == data)
         idPostsData <- file$`ID do Post`[posData]
         uniqueIdsPostsData <- unique(sort(idPostsData))
         for(iposts in 1:length(uniqueIdsPostsData)){
            idPost <- uniqueIdsPostsData[iposts]
            polaridadePostData <- file$Polaridade[which(file$`ID do Post`[posData] == idPost)]
            if(length(polaridadePostData) >= 5){
               sentimento <- getIndiceDeSentimento(polaridadePostData);
               cat(paste(datasPosts[idata],length(polaridadePostData), idPost,sentimento,sep=" "),sep="\n");
               mymatrix <- rbind(mymatrix,cbind(as.character(data),idPost,sentimento))          
            }
         }
      }
      colnames(mymatrix) <- c("Data","id","sentimento")
      mymatrix <- as.data.frame(mymatrix)
      mymatrix <- mymatrix %>% filter(sentimento < 1 & sentimento > -1)
      ggplot(mymatrix, aes(x=ymd(Data),y=as.numeric(sentimento))) + geom_point(stat="identity") + 
         stat_smooth(method="loess", span=0.1, se=TRUE, aes(Sombra="Desvio"), alpha=0.3) +
         theme_bw() +
         xlab("Data") + ylab("Sentimento dos Posts") + ylim(-1,1)
   }
   
   
   plotCurtidasComentarios = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         dplyr::select(`Autor ID`, Polaridade, Comentários, Curtidas) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         #   filter(Polaridade == "Negativo") %>%
         arrange(`Autor ID`) %>%
         summarise(totalCurtidas = sum(as.numeric(Curtidas)), 
                   totalComentarios = sum(as.numeric(Comentários))) %>%
         arrange(totalComentarios, totalCurtidas) %>%
         filter(totalComentarios >= 0) %>%
         tail(50) %>%
         ggplot() + 
         geom_point(aes(x = totalComentarios, 
                        y = totalCurtidas, 
                        col = Polaridade)) + 
         ylab("Número de Curtidas") +
         xlab("Número de Comentários") +
         geom_text( aes (x = as.numeric(totalComentarios), 
                         y = as.numeric(totalCurtidas), 
                         label = ifelse(totalCurtidas > 5, `Autor ID`,"") ) , 
                    vjust = 0, hjust = 1, size = 2.5 )

   }
   
   plotComentaristasPopulares = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      file %>% 
         dplyr::select(`Autor ID`, Curtidas, Polaridade) %>%
         group_by(`Autor ID`) %>% 
         count(`Autor ID`, Curtidas, Polaridade) %>%
         arrange(n, `Autor ID`) %>%
         tail(50) %>% 
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), fill = Polaridade)
         ) + 
         ylab("Número de Curtidas em Comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         #   geom_text( aes (x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), label = as.numeric(n) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
      
   plotDetratoresApoiadores = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      file %>% 
         dplyr::select(`Autor ID`, Polaridade) %>%
         group_by(`Autor ID`) %>% 
         count(`Autor ID`, Polaridade) %>%
         arrange(n, `Autor ID`) %>%
         tail(50) %>% 
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), fill = Polaridade)) + 
         ylab("Número de comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         #   geom_text( aes (x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), label = as.numeric(n) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotDetratoresCurtidos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)

      file %>% 
         dplyr::select(`Autor ID`, Polaridade, Curtidas) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         arrange(`Autor ID`, Polaridade, Curtidas) %>%
         filter(Polaridade == "Negativo", Curtidas > 0) %>%
         arrange(`Autor ID`, Curtidas) %>%
         summarise(total = sum(as.numeric(Curtidas))) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), fill = Polaridade)
         ) + 
         ylab("Número de Curtidas") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         geom_text( aes (x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotDetratoresAssiduos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         dplyr::select(`Autor ID`, Polaridade) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         arrange(`Autor ID`, Polaridade) %>%
         filter(Polaridade == "Negativo") %>%
         arrange(`Autor ID`) %>%
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), fill = Polaridade)
         ) + 
         ylab("Número de Comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         geom_text( aes (x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }

   
   plotApoiadoresCurtidos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         dplyr::select(`Autor ID`, Polaridade, Curtidas) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         arrange(`Autor ID`, Polaridade, Curtidas) %>%
         filter(Polaridade == "Positivo", Curtidas > 0) %>%
         arrange(`Autor ID`, Curtidas) %>%
         summarise(total = sum(as.numeric(Curtidas))) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), fill = Polaridade)
         ) + 
         ylab("Número de Curtidas") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         geom_text( aes (x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotApoiadoresAssiduos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         dplyr::select(`Autor ID`, Polaridade) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         arrange(`Autor ID`, Polaridade) %>%
         filter(Polaridade == "Positivo") %>%
         arrange(`Autor ID`) %>%
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), fill = Polaridade)
         ) + 
         ylab("Número de Comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
         geom_text( aes (x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotPalavrasDetratores = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      autores <- file %>% 
         dplyr::select(`Autor ID`, Polaridade) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         arrange(`Autor ID`, Polaridade) %>%
         filter(Polaridade == "Negativo") %>%
         arrange(`Autor ID`) %>%
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         dplyr::select(`Autor ID`)
      
      text <- file %>%
         filter(as.character(`Autor ID`) %in% autores$`Autor ID`, Polaridade == "Negativo") %>%
         dplyr::select(Conteúdo) %>%
         toupper()
      
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 50)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = cornegativo) + 
         ylab("Número de ocorrências") +
         xlab("") +
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()      
      
   }
   
   plotPalavrasApoiadores = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      autores <- file %>% 
         dplyr::select(`Autor ID`, Polaridade) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         arrange(`Autor ID`, Polaridade) %>%
         filter(Polaridade == "Positivo") %>%
         arrange(`Autor ID`) %>%
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         dplyr::select(`Autor ID`)
      
      text <- file %>%
         filter(as.character(`Autor ID`) %in% autores$`Autor ID`, Polaridade == "Positivo") %>%
         dplyr::select(Conteúdo) %>%
         toupper()
      
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 50)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = corpositivo) + 
         ylab("Número de ocorrências") +
         xlab("") +
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
      
   }
      
   plotSerieTemporal = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)

      amostra <- file
      df_datas <- amostra %>%
         mutate( Data = dmy_hm(Data) %>%
                    as.Date() %>%
                    format("%d/%m/%Y"),
                 Polaridade = as.factor(toupper(Polaridade))
         ) %>%
         group_by(Data, Polaridade) %>%
         summarise(count = n()) %>%
         group_by(Data) %>%
         mutate(freq = count / sum(count))
      
      primeirodia <- min(dmy(df_datas$Data));
      ultimodia <- max(dmy(df_datas$Data))
      ggplot(df_datas, aes(x=dmy(Data), y=freq, fill=Polaridade)) +
         geom_bar(position = "stack", stat = "identity") +
         scale_x_date(date_breaks = "1 day",
                      labels = date_format("%d/%m/%Y")) +
         theme(text = element_text(size=6), axis.text.x = element_text(angle=90, hjust=1)) +
         scale_y_continuous(labels=scales::percent) +
         labs (title = "") +
         labs (x = "", y = "Porcentagem de Posts") +
         theme(text = element_text(size=6), axis.text.x = element_text(angle=45, hjust=1)) +
#         scale_fill_discrete(name="Sentimento") +
         coord_cartesian(xlim = c(primeirodia, ultimodia)) +
         scale_fill_manual("Sentimentos", values = c("POSITIVO" = corpositivo, "NEGATIVO" = cornegativo, "NEUTRO" = corneutro, "GOVERNO" = ggplotColours(n=4)[4])) +
#         scale_fill_manual("Sentimentos", values = c("POSITIVO" = ggplotColours(n=4)[3], "NEGATIVO" = ggplotColours(n=4)[1], "NEUTRO" = ggplotColours(n=4)[2], "GOVERNO" = ggplotColours(n=4)[4])) +
         geom_text(size = 2, col = "white", aes(x = dmy(Data), y = getPositionY(df_datas), label = paste(as.character(100*round(df_datas$freq,2)),"%",sep="")));
      
   }

   plotDifPreditores = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      my_group_number <- unlist(strsplit(unlist(strsplit(unlist(strsplit(filepath,"[.]"))[1],"ã"))[2],"o"))[2]
      
      file <- cleaningRelNI1Data(file)
      
      file <- file %>% 
	      group_by(Polaridade, Grupos, Temas) %>%
              summarise(npositivo = length(which(Polaridade == "Positivo")),
	                nnegativo = length(which(Polaridade == "Negativo")),
			nneutro = length(which(Polaridade == "Neutro")),
			is = ifelse(((npositivo + nnegativo)==0),0,ifelse((nnegativo + npositivo > 2),(npositivo - nnegativo) / (npositivo + nnegativo), 0)))%>%
            ungroup()%>%
            select(Grupos, Temas, npositivo, nnegativo, is)
      
      positivos <- file %>%
	       group_by(Grupos, Temas) %>% 
	       summarise(Comentarios = sum(npositivo)) %>%
		    arrange(Temas)

      negativos <- file %>%
         group_by(Grupos, Temas) %>% 
			summarise(Comentarios = sum(nnegativo)) %>%
			arrange(Temas)

		names(positivos) <- c("Grupos","Temas","Comentarios")
		names(negativos) <- c("Grupos","Temas","Comentarios")

		todos <- positivos
		names(todos) <- c("Grupos","Temas","Diferenca")
		todos$Diferenca <- positivos$Comentarios - negativos$Comentarios
		todos$Soma <- positivos$Comentarios + negativos$Comentarios
		todos <- todos %>% mutate(Sentimento = ifelse(Diferenca > 0, "Positivo","Negativo"))
		
		todos$all_group_number <- unlist(strsplit(unlist(strsplit(todos$Grupos, "-"))[seq(from=1, by = 2, length.out = length(todos$Grupos))], " "))[seq(from=2, by=2, length.out = length(todos$Grupos))]
		
		todos <- todos %>%
		   filter(all_group_number == my_group_number)
		
		todos <- todos %>% 
		   mutate(Porcentagem = ifelse(Soma == 0, 
		                               0, 
		                               ifelse(Sentimento == "Positivo",
		                                      signif(abs(Diferenca)/Soma,2)*100, 
		                                      signif(abs(Diferenca)/Soma,2)*-100))) %>% 
		   filter(Soma != 0)

		todos  %>%
		   ggplot(aes(x = reorder(Temas, Porcentagem), y = Porcentagem, fill=Sentimento)) + 
			ylim(-105,105) +
			scale_fill_manual("Sentimento", values = c("Positivo" = corpositivo, "Negativo" = cornegativo))+
			geom_text( aes(x=reorder(Temas, Porcentagem), y=Porcentagem, label = paste(Porcentagem,"%",sep="")), vjust = 0, hjust = ifelse(as.numeric(todos$Porcentagem) >= 0,0,1), size = 3 ) +
			geom_bar(stat="identity") + 
			ylab("Diferença entre Comentários Positivos e Negativos (%)") + xlab("Temas") + 
			coord_flip()
   }
 

   plotPreditores = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)

      my_group_number <- unlist(strsplit(unlist(strsplit(unlist(strsplit(filepath,"[.]"))[1],"ã"))[2],"o"))[2]

      file <- cleaningRelNI1Data(file)

      file <- file %>% 
	      group_by(Polaridade, Grupos, Temas) %>%
         summarise(npositivo = length(which(Polaridade == "Positivo")),
	                nnegativo = length(which(Polaridade == "Negativo")),
			          nneutro = length(which(Polaridade == "Neutro")),
			          is = ifelse(((npositivo + nnegativo)==0),
			                      0,
			                      ifelse((nnegativo + npositivo > 2),(npositivo - nnegativo) / (npositivo + nnegativo), 0)))%>%
         ungroup()%>%
         select(Grupos, Temas, npositivo, nnegativo, is)

      positivos <- file  %>% 
	      filter(npositivo > 0)%>%
         select(Grupos, Temas, npositivo)

	  negativos <- file %>%
	     select(Grupos, Temas, nnegativo) %>%
	     filter(nnegativo > 0) %>%
	     mutate(nnegativo = nnegativo * (-1)) %>%
	     select(Grupos, Temas, nnegativo)


	  names(positivos) <- c("Grupos","Temas","Comentarios")
	  names(negativos) <- c("Grupos","Temas","Comentarios")
	  todos <- rbind(positivos,negativos)
	  todos <- todos %>% mutate(Sentimento = ifelse(Comentarios > 0, "Positivo","Negativo"))
	  
	  todos$all_group_number <- unlist(strsplit(unlist(strsplit(todos$Grupos, "-"))[seq(from=1, by = 2, length.out = length(todos$Grupos))], " "))[seq(from=2, by=2, length.out = length(todos$Grupos))]
	  
	  todos <- todos %>%
	     filter(all_group_number == my_group_number)
	  
	  todos <- todos %>% 
	     group_by(Grupos, Temas, Sentimento) %>% 
	     summarise(count = sum(abs(Comentarios))) %>%
	     group_by(Temas) %>%
	     mutate(Porcentagem = 100*count / sum(count) ) %>%
	     arrange(Temas)

		todos %>%
		   ggplot(aes(x = reorder(Temas, Porcentagem), y = Porcentagem, fill=Sentimento)) + 
		   geom_bar(stat="identity") + 
		   scale_fill_manual("Sentimento", values = c("Positivo" = corpositivo, "Negativo" = cornegativo))+
		   geom_text(size = 3, col = "white", aes(x = reorder(Temas, Porcentagem), y = if_else(todos$Sentimento == "Negativo", 10, 90), label = paste(as.character(round(Porcentagem,2)),"%",sep=""), hjust = 0)) +
		   ylab("% de Comentários") + xlab("Temas") + 
			coord_flip()
   }

   plotGenero = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      file$Genero[which(is.na(file$Genero))] <- "Não declarado"
      file %>% 
         dplyr::select(Genero, Polaridade) %>%
         group_by(Genero) %>% 
         count(Genero, Polaridade) %>%
         arrange(n, Genero) %>%
         tail(50) %>% 
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = Genero, y = as.numeric(n), fill = toupper(Polaridade))
         ) + 
         ylab("Número de comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("POSITIVO" = corpositivo, "NEGATIVO" = cornegativo, "NEUTRO" = corneutro)) +
         #   geom_text( aes (x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), label = as.numeric(n) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotPalavras = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$Conteúdo)
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 20)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = "magenta") + 
         ylab("Número de ocorrências") +
         xlab("") +
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotPalavrasNegativas = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$Conteúdo[which(toupper(file$Polaridade) == "NEGATIVO")])
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 20)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = cornegativo) + 
         ylab("Número de ocorrências") +
         xlab("") +
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }   
   
   plotPalavrasPositivas = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$Conteúdo[which(toupper(file$Polaridade) == "POSITIVO")])
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 20)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = corpositivo) + 
         ylab("Número de ocorrências") +
         xlab("") +
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotPalavrasNeutras = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$Conteúdo[which(toupper(file$Polaridade) == "NEUTRO")])
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 20)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = corneutro) + 
         ylab("Número de ocorrências") +
         xlab("") +
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }

   plotWordcloud = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      text <- toupper(file$Conteúdo)
      mydfm <- getDFMatrix(text);
      set.seed(100)
      if(dim(mydfm)[1] <= 500){
         textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                            rot.per = .25, 
                            colors = RColorBrewer::brewer.pal(9,"Dark2")[4:8])        
      }
      else{
         textplot_wordcloud(mydfm[1:500], min.freq = 3, random.order = FALSE,
                            rot.per = .25, 
                            colors = RColorBrewer::brewer.pal(9,"Dark2")[4:8])        
      }
      
   }
      
   plotWordcloudNegativo = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
     text <- toupper(file$Conteúdo[which(toupper(file$Polaridade) == "NEGATIVO")])
     mydfm <- getDFMatrix(text);
     set.seed(100)
     if(dim(mydfm)[1] <= 500){
        textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Reds")[4:8])        
     }
     else{
        textplot_wordcloud(mydfm[1:500], min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Reds")[4.8])        
     }
     
  }
  
  plotWordcloudPositivo = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
     text <- toupper(file$Conteúdo[which(toupper(file$Polaridade) == "POSITIVO")])
     mydfm <- getDFMatrix(text);
     set.seed(100)
     if(dim(mydfm)[1] <= 500){
        textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Blues")[4:8])        
     }
     else{
        textplot_wordcloud(mydfm[1:500], min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Blues")[4:8])        
     }
     
  }
  
  plotWordcloudNeutro = function(){
     filepath <- input$file$datapath
     file <- read_xlsx(filepath)
     text <- toupper(file$Conteúdo[which(toupper(file$Polaridade) == "NEUTRO")])
     mydfm <- getDFMatrix(text);
     set.seed(100)
     if(dim(mydfm)[1] <= 500){
        textplot_wordcloud(mydfm, min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Greens")[4:8])        
     }
     else{
        textplot_wordcloud(mydfm[1:500], min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Greens")[4:8])        
     }
     
  }
  
  ###### Outputs
  
  output$indicesentimentos = downloadHandler(
    filename = 'indicesentimentos.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 16, height = 9,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotIndiceSentimentos(), device = device)
    })

  output$variabilidade = downloadHandler(
     filename = function() {
        paste("variabilidade.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = 16, height = 9,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotVariabilidade(), device = device)
        
     }
  )

  output$comentaristaspopulares = downloadHandler(
     filename = function() {
        paste("comentaristaspopulares.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotComentaristasPopulares(), device = device)
        
     }     
  )
    
  output$detratoresapoiadores = downloadHandler(
     filename = function() {
        paste("detratoresapoiadores.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotDetratoresApoiadores(), device = device)
        
     }     
  )
  
  output$detratorescurtidos = downloadHandler(
     filename = function() {
        paste("detratorescurtidos.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotDetratoresCurtidos(), device = device)
        
     }     
  )
  
  output$detratoresassiduos = downloadHandler(
     filename = function() {
        paste("detratoresassiduos.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotDetratoresAssiduos(), device = device)
        
     }     
  )
  
  
  output$apoiadorescurtidos = downloadHandler(
     filename = function() {
        paste("apoiadorescurtidos.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotApoiadoresCurtidos(), device = device)
        
     }     
  )
  
  output$apoiadoresassiduos = downloadHandler(
     filename = function() {
        paste("apoiadoresassiduos.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotApoiadoresAssiduos(), device = device)
        
     }     
  )
  
  output$palavrasapoiadores = downloadHandler(
     filename = function() {
        paste("palavrasapoiadores.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotPalavrasApoiadores(), device = device)
        
     }     
  )
  
  output$palavrasdetratores = downloadHandler(
     filename = function() {
        paste("palavrasdetratores.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotPalavrasDetratores(), device = device)
        
     }     
  )
  
  output$genero = downloadHandler(
     filename = function() {
        paste("genero.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotGenero(), device = device)
        
     }     
  )
  
  output$preditores = downloadHandler(
     filename = function() {
        paste("preditores.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotPreditores(), height=9,width=16,dpi=300)
        
     }     
  )

  output$difpreditores = downloadHandler(
     filename = function() {
        paste("difpreditores.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotDifPreditores(), height=9,width=16,dpi=300)
        
     }     
  )


  output$serietemporal = downloadHandler(
     filename = function() {
        paste("serietemporal.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = 16, height = 9,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotSerieTemporal(), device = device)
        
     }     
  )
  
  output$palavras = downloadHandler(
     filename = function() {
        paste("palavras.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotPalavras(), device = device)
        
     }
  )
  
  output$palavrasnegativas = downloadHandler(
     filename = function() {
        paste("listadepalavrasnegativas.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotPalavrasNegativas(), device = device)
        
     }
  )
  
  output$palavraspositivas = downloadHandler(
     filename = function() {
        paste("listadepalavraspositivas.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotPalavrasPositivas(), device = device)
        
     }
  )

  output$palavrasneutras = downloadHandler(
     filename = function() {
        paste("listadepalavrasneutras.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotPalavrasNeutras(), device = device)
        
     }
  )
  
  output$wordcloud = downloadHandler(
     filename = function() {
        paste("wordcloud.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotWordcloud(), device = device)
        
     }     
  )
  
  output$wordcloudnegativo = downloadHandler(
     filename = function() {
        paste("wordcloudnegativo.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotWordcloudNegativo(), device = device)
        
     }     
  )
  
  output$wordcloudpositivo = downloadHandler(
     filename = function() {
        paste("wordcloudpositivo.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotWordcloudPositivo(), device = device)
        
     }     
  )
  
  output$wordcloudneutro = downloadHandler(
     filename = function() {
        paste("wordcloudneutro.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotWordcloudNeutro(), device = device)
        
     }     
  )
  
  output$plotpostsporsentimento = downloadHandler(
     filename = function() {
        paste("postsporsentimento.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotPostsPorSentimento(), device = device)
        
     }     
     
  )
  
  output$curtidascomentarios = downloadHandler(
     filename = function() {
        paste("curtidascomentarios.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotCurtidasComentarios(), device = device)
        
     }     
  )
  
  output$melhorpiorposts = downloadHandler(
     filename = function() {
        paste("melhorpiorposts.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
                          res = 300, units = "in")
        }
        ggsave(file, plot = plotMelhorPiorPosts(), device = device)
        
     }     
  )
  
  

}
