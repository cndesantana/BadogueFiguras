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
library(raster)
library(jpeg)


badwords <- c("scontent.xx.fbcdn.net","https","oh","oe","pra","v","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que","q","é","sr","governador","rui","costa","senhor")

getScreenshot <- function(link1, filename1){
   workdir <- "figures_badogue"
   if(file.exists(workdir)){
      resolution <- "1366x768"
      
      outputfile1 <- file.path(workdir,filename1)
      command1 = paste("pageres ", link1, " ", resolution, " --format='jpg' --verbose --sector='.contentArea' --filename=",outputfile1,sep="")
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
      
      mymatrix %>% 
         select(id, sentimento, ncomentarios) %>% 
         group_by(id, sentimento, ncomentarios) %>% 
         arrange(sentimento, ncomentarios) %>%
         filter(ncomentarios > 100) %>%
         tail(60) %>%
         ggplot() + 
         geom_bar(stat="identity", aes(x=reorder(as.character(id),as.numeric(sentimento)),y=as.numeric(sentimento))) + 
         scale_fill_manual("red") +
         xlab("") + ylab("Sentimento dos Posts") + 
         geom_text( aes(x=reorder(as.character(id),as.numeric(sentimento)),y=as.numeric(sentimento), label = signif(as.numeric(sentimento),2) ) , vjust = 0, hjust = 0, size = 3 ) +
         coord_flip()
      
   }

   plotIndiceSentimentos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      allpolaridade <- toupper(file$Polaridade)
      isent <- getIndiceDeSentimento(allpolaridade);
      
      plot(-10:10,axes=FALSE,xlab="",ylab="",type="n")
      gradient.rect(1,-10,3,10,reds=c(1,0), greens=c(seq(0,1,length=10),seq(1,0,length=10)),blues=c(0,1),gradient="y")
      text(x = 3, y=10*signif(isent,2), labels = paste(intToUtf8(9664),signif(isent,2)),pos = 4)
      text(x = 0, y=10,  labels = 1,pos = 4)
      text(x = 0, y=-10, labels = -1,pos = 4)
      
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
      ggplot(mymatrix, aes(x=ymd(Data),y=as.numeric(sentimento))) + geom_point(stat="identity") + 
         stat_smooth(method="loess", span=0.1, se=TRUE, aes(Sombra="Desvio"), alpha=0.3) +
         theme_bw() +
         xlab("Data") + ylab("Sentimento dos Posts")
   }
   
   
   plotCurtidasComentarios = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         select(`Autor ID`, Polaridade, Comentários, Curtidas) %>%
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
         select(`Autor ID`, Curtidas, Polaridade) %>%
         group_by(`Autor ID`) %>% 
         count(`Autor ID`, Curtidas, Polaridade) %>%
         arrange(n, `Autor ID`) %>%
         tail(50) %>% 
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), fill = Polaridade)
         ) + 
         ylab("Numero de Curtidas em Comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = ggplotColours(n=3)[2], "Negativo" = ggplotColours(n=3)[1], "Neutro" = ggplotColours(n=3)[3])) +
         #   geom_text( aes (x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), label = as.numeric(n) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
      
   plotDetratoresApoiadores = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      file %>% 
         select(`Autor ID`, Polaridade) %>%
         group_by(`Autor ID`) %>% 
         count(`Autor ID`, Polaridade) %>%
         arrange(n, `Autor ID`) %>%
         tail(50) %>% 
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), fill = Polaridade)) + 
         ylab("Numero de comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = ggplotColours(n=3)[2], "Negativo" = ggplotColours(n=3)[1], "Neutro" = ggplotColours(n=3)[3])) +
         #   geom_text( aes (x = reorder(`Autor ID`,as.numeric(n)), y = as.numeric(n), label = as.numeric(n) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotDetratoresCurtidos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)

      file %>% 
         select(`Autor ID`, Polaridade, Curtidas) %>%
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
         ylab("Numero de Curtidas") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = ggplotColours(n=3)[2], "Negativo" = ggplotColours(n=3)[1], "Neutro" = ggplotColours(n=3)[3])) +
         geom_text( aes (x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotDetratoresAssiduos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         select(`Autor ID`, Polaridade) %>%
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
         ylab("Numero de Comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = ggplotColours(n=3)[2], "Negativo" = ggplotColours(n=3)[1], "Neutro" = ggplotColours(n=3)[3])) +
         geom_text( aes (x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }

   
   plotApoiadoresCurtidos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         select(`Autor ID`, Polaridade, Curtidas) %>%
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
         ylab("Numero de Curtidas") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = ggplotColours(n=3)[2], "Negativo" = ggplotColours(n=3)[1], "Neutro" = ggplotColours(n=3)[3])) +
         geom_text( aes (x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotApoiadoresAssiduos = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      file %>% 
         select(`Autor ID`, Polaridade) %>%
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
         ylab("Numero de Comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = ggplotColours(n=3)[2], "Negativo" = ggplotColours(n=3)[1], "Neutro" = ggplotColours(n=3)[3])) +
         geom_text( aes (x = reorder(`Autor ID`,as.numeric(total)), y = as.numeric(total), label = as.numeric(total) ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   }
   
   plotPalavrasDetratores = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      autores <- file %>% 
         select(`Autor ID`, Polaridade) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         arrange(`Autor ID`, Polaridade) %>%
         filter(Polaridade == "Negativo") %>%
         arrange(`Autor ID`) %>%
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         select(`Autor ID`)
      
      text <- file %>%
         filter(as.character(`Autor ID`) %in% autores$`Autor ID`, Polaridade == "Negativo") %>%
         select(Conteúdo) %>%
         toupper()
      
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 50)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = ggplotColours(n=3)[1]) + 
         ylab("Número de ocorrências") +
         xlab("") +
         geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()      
      
   }
   
   plotPalavrasApoiadores = function(){
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      
      autores <- file %>% 
         select(`Autor ID`, Polaridade) %>%
         group_by(`Autor ID`, Polaridade) %>% 
         arrange(`Autor ID`, Polaridade) %>%
         filter(Polaridade == "Positivo") %>%
         arrange(`Autor ID`) %>%
         summarise(total = n()) %>%
         arrange(total) %>%
         filter(total > 3) %>%
         tail(50) %>%
         select(`Autor ID`)
      
      text <- file %>%
         filter(as.character(`Autor ID`) %in% autores$`Autor ID`, Polaridade == "Positivo") %>%
         select(Conteúdo) %>%
         toupper()
      
      mydfm <- getDFMatrix(text);
      words_td <- topfeatures(mydfm, 50)
      ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
                  fill = ggplotColours(n=3)[2]) + 
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
      
      primeirodia <- min(df_datas$Data);
      ultimodia <- max(df_datas$Data)
      ggplot(df_datas, aes(x=dmy(Data), y=freq, fill=Polaridade)) +
         geom_bar(position = "stack", stat = "identity") +
         scale_x_date(date_breaks = "1 day",
                      labels = date_format("%d/%m/%Y")) +
         theme(text = element_text(size=6), axis.text.x = element_text(angle=90, hjust=1)) +
         scale_y_continuous(labels=scales::percent) +
         labs (title = "") +
         labs (x = "", y = "Porcentagem de Posts") +
         theme(text = element_text(size=6), axis.text.x = element_text(angle=45, hjust=1)) +
         scale_fill_discrete(name="Sentimento") +
         coord_cartesian(xlim = c(dmy(primeirodia), dmy(ultimodia))) +
         scale_fill_manual("Sentimentos", values = c("POSITIVO" = ggplotColours(n=4)[2], "NEGATIVO" = ggplotColours(n=4)[1], "NEUTRO" = ggplotColours(n=4)[3], "GOVERNO" = ggplotColours(n=4)[4])) +
         geom_text(size = 2, col = "white", aes(x = dmy(Data), y = getPositionY(df_datas), label = paste(as.character(100*round(df_datas$freq,2)),"%",sep="")));
      
   }
   
   
   plotGenero = function() {
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      file$Genero[which(is.na(file$Genero))] <- "Não declarado"
      file %>% 
         select(Genero, Polaridade) %>%
         group_by(Genero) %>% 
         count(Genero, Polaridade) %>%
         arrange(n, Genero) %>%
         tail(50) %>% 
         ggplot() + 
         geom_bar(stat = "identity", 
                  aes(x = Genero, y = as.numeric(n), fill = Polaridade)
         ) + 
         ylab("Numero de comentários") +
         xlab("") +
         scale_fill_manual("Polaridade", values = c("Positivo" = ggplotColours(n=3)[2], "Negativo" = ggplotColours(n=3)[1], "Neutro" = ggplotColours(n=3)[3])) +
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
         ylab("Numero de ocorrencias") +
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
                  fill = "red") + 
         ylab("Numero de ocorrencias") +
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
                  fill = "green") + 
         ylab("Numero de ocorrencias") +
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
                  fill = "green") + 
         ylab("Numero de ocorrencias") +
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
                            colors = RColorBrewer::brewer.pal(9,"Dark2")[5:9])        
      }
      else{
         textplot_wordcloud(mydfm[1:500], min.freq = 3, random.order = FALSE,
                            rot.per = .25, 
                            colors = RColorBrewer::brewer.pal(9,"Dark2")[5:9])        
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
                           colors = RColorBrewer::brewer.pal(9,"Reds")[5:9])        
     }
     else{
        textplot_wordcloud(mydfm[1:500], min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Reds")[5:9])        
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
                           colors = RColorBrewer::brewer.pal(9,"Greens")[5:9])        
     }
     else{
        textplot_wordcloud(mydfm[1:500], min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Greens")[5:9])        
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
                           colors = RColorBrewer::brewer.pal(9,"Blues")[5:9])        
     }
     else{
        textplot_wordcloud(mydfm[1:500], min.freq = 3, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(9,"Blues")[5:9])        
     }
     
  }
  
  ###### Outputs
  
  output$indicesentimentos = downloadHandler(
    filename = 'indicesentimentos.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
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
           grDevices::png(..., width = width, height = height,
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
  
  output$serietemporal = downloadHandler(
     filename = function() {
        paste("serietemporal.png", sep = "")
     },
     content = function(file) {
        device <- function(..., width, height) {
           grDevices::png(..., width = width, height = height,
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
