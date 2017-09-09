library(shiny)
library(plotrix)
library(stringr)
library(quanteda)
library(readtext)
library(tm)
library(stringr)
library(dplyr)
library(wordcloud)

badwords <- c("scontent.xx.fbcdn.net","https","oh","oe","pra"," v ","como","para","de","do","da","das","dos","isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela","aquelas","que","q","é","sr","governador","rui","costa","senhor")

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

function(input, output) {
   plotVariabilidade = function() {
      filepath <- input$file$datapath
      dat <- read_xlsx(filepath)
      datasPosts <- as.Date(dat$Data,format="%d/%m/%Y")
      uniqueDataPosts <- unique(sort(datasPosts))
      mymatrix <- list();
      for(idata in 1:length(uniqueDataPosts)){
         data <- uniqueDataPosts[idata];
         posData <- which(datasPosts == data)
         idPostsData <- dat$`ID do Post`[posData]
         uniqueIdsPostsData <- unique(sort(idPostsData))
         for(iposts in 1:length(uniqueIdsPostsData)){
            idPost <- uniqueIdsPostsData[iposts]
            polaridadePostData <- dat$Polaridade[which(dat$`ID do Post`[posData] == idPost)]
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

}
