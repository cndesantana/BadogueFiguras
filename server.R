library(shiny)
library(plotrix)

library(tm)
library(stringr)
library(dplyr)

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

plotPalavras <- function(text, sentimento){

   mycol <- switch(sentimento, 
          geral = "gray50", 
          negativo = "red",
          positivo = "green",
          neutro = "blue")
   d <- getMatrizDeOcorrencias(text)
   nwords <- ifelse(length(as.character(d[,1])) > 10, 10, length(as.character(d[,1])))
   d <- d[1:nwords,]

   p <- ggplot(d, aes(x = reorder(word, freq), y = freq)) + 
      geom_bar( stat = "Identity" , fill = mycol) +
      geom_text( aes (label = freq ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
      xlab(NULL) + 
      ylab("Número de Ocorrências") + 
      coord_flip() 
   print(p)
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

function(input, output) {
  plotIndiceSentimentos = function() {
    filepath <- input$file$datapath
    file <- read_xlsx(filepath)
    allpolaridade <- toupper(file$Polaridade)
    isent <- getIndiceDeSentimento(allpolaridade);

    plot(-10:10,axes=FALSE,xlab="",ylab="",type="n")
    gradient.rect(1,-10,3,10,reds=c(1,0), greens=c(seq(0,1,length=10),seq(1,0,length=10)),blues=c(0,1),gradient="y")
    text(x = 3, y=signif(isent,2), labels = paste(intToUtf8(9664),signif(isent,2)),pos = 4)
    text(x = 0, y=10,  labels = 1,pos = 4)
    text(x = 0, y=-10, labels = -1,pos = 4)

#    qplot(speed, dist, data = cars)
  }
  output$indicesentimentos = downloadHandler(
    filename = 'indicesentimentos.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotIndiceSentimentos(), device = device)
    })

### Tratando a aba sobre lista de palavras
  plotListaPalavras = function() {
    filepath <- input$file$datapath
    file <- read_xlsx(filepath)
    allcomentarios <- file$Conteúo
    plotPalavras(allcomentarios,"geral");
  #  d <- getMatrizDeOcorrencias(allcomentarios);
  
  #  ggplot(d, aes(x = reorder(word, freq), y = freq)) + 
  #        geom_bar( stat = "Identity" , fill = mycol) +
  #        geom_text( aes (label = freq ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
  #        xlab(NULL) + 
  #        ylab("Número de Ocorrências") + 
  #        coord_flip()
  #  plot(hist(as.numeric(d$freq))); 
  
  }
  output$palavras = downloadHandler(
    filename = 'palavras.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotListaPalavras(), device = device)
    })

### Tratando a aba sobre o wordcloud
  plotWordcloud = function() {
    plot(1:1000,log(1:1000));
  }
  output$wordcloud = downloadHandler(
    filename = 'wordcloud.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotWordcloud(), device = device)
    })

}
