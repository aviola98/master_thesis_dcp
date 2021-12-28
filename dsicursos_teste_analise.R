#joining the 2 bozo discourses databases
library(tidyverse)
library("tidylog")
#install.packages("dplyr")
library(dplyr)
#install.packages("rvest")
library(rvest)
#install.packages("stringr")
library(stringr)
#install.packages("readr")
library(readr)
# install.packages("tidytext")
library(tidytext)
# install.packages("textstem")
library(textstem)
# install.packages("lexiconPT")
library(lexiconPT)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("tm")
library(tm)
#install.packages("DT")
library(DT)
#install.packages("topicmodels")
library(topicmodels)
#install.packages("reshape2")
library(reshape2)
#install.packages("broom")
library(broom)
#install.packages("knitr")
library(knitr)

bolsonaro_discursos <- read_csv("bolsonaro_discursos.csv")

#tokenizando os discursos e armazenando o dataframe tokenizado em um novo banco "bolsonaro_palavras"
bolsonaro_palavras <-
  bolsonaro_discursos %>%
  unnest_tokens(palavra,transcricao,strip_numeric=T)


#Tirando as stopwrods

#criando o banco de stopwords 
stopwords <- get_stopwords(language="pt") %>%
  rename(palavra=word) %>%
  add_row(palavra = c("voces","voce","todos","nao","2020","deixo","deixar","ha","pouco","e","senhora","senhores","senhor","aqui","presidente","ser","la","prezado","ministro","vez","entao","ter","porque"), lexicon="pessoal")

#realizando o anti_join para retirar as stopwords
bolsonaro_palavras <-
  bolsonaro_palavras %>%
  anti_join(stopwords,by="palavra")

#Gráfico da frequência de palavras nos discursos de bolsonaro
frequencia_palavras_bolsonaro<-
  bolsonaro_palavras %>%
  count(palavra,sort=T) %>%
  mutate(palavra=fct_reorder(palavra,n)) %>%
  slice(1:20) %>%
  ggplot(aes(palavra,n))+
  geom_col(fill="red")+
  coord_flip()+
  labs(x="")+
  theme_minimal() +
  xlab("Palavras")+
  ylab("Frequência Palavras")+
  ggtitle("Palavras mais frequentes nos discursos de Bolsonaro")

frequencia_palavras_bolsonaro

#realizando um trigram para ver quais palavras aparecem mais frequentemente juntas 

bozo_trigram<-
  bolsonaro_discursos %>%
  unnest_tokens(trigram,transcricao, token="ngrams",n=3) %>%
  #retirando as stopwords
  separate(trigram,c("palavra1","palavra2","palavra3"), sep=" ") %>%
  anti_join(stopwords,by=c("palavra1"="palavra"))%>%
  anti_join(stopwords,by=c("palavra2"="palavra"))%>%
  anti_join(stopwords,by=c("palavra3"="palavra")) %>%
  unite("trigram", c(palavra1,palavra2,palavra3),sep=" ",remove=F)

#vendo os trigramas mais frequentes
bozo_trigram_grafico <-
  bozo_trigram %>%
  count(trigram,sort=T) %>%
  mutate(trigram=fct_reorder(trigram,n)) %>%
  slice(1:20) %>%
  ggplot(aes(trigram,n))+
  geom_col(fill="red")+
  coord_flip()+
  labs(x="")+
  theme_minimal() +
  xlab("Trigramas")+
  ylab("Frequência Trigramas")+
  ggtitle("Trigrams mais frequentes nos discursos de Bolsonaro")
bozo_trigram_grafico

library(igraph)
library(ggraph)

#para realizar um igraph irei utilizar um bigrama para facilitar a captação de combinações mais comuns entre as palavras

#criando um bigrama
bolsonaro_bigrama <- bolsonaro_discursos %>%
  unnest_tokens(bigram,transcricao, token="ngrams",n=2) %>%
  #retirando as stopwords
  separate(bigram,c("palavra1","palavra2"), sep=" ") %>%
  anti_join(stopwords,by=c("palavra1"="palavra"))%>%
  anti_join(stopwords,by=c("palavra2"="palavra"))%>%
  unite("bigram", c(palavra1,palavra2),sep=" ",remove=F)

#contando as palavras

bigram_counts <- bolsonaro_bigrama %>%
  count(palavra1,palavra2,sort=T)

#filtrando o gráfico somente para combinações comuns
bi_graph <- bigram_counts %>%
  filter(n>100) %>%
  graph_from_data_frame()

#gerando uma combinação aleatória
set.seed(2017)

#construindo uma seta "a" 
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bi_graph, layout="fr")+
  #adicionando a estética edge_alpha para fazer com que as setas fiquem transparentes com base no quão comuns ou raros são os bigramas
  geom_edge_link(aes(edge_alpha=n),show.legend=F,
                 #adicionando a seta e o "end_cap" sinalizando que a seta termina antes de tocar o nó
                 arrow=a,end_cap=circle(.07,"inches")) +
  #adicionando fatores estéticos no nó como cor e tamanho 
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name),vjust = 1, hjust = 1) +
  #adicionando um tema que melhor combina com esse tipo de gráfico
  theme_void() +
  ggtitle("Relação entre palavras nos discursos selecionados de Bolsonaro")

