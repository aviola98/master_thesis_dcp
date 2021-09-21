library(tidyverse)
library(rvest)
library(stringr)
library(tidytext)
library(lexiconPT)
library(tm)
library(DT)
library(igraph)
library(ggraph)
library(wordcloud)
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)

#abrindo arquivo

ONU_disc <- read_csv("ONU_disc.csv")

#realizando um for loop com o intuito de extrair os discursos dos links

#criando um tibble vazio para armazenar os discursos

ONU_discursos <- tibble()

#for loop

#OBS: pelo fato de estar realizando uma leitura dos links , o loop pode demorar por volta de 2 minutos para rodar 

for (link in ONU_disc$link_disc){
  
  #lendo o html do link da página
  pagina <- read_html(link)
  #lendo o node do titulo com html_nodes através de seu xpath
  node_titulo <- html_nodes(pagina, xpath = "//h1[@class = 'documentFirstHeading']")
  #extraindo o titulo através de seu node e retirando caractéres estranhos com str_squish
  titulo <- html_text(node_titulo) %>%
    str_squish()
  
  #lendo o node do discurso através da função html_nodes e de seu xpath
  node_discurso <- html_nodes(pagina, xpath="//div[@id = 'parent-fieldname-text']")
  #extraindo o discurso através do seu node e retirando os caractéres estranhos
  discurso <- html_text(node_discurso) %>%
    str_squish()
  discurso <- discurso[discurso!=""]
  
  #criando uma tabela com os titulos e seus respectivos discursos
  tabela_discursos <- tibble(titulo, discurso)
  
  #juntando a tabela_discursos com o banco ONU_discursos através da função bind_rows
  ONU_discursos <- bind_rows(ONU_discursos, tabela_discursos)
  
}

View(ONU_discursos)

#retirando data

ONU_discursos <- 
  ONU_discursos %>%
  mutate(discurso_separado = str_split(discurso, "09/21/21")) %>%
  mutate(discurso_separado_segundo = map_chr(discurso_separado, tail,n=1)) %>%
  unnest(discurso_separado_segundo) %>%
  select("titulo","discurso_separado_segundo") %>%
  rename(discurso = discurso_separado_segundo)

#tokenizando

ONU_palavras <- 
  ONU_discursos %>%
  unnest_tokens(palavra,discurso,strip_numeric = T)

#tirando stop_words


#criando o banco de stopwords 
stopwords <- get_stopwords(language="pt") %>%
  rename(palavra=word) %>%
  add_row(palavra = c("é","senhora","senhores","senhor","aqui","presidente","ser","lá","prezado","ministro","vez","então","ter","vossa","excelência"), lexicon="pessoal")

#realizando o anti_join para retirar as stopwords
ONU_palavras <-
  ONU_palavras %>%
  anti_join(stopwords,by="palavra")

#igraph todos

ONU_bigrama <- ONU_discursos %>%
  unnest_tokens(bigram, discurso,token="ngrams",n=2)%>%
  separate(bigram,c("palavra1","palavra2"), sep=" ") %>%
  anti_join(stopwords,by=c("palavra1"="palavra"))%>%
  anti_join(stopwords,by=c("palavra2"="palavra"))%>%
  unite("bigram", c(palavra1,palavra2),sep=" ",remove=F)

#contando palavras

ONU_counts <- ONU_bigrama %>%
  count(palavra1,palavra2)

#filtrando somente combinações comuns

ONU_graph <- ONU_counts %>%
  filter(n>2) %>%
  graph_from_data_frame()

set.seed(2017)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ONU_rel <-
ggraph(ONU_graph, layout="fr")+
  #adicionando a estética edge_alpha para fazer com que as setas fiquem transparentes com base no quão comuns ou raros são os bigramas
  geom_edge_link(aes(edge_alpha=n),show.legend=F,
                 #adicionando a seta e o "end_cap" sinalizando que a seta termina antes de tocar o nó
                 arrow=a,end_cap=circle(.07,"inches")) +
  #adicionando fatores estéticos no nó como cor e tamanho 
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name),vjust = 1, hjust = 1) +
  #adicionando um tema que melhor combina com esse tipo de gráfico
  theme_void() +
  ggtitle("Relação entre palavras nos discursos de Bolsonaro na ONU (2019-2021)")

ONU_rel

#igraph 2019

disc_74 <- ONU_discursos %>%
  slice(1)

ONU2019_bigrama <- disc_74 %>%
  unnest_tokens(bigram, discurso,token="ngrams",n=2)%>%
  separate(bigram,c("palavra1","palavra2"), sep=" ") %>%
  anti_join(stopwords,by=c("palavra1"="palavra"))%>%
  anti_join(stopwords,by=c("palavra2"="palavra"))%>%
  unite("bigram", c(palavra1,palavra2),sep=" ",remove=F)

#contando palavras

ONU2019_counts <- ONU2019_bigrama %>%
  count(palavra1,palavra2)

#filtrando somente combinações comuns

ONU2019_graph <- ONU2019_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

set.seed(2017)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ONU2019_rel <-
ggraph(ONU2019_graph, layout="fr")+
  #adicionando a estética edge_alpha para fazer com que as setas fiquem transparentes com base no quão comuns ou raros são os bigramas
  geom_edge_link(aes(edge_alpha=n),show.legend=F,
                 #adicionando a seta e o "end_cap" sinalizando que a seta termina antes de tocar o nó
                 arrow=a,end_cap=circle(.07,"inches")) +
  #adicionando fatores estéticos no nó como cor e tamanho 
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name),vjust = 1, hjust = 1) +
  #adicionando um tema que melhor combina com esse tipo de gráfico
  theme_void() +
  ggtitle("Relação entre palavras no discurso de Bolsonaro na abertura da 74ª Assembleia da ONU")

ONU2019_rel

#igraph 2020

disc_75 <- ONU_discursos %>%
  slice(2)

ONU2020_bigrama <- disc_75 %>%
  unnest_tokens(bigram, discurso,token="ngrams",n=2)%>%
  separate(bigram,c("palavra1","palavra2"), sep=" ") %>%
  anti_join(stopwords,by=c("palavra1"="palavra"))%>%
  anti_join(stopwords,by=c("palavra2"="palavra"))%>%
  unite("bigram", c(palavra1,palavra2),sep=" ",remove=F)

#contando palavras

ONU2020_counts <- ONU2020_bigrama %>%
  count(palavra1,palavra2)

#filtrando somente combinações comuns

ONU2020_graph <- ONU2020_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

set.seed(2017)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ONU2020_rel <-
ggraph(ONU2020_graph, layout="fr")+
  #adicionando a estética edge_alpha para fazer com que as setas fiquem transparentes com base no quão comuns ou raros são os bigramas
  geom_edge_link(aes(edge_alpha=n),show.legend=F,
                 #adicionando a seta e o "end_cap" sinalizando que a seta termina antes de tocar o nó
                 arrow=a,end_cap=circle(.07,"inches")) +
  #adicionando fatores estéticos no nó como cor e tamanho 
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name),vjust = 1, hjust = 1) +
  #adicionando um tema que melhor combina com esse tipo de gráfico
  theme_void() +
  ggtitle("Relação entre palavras no discurso de Bolsonaro na abertura da 75ª Assembleia da ONU")

ONU2020_rel

#igraph 2021

disc_76 <- ONU_discursos %>%
  slice(3)

ONU2021_bigrama <- disc_76 %>%
  unnest_tokens(bigram, discurso,token="ngrams",n=2)%>%
  separate(bigram,c("palavra1","palavra2"), sep=" ") %>%
  anti_join(stopwords,by=c("palavra1"="palavra"))%>%
  anti_join(stopwords,by=c("palavra2"="palavra"))%>%
  unite("bigram", c(palavra1,palavra2),sep=" ",remove=F)

#contando palavras

ONU2021_counts <- ONU2021_bigrama %>%
  count(palavra1,palavra2)

#filtrando somente combinações comuns

ONU2021_graph <- ONU2021_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

set.seed(3000)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ONU2021_graph <-
ggraph(ONU2021_graph, layout="fr")+
  #adicionando a estética edge_alpha para fazer com que as setas fiquem transparentes com base no quão comuns ou raros são os bigramas
  geom_edge_link(aes(edge_alpha=n),show.legend=F,
                 #adicionando a seta e o "end_cap" sinalizando que a seta termina antes de tocar o nó
                 arrow=a,end_cap=circle(.07,"inches")) +
  #adicionando fatores estéticos no nó como cor e tamanho 
  geom_node_point(color="lightblue",size=5)+
  geom_node_text(aes(label=name),vjust = 1, hjust = 1) +
  #adicionando um tema que melhor combina com esse tipo de gráfico
  theme_void() +
  ggtitle("Relação entre palavras no discurso de Bolsonaro na abertura da 76ª Assembleia da ONU")

ONU2021_graph

#palavras mais frequentes 

ONU_freq <-
  ONU_palavras %>%
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
  ggtitle("Palavras mais frequentes nos discursos de Bolsonaro nas Assembleias da ONU")

ONU_freq

#2019
ONU2019_freq <- 
  ONU_discursos %>%
  slice(1) %>%
  unnest_tokens(palavra,discurso,strip_numeric=T) %>%
  anti_join(stopwords,by="palavra") %>%
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
  ggtitle("Palavras mais frequentes no discurso de Bolsonaro na 74ª Assembleia da ONU")

ONU2019_freq


#2020
ONU2020_freq <- 
  ONU_discursos %>%
  slice(2) %>%
  unnest_tokens(palavra,discurso,strip_numeric=T) %>%
  anti_join(stopwords,by="palavra") %>%
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
  ggtitle("Palavras mais frequentes no discurso de Bolsonaro na 75ª Assembleia da ONU")

ONU2020_freq

#2021
ONU2021_freq <- 
  ONU_discursos %>%
  slice(3) %>%
  unnest_tokens(palavra,discurso,strip_numeric=T) %>%
  anti_join(stopwords,by="palavra") %>%
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
  ggtitle("Palavras mais frequentes no discurso de Bolsonaro na 76ª Assembleia da ONU") +
  labs(caption = "a palavra 'us' está para US dollars")

ONU2021_freq

#nuvem de palavra

ONU_palavras <-
ONU_palavras %>%
  count(palavra) %>%
  #paste("estados","unidos", sep=" ") %>%
  #paste("direitos","humanos",sep=" ")

set.seed(1234)

wordcloud(words  = ONU_palavras$palavra,
          freq = ONU_palavras$n,
          min.freq = 1,
          max.words = 100,
          scale=c(3,0.4) ,
          random.order = F,
          rot.per = 0.35,
          colors = brewer.pal(8,"Dark2"))

set.seed(1234)

wordcloud2(data = ONU_palavras,
           size = 1.6,
           color='random-dark')

set.seed(1234)

wordcloud2(data=ONU_palavras, size = 0.7, shape = 'pentagon')
