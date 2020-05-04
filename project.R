library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(tidytext)
library(rvest)
library(httr)
library(curl)
library(jsonlite)
library(dplyr)
library(pool)
library(modelr)
library(purrr)
library(broom)
library(tidyr)
library(wordcloud)
source("/Users/jane/Downloads/data\ wrangling/project/api-keys.R")
##----scrap the rank on IMDb site----
rank <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250" %>%
  read_html() %>% 
  html_table(fill = TRUE)%>%.[[1]]
names(rank)[names(rank)=="Rank & Title"] <- "film_title"
names(rank)[names(rank)=="IMDb Rating"] <- "imdb_rating"
rank <- rank[,2:3]
#tidy the data frame and keep title, year, rating and rank of IMDb
tidy_rank <-rank %>% 
  separate(film_title,c("rank","film_title","year"),sep="\n") %>%
  select(film_title,year,imdb_rating) %>%
  mutate(n_rank=min_rank(desc(imdb_rating)))
tidy_rank$year <- tidy_rank$year %>%
  str_replace_all("        ","") %>%
  str_replace_all("\\(","") %>%
  str_replace_all("\\)","") %>%
  as.numeric()
tidy_rank$film_title <- tidy_rank$film_title %>%
  str_replace_all("      ","")
head(tidy_rank)

tidy_rank %>%
  group_by(year) %>%
  summarise(total=n()) %>%
  arrange(desc(total))

##----omdb further information about imdb top 250 rating films----
omdb_title <- tidy_rank$film_title %>%
  str_replace_all("Taare Zameen Par","Like Stars on Earth")%>%
  str_replace_all("Capharnaüm","Capernaum")%>%
  str_replace_all("Rashômon","Rashomon")%>%
  str_replace_all("Babam ve Oglum","My Father and My Son")%>%
  str_replace_all("Relatos salvajes","Wild Tales")%>%
  str_replace_all("Kis Uykusu","Winter Sleep")%>%
  str_replace_all(" ","+")
  
omdb_result <- 0
for (i in 1:length(omdb_title)){
  url <- paste0(
    "http://www.omdbapi.com/?apikey=", 
    api.key.Xu.omdb,"&t=",omdb_title[i],"&y=&plot=short&r=json?")
  omdb_each <- url %>% curl() %>% readLines() %>%
    prettify() %>%
    fromJSON() %>%as.data.frame()
  omdb_result <- rbind(omdb_result,omdb_each)
}
omdb_imdb <- omdb_result %>%
  filter(Ratings.Source=="Internet Movie Database")
names(omdb_imdb)
#Which production company is popular
omdb_imdb %>%
  group_by(Production) %>%
  summarise(total=n()) %>%
  filter(total>5)%>%
  mutate(Production = reorder(Production, total))%>%
  ggplot(aes(Production, total)) + 
  geom_bar(stat = "identity") +
  xlab(NULL) + coord_flip() +
  ggtitle("common production in IMDb top 250 rating")
##average rating of company
omdb_imdb$Ratings.Value <-omdb_imdb$Ratings.Value %>%
  str_replace_all("\\/10","") %>%
  as.numeric()
omdb_imdb %>%
  group_by(Production) %>%
  summarise(mean=mean(Ratings.Value)) %>%
  arrange(desc(mean))
#Which genre is popular
tidy_genre<- omdb_imdb %>%
  separate(Genre,into=c("1","2","3","4","5","6"),sep=", ") %>%
  gather("1":"6",key=n_genre,value=genre)%>%
  na.omit(genre)
##average ratins points of genre
tidy_genre %>%
  group_by(genre) %>%
  summarise(total=n()) %>%
  arrange(desc(total))
tidy_genre %>%
  group_by(genre) %>%
  summarise(mean=mean(Ratings.Value)) %>%
  arrange(mean)
tidy_genre %>% 
  group_by(genre) %>%
  summarise(total=n()) %>%
  mutate(genre = reorder(genre, total))%>%
  ggplot(aes(genre, total)) + 
  geom_bar(stat = "identity") +
  xlab(NULL) + coord_flip() +
  ggtitle("most common genre in IMDb top 250 Rating")

#which country is popular
tidy_country<- omdb_imdb %>%
  separate(Country,into=c("1","2","3","4","5","6"),sep=", ") %>%
  gather("1":"6",key=n_country,value=country)%>%
  na.omit(country)
tidy_country %>%
  group_by(country) %>%
  summarise(total=n()) %>%
  arrange(desc(total))
tidy_country %>% 
  group_by(country) %>%
  summarise(total=n()) %>%
  mutate(country = reorder(country, total))%>%
  ggplot(aes(country, total)) + 
  geom_bar(stat = "identity") +
  xlab(NULL) + coord_flip() +
  ggtitle("most common country in IMDb top 250 Rating")
#famous actor/actress
tidy_act<- omdb_imdb %>%
  separate(Actors,into=c("1","2","3","4","5","6"),sep=", ") %>%
  gather("1":"4",key=n_actor,value=actor)
tidy_act %>% 
  group_by(actor) %>%
  summarise(total=n()) %>%
  mutate(actor = reorder(actor, total))%>%
  filter(total>3)%>%
  ggplot(aes(actor, total)) + 
  geom_bar(stat = "identity") +
  xlab(NULL) + coord_flip() +
  ggtitle("most common actor/actress in IMDb top 250 Rating")

#relation between rating value and runtime
tidy_relation <- omdb_imdb %>%
  select(Title, Year, Runtime, imdbRating, BoxOffice)
tidy_relation$Runtime <- tidy_relation$Runtime %>%
  str_replace_all(" min","")%>%
  as.numeric()

tidy_relation %>%
  ggplot(aes(imdbRating,Runtime)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 5,angle = 90))+
  ggtitle("boxplot between rating and runtime")

#relation between rating and boxoffice
tidy_relation <- tidy_relation %>%
  filter(BoxOffice!="N/A")
tidy_relation$BoxOffice <- tidy_relation$BoxOffice %>%
  str_replace_all(",","") %>%
  str_replace_all("\\$","") %>%
  as.numeric()
tidy_relation %>%
  ggplot(aes(imdbRating,BoxOffice)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 5,angle = 90))+
  ggtitle("boxplot between rating and boxoffice")
tidy_relation%>%filter(BoxOffice == max(tidy_relation$BoxOffice))
tidy_relation %>% 
  mutate(rank=min_rank(desc(BoxOffice)))%>%
  arrange(rank)%>%
  head()
tidy_relation %>% 
  mutate(rank=min_rank(BoxOffice))%>%
  arrange(rank)%>%
  head()
 
##----new york times movie reviews----
#get the information about critics of new york times
url2 <- 
  paste0(
  "https://api.nytimes.com/svc/movies/v2/critics/all.json?api-key=",api.key.Xu.nytimes)
critics<- url2%>% curl() %>% readLines() %>%
  prettify() %>%
  fromJSON()%>%as.data.frame() 

url3 <- 
  paste0(
    "https://api.nytimes.com/svc/movies/v2/reviews/picks.json?api-key=",api.key.Xu.nytimes)
pick<- url3%>% curl() %>% readLines() %>%
  prettify() %>%
  fromJSON()%>%as.data.frame() 

#specifically choose film "dark knight"
url4 <- 
  paste0(
    "https://api.nytimes.com/svc/movies/v2/reviews/search.json?query=dark%20knight&api-key=",api.key.Xu.nytimes)
dark_knight<- url4%>% curl() %>% readLines() %>%
  prettify() %>%
  fromJSON()%>%as.data.frame()
dark_knight <- dark_knight %>%
  filter(results.display_title == "The Dark Knight")
dark_knight
dark_knight$results.link$url
#analysis the article
article <- dark_knight$results.link$url %>%
  read_html() %>%
  html_nodes("p") %>% html_text()
article.tbl <- tibble(text=article) %>%
  filter(text !="Advertisement")
article.tbl <-article.tbl[3:14,1]
tidy_article <-article.tbl %>%
  unnest_tokens(word,text)
data("stop_words")
tidy_article %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%  
  filter(n >= 4) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + 
  geom_bar(stat = "identity") +
  xlab(NULL) + coord_flip() +
  ggtitle("most common non-stop-words in article")

tidy_article %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%  
  filter(n >= 4) %>%
  mutate(word = reorder(word, n)) %>%
  with(wordcloud(word, n, max.words = 100))
#simple sentiment analysis of this article
tidy_sentiment <- article.tbl %>%
  mutate(paranum = row_number()) %>%
  unnest_tokens(word,text)
darkknightsentiment <- tidy_sentiment %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = paranum, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
ggplot(darkknightsentiment,aes(index, sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  ggtitle("sentiment analysis of dark knight review")
article.tbl[7,1][[1]]

#adding dark knight rises
dark_knight_rise<- url4%>% curl() %>% readLines() %>%
  prettify() %>%
  fromJSON()%>%as.data.frame()
dark_knight_rise<- dark_knight_rise %>%
  filter(results.display_title != "The Dark Knight")
dark_knight_rise$results.link$url
article1 <- dark_knight_rise$results.link$url %>%
  read_html() %>%
  html_nodes("p") %>% html_text()
article.tbl1 <- tibble(text=article1) %>%
  filter(text !="Advertisement")
article.tbl <-article.tbl[3:19,1]

tidy_article1 <-article.tbl1 %>%
  unnest_tokens(word,text)
tidy_article1 %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%  
  filter(n >= 4) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + 
  geom_bar(stat = "identity") +
  xlab(NULL) + coord_flip() +
  ggtitle("most common non-stop-words in article")
tidy_article1 %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%  
  filter(n >= 4) %>%
  mutate(word = reorder(word, n)) %>%
  with(wordcloud(word, n, max.words = 100))

tidy_sentiment1 <- article.tbl1 %>%
  mutate(paranum = row_number()) %>%
  unnest_tokens(word,text)
darkknightrisesentiment <- tidy_sentiment1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = paranum, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
ggplot(darkknightrisesentiment,aes(index, sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  ggtitle("sentiment analysis of dark knight rises review")


#output csv file
write.csv(omdb_imdb,"/Users/jane/Downloads/data\ wrangling/project/omdb_imdb.csv")
write.csv(article.tbl,"/Users/jane/Downloads/data\ wrangling/project/dark_knight.csv")
write.csv(article.tbl1,"/Users/jane/Downloads/data\ wrangling/project/dark_knight_rises.csv")


