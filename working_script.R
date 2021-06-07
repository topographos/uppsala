#load r packages
library(rvest)
library(rebus)
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords)
library(wordcloud)
library(textdata)
library(ggplot2)

# read the web page
html = read_html("https://www.abm.uu.se/cdhu-eng/projects/")

# read p html element
p = html %>% html_elements("p") %>% html_text2()

# create tokenized vector
tokenized_p = p %>% 
  str_remove_all(PUNCT) %>% 
  str_split(SPACE) %>% 
  unlist() %>% 
  str_to_lower()

# import stop words
data("stop_words")

# create a tibbele and filter out some words
df = tibble(
  word = tokenized_p
) %>% 
filter(word != "") %>% 
filter(word != "contact") %>% 
filter(!word %in% c(stop_words$word)) %>% 
group_by(word) %>% 
count() %>% 
ungroup()

# get the 100 most mentioned workds
top_100 = df %>% slice_max(n, n = 100)

# create a world cloud
wc = wordcloud(top_100$word, top_100$n, colors=brewer.pal(8, "Dark2"), random.order = FALSE)


# sentiments
nrc <- get_sentiments("nrc")

nrc_sentiment_counts <- nrc %>%
  inner_join(df, by = c("word" = "word")) %>% 
  group_by(sentiment) %>%
  summarise(n = sum(n))

nrc_graph <- nrc_sentiment_counts %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + labs(title = "Frequency of NRC sentiments in the Centre for Digital Humanities Uppsala")
nrc_graph


