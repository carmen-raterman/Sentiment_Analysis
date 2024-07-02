# Adapted from the book "Text Mining with R: A Tidy Approach" https://www.tidytextmining.com/sentiment.html

# Load packages
library(pacman)
p_load(tidyverse,tidytext,janeaustenr,dplyr,stringr, wordcloud, textdata, wordcloud2, dplyr, gutenbergr, scales)

# Tokens and Tidy data
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

text_df <- tibble(line = 1:4, text = text)
text_df

text_df %>%
  unnest_tokens(word, text)

# Can we do this for longer strings?
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
          chapter = cumsum(str_detect(text, 
          regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()
original_books

tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

# What about words used as filler?
data(stop_words)
stop_words

tidy_books2 <- tidy_books %>%
  anti_join(stop_words)

# Once tokennized, use count() to find the most common words
tidy_books2 %>%
  count(word, sort = TRUE) 

# Visualize results using ggplot

# Quick aside to build a custom color palette
coul <- brewer.pal(4, "Spectral") 
# Add more colors to this palette :
coul <- colorRampPalette(coul)(14)

tidy_books2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word), col = coul) +
  geom_col() +
  labs(y = NULL) +
  scale_fill_manual(values = coul) +
  theme(legend.position="none")

# How can we compare across multiple tidytext dataframes?
# creating a function that handles potential book downloading errors
download_books <- function(book_ids) {
  downloaded_books <- list()
  for (book_id in book_ids) {
    try({
      book <- gutenberg_download(book_id)
      downloaded_books[[as.character(book_id)]] <- book
    }, silent = TRUE)
  }
  bind_rows(downloaded_books, .id = "book_id")
}

# Gutenbergr package has many books
# Downloading books for H.G. Wells
hgwells_books <- c(35, 36, 5230)
tidy_hgwells <- download_books(hgwells_books) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Downloading books for Brontë Sisters
bronte_books <- c(1260, 768, 969, 9182, 767)
tidy_bronte <- download_books(bronte_books) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Assuming `tidy_books` is defined similarly for Jane Austen or another author
# Replace this with the actual tidy_books for Jane Austen
jane_austen_books <- c(1342, 158, 141, 121, 105) # Example Jane Austen books
tidy_books <- download_books(jane_austen_books) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Combining data
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Brontë Sisters`:`H.G. Wells`,
               names_to = "author", values_to = "proportion")

# Plotting
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# Correlation tests
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)


# Sentiment Analysis
# Look at various sentiment lexicons

get_sentiments("afinn") #http://www2.imm.dtu.dk/pubdb/pubs/6010-full.html ()

get_sentiments("bing") #https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html ()

get_sentiments("nrc") #http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm (crowdsourcing)

# Sentiment analysis using existing lexicons with an inner join to your text


tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) #default is unigram (can do diagrams etc for more complex words/phrases)

# Pull in the sentiment lexicon
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# Filter the Austin books to just one book, and just those words mapped to joy, then counted
tidy_2 <- tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

## Looking at how sentiment changes across the course of a novel, using the bing lexicon
# Dataset creation - austen books as words, joined with sentiments, divided by 80, counted, diff betw pos/neg
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>% # "%/%" divides line nbr by 80 as an integer
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Now plot trajectories across each novel
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  scale_fill_brewer(palette = "Dark2", direction = 1)


## How might a choice of lexicon/dictionary change your results?
# Filter to just Pride & Prejudice
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

# Using AFINN
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

# Using BING & NRC, binding rows
bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Now let's compare them.  What do you see?
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")+
  scale_fill_brewer(palette = "Accent", direction = 1)

## We can visualize the data in other formats, not just ggplot graphs

# What about a word cloud?
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

tidy_wc <- tidy_books %>%
  anti_join(stop_words) %>%
  count(word)

wordcloud(words = tidy_wc$word, freq = tidy_wc$n, min.freq = 200,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Set2"), scale=c(2.5,0.5))

tidy_wc %>%
  filter(n > 200) %>%
  wordcloud2(size=1.6, color='random-dark')

## What words are contributing to each sentiment?
bing_word_counts <- tidy_books2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# How can we adjust for misclassified words in-context?
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)
custom_stop_words


## What if you want to compare more than single words (unigrams)?
# Bigrams - 2 word pairs (all)
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# Visualize the resulting set
austen_bigrams %>%
  count(bigram, sort = TRUE)

# Notice the filler (stop) words.  
#We can remove those by splitting the bigram into 2 columns, 
#then filtering each for stop words, then recombining
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# Trigrams? All at once...
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


