# -------------------------------
# FINAL PROJECT ANALYSIS BY SHALINI JAMES PAULRAJ
# Comprehensive Text Analytics on Airbnb Postings:
# Data/Text Cleaning & Preprocessing, Tokenization, and Four TM Frameworks
# -------------------------------

# 1. DATA RETRIEVAL: Connect to MongoDB and Export Data
# Install and load necessary packages
install.packages("mongolite")
library(mongolite)
unlink("C:/Users/shali/AppData/Local/R/win-library/4.4/00LOCK", recursive = TRUE)
if (!require(shiny)) install.packages("shiny")
library(mongolite)
library(writexl)
library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(quanteda)
library(igraph)
library(ggraph)
library(shiny)

# --- Connect to MongoDB and Retrieve Data ---
connection_string <- 'mongodb+srv://shalini:shalini25@shalini.thgq9.mongodb.net/?retryWrites=true&w=majority&appName=Shalini'
# Note: Change collection and database if needed to match the Airbnb dataset.
airbnb_collection <- mongo(collection = "listingsAndReviews", db = "sample_airbnb", url = connection_string)
raw_data <- airbnb_collection$find()
View(raw_data)


# 2. DATA PREPARATION & PREPROCESSING
# Assume the text to analyze is in the "description" column; otherwise use the first column
if("description" %in% colnames(raw_data)){
  text_data <- raw_data$description
} else {
  text_data <- raw_data[, 1]  # adjust if necessary
}

# Create a data frame for text analysis
text_df <- data.frame(line = 1:length(text_data),
                      text = as.character(text_data),
                      stringsAsFactors = FALSE)
head(text_df)

# 3. TEXT TOKENIZATION & FREQUENCY ANALYSIS
# Tokenize the text: convert to lower case, remove punctuation
tokens <- text_df %>%
  unnest_tokens(word, text)

# Remove standard English stop words
data("stop_words")
tokens_clean <- tokens %>%
  anti_join(stop_words, by = "word")

# Calculate word frequencies
word_freq <- tokens_clean %>%
  count(word, sort = TRUE)
print(word_freq)

# Visualize the top 20 most frequent words
top_words <- word_freq %>% top_n(20, n)
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Words", y = "Frequency", title = "Top 20 Words in Postings")

# 4. SENTIMENT ANALYSIS
# Use the AFINN lexicon to calculate sentiment scores for each posting
afinn <- get_sentiments("afinn")
sentiment_scores <- tokens_clean %>%
  inner_join(afinn, by = "word") %>%
  group_by(line) %>%
  summarise(sentiment = sum(value))
print(sentiment_scores)

# Visualize distribution of sentiment scores
ggplot(sentiment_scores, aes(x = sentiment)) +
  geom_histogram(binwidth = 1, fill = "tomato", color = "black") +
  labs(x = "Sentiment Score", y = "Count", title = "Distribution of Sentiment Scores")

# 5. N-GRAMS ANALYSIS
# Create bigrams from the text
bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
# Separate bigrams into individual words
bigrams_separated <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")
# Remove stop words from both words in the bigram
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word)
# Count bigrams
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
print(bigram_counts)

# Visualize the top 15 bigrams
top_bigrams <- bigram_counts %>% top_n(15, n) %>%
  unite(bigram, word1, word2, sep = " ")
ggplot(top_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(x = "Bigrams", y = "Frequency", title = "Top 15 Bigrams")

# 6. TF-IDF ANALYSIS
# Calculate TF-IDF for words grouped by document (each posting is a document)
tfidf <- tokens_clean %>%
  count(line, word, sort = TRUE) %>%
  bind_tf_idf(word, line, n) %>%
  arrange(desc(tf_idf))
print(tfidf)

# Visualize top words by TF-IDF
top_tfidf <- tfidf %>% group_by(word) %>% summarise(tf_idf = max(tf_idf)) %>% top_n(20, tf_idf)
ggplot(top_tfidf, aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(x = "Words", y = "TF-IDF", title = "Top 20 Words by TF-IDF")

# Business Insight:
# - Frequency analysis reveals the most common words in the posting descriptions.
# - Sentiment analysis (using the AFINN lexicon) shows overall sentiment trends per posting.
# - N-grams analysis identifies common word pairs (bigrams) that may capture key phrases.
# - TF-IDF analysis highlights unique words that differentiate individual postings.

# Creating a Dashboard using R shiny
# UI
# -------------------------------
ui <- fluidPage(
  titlePanel("Airbnb Postings Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Text Mining Frameworks"),
      p("Navigate through the tabs to view analysis results:"),
      tags$ul(
        tags$li("Word Frequency Analysis"),
        tags$li("Sentiment Analysis"),
        tags$li("Bigrams Analysis"),
        tags$li("TF-IDF Analysis")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Frequency Analysis", plotOutput("freqPlot")),
        tabPanel("Sentiment Analysis", plotOutput("sentPlot")),
        tabPanel("Bigrams Analysis", plotOutput("bigramPlot")),
        tabPanel("TF-IDF Analysis", plotOutput("tfidfPlot"))
      )
    )
  )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
  
  output$freqPlot <- renderPlot({
    ggplot(top_words, aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "Words", y = "Frequency", title = "Top 20 Words in Postings")
  })
  
  output$sentPlot <- renderPlot({
    ggplot(sentiment_scores, aes(x = sentiment)) +
      geom_histogram(binwidth = 1, fill = "tomato", color = "black") +
      labs(x = "Sentiment Score", y = "Count", title = "Distribution of Sentiment Scores")
  })
  
  output$bigramPlot <- renderPlot({
    ggplot(top_bigrams, aes(x = reorder(bigram, n), y = n)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(x = "Bigrams", y = "Frequency", title = "Top 15 Bigrams")
  })
  
  output$tfidfPlot <- renderPlot({
    ggplot(top_tfidf, aes(x = reorder(word, tf_idf), y = tf_idf)) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(x = "Words", y = "TF-IDF", title = "Top 20 Words by TF-IDF")
  })
}
# -------------------------------
# Run the Shiny App
# -------------------------------
shinyApp(ui, server)


