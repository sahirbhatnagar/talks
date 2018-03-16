#' #######################################################################################
#' Functions to process description text from google scraping
#'
#' @description This code takes as input google scraping results from
#'   http://scraping.services/ and returns a document term matrix. Frequencies
#'   of words can then be plotted using wordclouds or barplots. This script was
#'   used to produce the figures in my keynote presentation titled "Pick your
#'   favorite buzzword: Data Science, Machine Learning, Big Data. Data Science,
#'   Machine Learning, Big Data" at the 14th Annual Research day in the
#'   Department of Epidemiology, Biostatistics and Occupational Health at McGill
#'   University on March 16th, 2018.
#'
#' @note http://scraping.services/ is a paid service accesible only through
#'   their website, so I can't make this script entirely reproducible. The
#'   results from the searches are provided as .csv files in the data/ folder
#'
#'
#'   #######################################################################################
#'
   


# packages ----------------------------------------------------------------

pacman::p_load(data.table)
pacman::p_load(tm)
pacman::p_load(wordcloud)
pacman::p_load(ggplot2)
pacman::p_load(cowplot)
pacman::p_load(qdap)
pacman::p_load(sentimentr)
pacman::p_load(magrittr)
pacman::p_load(tidytext)
pacman::p_load(dplyr)
pacman::p_load(stringr)
pacman::p_load(janeaustenr)
pacman::p_load(tidyr)
pacman::p_load(viridis)

# load-data ---------------------------------------------------------------

DT_bigdata <- fread("data/bigdata_google_scrape.csv")
DT_stats <- fread("data/statistics_statistician_biostatistics_biostatistician_google_scrape.csv")
DT_ds <- fread("data/data_science_data_scientist_google_scrape.csv")
DT_epi <- fread("data/epidemiology_epidemiologist_google_scrape.csv")
DT_ml <- fread("data/machine_learning_google_scrape.csv")
DT_bootcamp <- fread("data/data_science_bootcamp_google_scrape.csv")


# Function to tokenize descriptions and give word frequencies -------------

get_freqs <- function(data, remove_words = c("statistics","statistician", "statisticians", 
                                             "biostatistics", "biostatistician",
                                             "biostatistic", "biostatisticians"),
                      top = 20) {
  
  docs <- VCorpus(DataframeSource(data.frame(doc_id = "doc_1", text = data[["DESCRIPTION"]])))     
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, tolower)   
  docs <- tm_map(docs, removeWords, stopwords("english"))   
  docs <- tm_map(docs, removeWords, remove_words)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)
  
  ### Stage the Data      
  dtm <- DocumentTermMatrix(docs)   
  tdm <- TermDocumentMatrix(docs)   
  
  dtms <- removeSparseTerms(dtm, .99) # This makes a matrix that is 10% empty space, maximum.
  freq <- colSums(as.matrix(dtms))   
  wf <- data.frame(word=names(freq), freq=freq)   
  freq <- wf %>% arrange(desc(freq)) %>% top_n(top) %>% pull(freq)
  names(freq) <- wf %>% arrange(desc(freq)) %>% top_n(top) %>% pull(word)
  
  return(freq)
  
}

top = 100
(top_bigdata <- get_freqs(data = DT_bigdata, remove_words = c("big","data"), top = top))
(top_stats <- get_freqs(data = DT_stats, remove_words = c("statistics","statistician", "statisticians", 
                                                            "biostatistics", "biostatistician",
                                                            "biostatistic", "biostatisticians"), top = top))
(top_ds <- get_freqs(data = DT_ds, remove_words = c("data","science","scientist","scientists"), top = top))
(top_epi <- get_freqs(data = DT_epi, remove_words = c("epidemiology","epidemiologist"), top = top))
(top_ml <- get_freqs(data = DT_ml, remove_words = c("machine","learning"), top = top))
(top_bootcamp <- get_freqs(data = DT_bootcamp, remove_words = c("data","science","bootcamp"), top = top))





# Wordclouds --------------------------------------------------------------

set.seed(12345)
png("figures/stat_wordcloud.png", width = 12, height = 8, units = "in", res = 125)
wordcloud(words = names(top_stats), freq = top_stats, max.words = 50, 
          colors = viridis(length(top_stats), begin = 0, end = 0.6, direction = -1), ordered.colors = F)
dev.off()


set.seed(3123)
png("figures/ds_wordcloud.png", width = 12, height = 8, units = "in", res = 125)
wordcloud(words = names(top_ds), freq = top_ds, max.words = 50, 
          colors = viridis(length(top_stats), begin = 0, end = 0.8, direction = -1), 
          ordered.colors = F)
dev.off()

set.seed(312789)
png("figures/epi_wordcloud.png", width = 12, height = 8, units = "in", res = 125)
wordcloud(words = names(top_epi), freq = top_epi, max.words = 50, 
          colors = viridis(length(top_epi), begin = 0, end = 0.3, direction = -1), 
          ordered.colors = F)
dev.off()



set.seed(312789)
png("figures/bootcamp_wordcloud.png", width = 12, height = 8, units = "in", res = 125)
wordcloud(words = names(top_bootcamp), freq = top_bootcamp, max.words = 50, 
          colors = viridis(length(top_bootcamp), begin = 0, end = 0.3, direction = -1), 
          ordered.colors = F)
dev.off()





# Not Used and Scrap Work ----------------------------------------------------------------


# docs <- VCorpus(DataframeSource(data.frame(doc_id = c(rep("Statistics", nrow(DT_stats)),
#                                                           rep("Data Science", nrow(DT_ds))), 
#                                            text = c(DT_stats[["DESCRIPTION"]], DT_ds[["DESCRIPTION"]]), 
#                                            stringsAsFactors = FALSE)))     
# docs <- VCorpus(VectorSource(list(DT_stats[["DESCRIPTION"]], DT_ds[["DESCRIPTION"]])))
# 
# docs <- tm_map(docs, removePunctuation)
# docs <- tm_map(docs, removeNumbers)
# docs <- tm_map(docs, tolower)   
# docs <- tm_map(docs, removeWords, stopwords("english"))   
# # docs <- tm_map(docs, removeWords, remove_words)
# docs <- tm_map(docs, stripWhitespace)
# docs <- tm_map(docs, PlainTextDocument)
# 
# term.matrix <- TermDocumentMatrix(docs)
# term.matrix <- as.matrix(term.matrix)
# dim(term.matrix)
# colnames(term.matrix) <- c("Statistics","Data Science")
# head(term.matrix)
# comparison.cloud(term.matrix,max.words=100,random.order=FALSE)
# wordcloud(words = rownames(term.matrix), freq = term.matrix[,1], max.words = 100)


# par(las=2) # make label text perpendicular to axis
# cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# par(mar=c(5,13,4,2)) # increase y-axis margin.
# barplot(sort(top_bigdata,
#              decreasing=FALSE), 
#         horiz=TRUE, 
#         las=1,
#         cex.names = 2,
#         cex.axis = 2,
#         cex.lab = 2,
#         cex.main = 2,
#         # col= rev(hcl(h = seq(15, 375, len = max(4, length(freq) + 1)), l = 60,
#         #                 c = 150, alpha = 0.8)),
#         col = cbbPalette,
#         main="Most Common Words (minimum 20)", ylab = "",xlab = "Frequency")


#  
## Relationships Between Terms
### Term Correlations
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.    
findAssocs(dtm, c("data","science", "statistics"), corlimit=0.3) # specifying a correlation limit of 0.85
findAssocs(dtms, "insights", corlimit=0.1) # specifying a correlation limit of 0.95   
# 
# Change "country" & "american", or "think" to terms that actually appear in your texts.
# Also adjust the `corlimit= ` to any value you feel is necessary.
#
# 
### Word Clouds!   
# First load the package that makes word clouds in R.    
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.99) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2)#, colors=cbbPalette)    

### Clustering by Term Similarity

### Hierarchal Clustering   
library(cluster)   
dtms <- removeSparseTerms(dtm, 0.96) # This makes a matrix that is only 15% empty space.
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=3)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 5 clusters   

### K-means clustering   
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.99) # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="mink")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  








dat <- jsonlite::read_json("/home/sahir/Dropbox/PhD/Year5/keynote/data/data_science_keyword.json")
dat <- jsonlite::fromJSON("/home/sahir/Dropbox/PhD/Year5/keynote/data/data_science_keyword.json",
                           flatten=TRUE, simplifyDataFrame = TRUE)

as.data.frame(do.call(rbind,dat[[2]]$results$keyword_results$`data science`$countries)) 


df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
df



mytext <- get_sentences(DT$DESCRIPTION)
out <- sentiment_by(mytext)
plot(out)
out %>% highlight()
library(tidyr)

out <- sentiment_by(DT$DESCRIPTION, by = DT$KEYWORD)

ggplot(out, aes(KEYWORD, ave_sentiment)) + 
  geom_col()

pacman::p_load(tidytext)
pacman::p_load(dplyr)
pacman::p_load(stringr)
pacman::p_load(janeaustenr)


tidy_books <- DT %>%
  group_by(KEYWORD) %>%
  ungroup() %>%
  unnest_tokens(word, DESCRIPTION) %>% 
  mutate(word = replace(word, word == "job", "jobs")) 

bing_word_counts <- tidy_books %>%
  group_by(KEYWORD) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_word_counts %>%
  group_by(sentiment, KEYWORD) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~KEYWORD, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(reshape2)
dev.off()
par(mfrow=c(1,2))
tidy_books %>% filter(KEYWORD=="data scientist") %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(#colors = c("gray20", "gray80"),
                   colors = cbbPalette[c(7,4)],
                   max.words = 20)

tidy_books %>% filter(KEYWORD=="statistician") %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = cbbPalette[c(7,4)],
                   max.words = 20)


data(SOTU)
corp <- SOTU
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, function(x)removeWords(x,stopwords()))

term.matrix <- TermDocumentMatrix(corp)
term.matrix <- as.matrix(term.matrix)
colnames(term.matrix) <- c("SOTU 2010","SOTU 2011")
str(term.matrix)
comparison.cloud(term.matrix,max.words=40,random.order=FALSE)
commonality.cloud(term.matrix,max.words=40,random.order=FALSE)


pacman::p_load(XML)
pacman::p_load(RCurl)

getGoogleLinks <- function(google.url) {
  doc <- getURL(google.url, httpheader = c("User-Agent" = "R
                                           (2.10.0)"))
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
                        (...){})
  nodes <- getNodeSet(html, "//h3[@class='r']//a")
  return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}

getGoogleLinks("https://www.google.ca/search?q=data+science+bootcamp")

pacman::p_load(rvest)
#load data
d <-read.csv("P:\\needWebsites.csv")
c <- as.character(d$Company.Name)

# Function for getting website.
getWebsite <- function(name) {
  url = URLencode(paste0("https://www.google.com/search?q=",name))
  
  page <- read_html(url)
  
  results <- page %>%
    html_nodes("cite") %>% # Get all notes of type cite. You can change this to grab other node types.
    html_text()
  
  
  return(results)
  
  result <- results[1]
  
  
  return(as.character(result)) # Return results if you want to see them all.
}

web <- getWebsite("data science bootcamp")
web

### Explore your data      
# freq <- colSums(as.matrix(dtm))   
# length(freq)   
# ord <- order(freq)   
# freq[order(freq, decreasing = TRUE)][1:20]
# m <- as.matrix(dtm)   
# dim(m)   
# write.csv(m, file="/home/sahir/Dropbox/PhD/Year5/keynote/data/text/DocumentTermMatrix.csv")   
### FOCUS - on just the interesting stuff...   
#  Start by removing sparse terms:   
# dtms$dimnames
### Word Frequency   
# head(table(freq), 20)   
# The above output is two rows of numbers. The top number is the frequency with which 
# words appear and the bottom number reflects how many words appear that frequently. 
#
# tail(table(freq), 20)   
# Considering only the 20 greatest frequencies
#
# **View a table of the terms after removing sparse terms, as above.

# freq   
# The above matrix was created using a data transformation we made earlier. 
# **An alternate view of term frequency:**   
# This will identify all terms that appear frequently (in this case, 50 or more times).   
# findFreqTerms(dtm, lowfreq=20)   # Change "50" to whatever is most appropriate for your data.
### Plot Word Frequencies
# **Plot words that appear at least 50 times.**   

# wf[wf$word=="job","freq"] <- wf[wf$word=="job","freq"] + wf[wf$word=="jobs","freq"]
# wf <- wf[-which(wf$word=="jobs"),]
# wf.o <- wf[order(wf$freq, decreasing = T),]

# writeLines(DT$DESCRIPTION, 
#            "/home/sahir/Dropbox/PhD/Year5/keynote/data/text/data_scientist_google_scrape_description.txt")
# Needed <- c("SnowballCC", "biclust")   
# install.packages(Needed, dependencies=TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")    
# pacman::p_load("SnowballCC")
# library(igraph)
# library(biclust)
# pacman::p_load_gh('hrbrmstr/pluralize')
# docs <- VCorpus(DirSource("/home/sahir/Dropbox/PhD/Year5/keynote/data/text/"))   