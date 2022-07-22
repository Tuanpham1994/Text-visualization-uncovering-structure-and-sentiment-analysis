setwd("C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Text ANalytics/Assignment 1")
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(syuzhet)
library(SnowballC)
library(qdap)
options(stringsAsFactors = FALSE)
#Load data, change to lower case and perform various replacements
#(recognize emoticons, times, numbers and smileys, etc)
#Try to figure out what each replacement does.
clothing_df <- read.csv("WomensClothingECommerceReviews.csv") 

# Load "translation" of emoticons
data(emoticon)
head(emoticon)
summary(emoticon)
clothing_df_small <- clothing_df %>% group_by(Clothing.ID) %>% slice(which.min(Rating))
#library(data.table)
#clothing_df_small <- clothing_df[ , .SD[Rating == min(Rating)], by = Clothing.ID]
#clothing_df_small <- clothing_df %>% group_by(Clothing.ID)%>% filter(Rating == min(Rating))

clothing_df$Description <- as.character(clothing_df$Review.Text)  %>%
  tolower() %>%
  {mgsub(emoticon[,2], emoticon[,1], .)} %>%
  {gsub("\\n", " ", .)} %>%                    # Remove \n (newline)     
  {gsub("[?!]+", ".", .)} %>%                  # Remove ? and ! (replace by single .)
  {gsub("[\\[\\*\\]]*", " ", .)} %>%           # Remove [ and ] * (replace by single space)
  {gsub("(\"| |\\$)-+\\.-+", " number ", .)} %>%# Find numbers
  {gsub("(-+:)*-+ *am", " timeam", .)} %>%     # Find time AM
  {gsub("(-+:)*-+ *pm", " timepm", .)} %>%     # Find time PM
  {gsub("-+:-+", "time", .)} %>%               # Find general time
  {gsub("( |\\$)--+", " number ", .)} %>%      # Find remaining numbers
  {gsub("-"," ", .)} %>%                       # Remove all -
  {gsub("\"+", " ", .)} %>%                    # Remove all "
  {gsub(";+", " ", .)} %>%                     # Remove excess ;
  {gsub("\\.+","\\. ", .)} %>%                 # Remove excess .
  {gsub(" +"," ", .)} %>%                      # Remove excess spaces
  {gsub("\\. \\.","\\. ", .)}                  # Remove space between periods

clothing_df_small$Description <- as.character(clothing_df_small$Review.Text)%>%
  tolower() %>%
  {mgsub(emoticon[,2], emoticon[,1], .)} %>%
  {gsub("\\n", " ", .)} %>%                    # Remove \n (newline)     
  {gsub("[?!]+", ".", .)} %>%                  # Remove ? and ! (replace by single .)
  {gsub("[\\[\\*\\]]*", " ", .)} %>%           # Remove [ and ] * (replace by single space)
  {gsub("(\"| |\\$)-+\\.-+", " number ", .)} %>%# Find numbers
  {gsub("(-+:)*-+ *am", " timeam", .)} %>%     # Find time AM
  {gsub("(-+:)*-+ *pm", " timepm", .)} %>%     # Find time PM
  {gsub("-+:-+", "time", .)} %>%               # Find general time
  {gsub("( |\\$)--+", " number ", .)} %>%      # Find remaining numbers
  {gsub("-"," ", .)} %>%                       # Remove all -
  {gsub("\"+", " ", .)} %>%                    # Remove all "
  {gsub(";+", " ", .)} %>%                     # Remove excess ;
  {gsub("\\.+","\\. ", .)} %>%                 # Remove excess .
  {gsub(" +"," ", .)} %>%                      # Remove excess spaces
  {gsub("\\. \\.","\\. ", .)}                  # Remove space between periods

print("number of reviews")
nrow(clothing_df)
typeof(clothing_df)

#remove blank text
clothing_df <- clothing_df[clothing_df$Review.Text != "",]
clothing_df_small <- clothing_df_small[clothing_df_small$Review.Text != "",]
summary(clothing_df_small)
summary(clothing_df)
hist(clothing_df$Age)
hist(clothing_df_small$Age)

clothing_df_small_backup <- clothing_df_small
clothing_df_backup <- clothing_df #%>% group_by(Clothing.ID) %>% slice(which.min(Rating))


#Create new CSV file
library(readr)

all_words_clothing <- clothing_df[,] %>%
  unnest_tokens("Description", output = "word") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  filter(n>20)

all_words_clothing_small <- clothing_df_small[,] %>%
  unnest_tokens(input = Description, output = "word") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) 

# get sentiment with bing dictionary:
# all_words$sentiment <- get_sentiment(all_words$word, method = "bing", language = "english") 

sentiment_scores_clothing <- polarity(all_words_clothing$word)$all        # get sentiment with polarity function
all_words_clothing$sentiment <- sentiment_scores_clothing[,"polarity"]

sentiment_scores_clothing_small <- polarity(all_words_clothing_small$word)$all
all_words_clothing_small$sentiment <- sentiment_scores_clothing_small[,"polarity"]
# Create plot, negative words with frequency > 145 and positive words with frequency > 300

all_words_clothing <- all_words_clothing %>%
  filter(sentiment != 0) %>%
  mutate(n = ifelse(sentiment == -1, -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  mutate(Sentiment = ifelse(sentiment == 1, "Positive","Negative"))

all_words_clothing %>% filter((Sentiment == "Positive" & n > 500)|(Sentiment == "Negative"& n < -145))%>%
  ggplot(aes(word, n, fill = Sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to \"total\" sentiment", x = "Word (min freq = 20)")

all_words_clothing_small <- all_words_clothing_small %>%
  filter(sentiment != 0) %>%
  mutate(n = ifelse(sentiment == -1, -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  mutate(Sentiment = ifelse(sentiment == 1, "Positive","Negative"))

all_words_clothing_small %>% filter((Sentiment == "Positive" & n > 2)|(Sentiment == "Negative"& n < -2))%>%
  ggplot(aes(word, n, fill = Sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to \"total\" sentiment", x = "Word (min freq = 20)")

####Overall sentiment analysis with negator and amplifier
data(negation.words)
head(negation.words)
data(amplification.words)
head(amplification.words)
key.pol[grep("sexy", x)]

custom_pol <- sentiment_frame(amplification.words, negation.words)
sentiment_score1 <- polarity(clothing_df$Description, polarity.frame = custom_pol)
sentiment_score_small <- polarity(clothing_df_small$Description, polarity.frame = custom_pol)

sentiment_score1$all
clothing_df$sentiment <- sentiment_score1$all[,"polarity"]
clothing_df_small$sentiment <- sentiment_score_small$all[,"polarity"]
hist(clothing_df$sentiment)
summary(clothing_df$sentiment)
hist(clothing_df_small$sentiment)
summary(clothing_df_small$sentiment)

#Split reviews based on response "happy"/"unhappy" and show mean of sentiment
pos_reviews_clothing <- clothing_df %>% filter(Rating == 5)
neg_reviews_clothing <- clothing_df %>% filter(Rating == 1)
pos_reviews_clothing_small <- clothing_df_small %>% filter(Rating ==5)
neg_reviews_clothing_small <- clothing_df_small %>% filter(Rating ==1)

ggplot(clothing_df ,aes(sentiment)) +
  geom_density(aes(fill = "Happy"),   data = pos_reviews_clothing, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_small ,aes(sentiment)) +
  geom_density(aes(fill = "Happy"),   data = pos_reviews_clothing_small, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_small, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

#####---sentiment scores per full review using different methods (treating as one sentence)

clothing_df$sent_bing <- get_sentiment(clothing_df$Description, method = "bing", language = "english")
clothing_df$sent_syu <- get_sentiment(clothing_df$Description, method = "syuzhet")
clothing_df$sent_afinn <- get_sentiment(clothing_df$Description,method = "afinn")
clothing_df$sent_nrc <- get_sentiment(clothing_df$Description,method = "nrc")

clothing_df_small$sent_bing <- get_sentiment(clothing_df_small$Description, method = "bing", language = "english")
clothing_df_small$sent_syu <- get_sentiment(clothing_df_small$Description, method = "syuzhet")
clothing_df_small$sent_afinn <- get_sentiment(clothing_df_small$Description,method = "afinn")
clothing_df_small$sent_nrc <- get_sentiment(clothing_df_small$Description,method = "nrc")
clothing_df_small %>% filter(Rating == 5) %>% summary()
clothing_df_small %>% filter(Rating == 1| Rating == 2) %>% summary()

####---polarity no valence shifters---###
pol_no_valence <- polarity(clothing_df[, "Description"])$all
clothing_df$polarity_no_valence <- pol_no_valence$polarity
?polarity
pol_no_valence_small <- polarity(text.var = clothing_df_small$Description)$all
clothing_df_small$polarity_no_valence <- pol_no_valence_small$polarity

###---polarity with valence shifter
data(negation.words)
head(negation.words)
data(amplification.words)
head(amplification.words)
custom_pol <- sentiment_frame(amplification.words,negation.words)
pol_valence <- polarity(clothing_df$Description, polarity.frame = custom_pol)$all
clothing_df$polarity_valence <- pol_valence$polarity

pol_valence_small <- polarity(clothing_df_small$Description, polarity.frame = custom_pol)$all
clothing_df_small$polarity_valence <- pol_valence_small$polarity


###---Histogram of general rating for all products---###
pos_reviews_clothing <- clothing_df %>% filter(Rating == 5)
neg_reviews_clothing <- clothing_df %>% filter(Rating == 1|Rating ==2)
pos_reviews_clothing_small <- clothing_df_small %>% filter(Rating == 5)
neg_reviews_clothing_small <- clothing_df_small %>% filter(Rating == 1|Rating ==2)

#Histograms of sentiment split on Happy/Unhappy on polarity without valence shifter

ggplot(clothing_df ,aes(polarity_no_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df ,aes(polarity_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_small ,aes(polarity_no_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_small, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_small, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_small ,aes(polarity_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_small, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_small, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_small ,aes(sent_syu)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_small, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_small, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_small ,aes(sent_nrc)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_small, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_small, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_small ,aes(sent_afinn)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_small, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_small, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_small ,aes(sent_bing)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_small, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_small, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

#Word frequencies in neg/pos reviews using polarity
# select negative reviews
all_neg_review_words_clothing <- clothing_df %>% filter(clothing_df$polarity_no_valence<0)
all_neg_review_words_clothing <- all_neg_review_words_clothing %>%
  unnest_tokens(word, Description) %>%
  anti_join(stop_words, by = "word")
all_neg_review_words_clothing%>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(25, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Word Frequencies: negatively classified reviews")
#Same for positive sentiment reviews

all_pos_review_words_clothing <- clothing_df  %>% filter(clothing_df$polarity_no_valence>0)

all_pos_review_words_clothing  <- (all_pos_review_words_clothing ) %>% 
  unnest_tokens(word,Description) %>% 
  anti_join(stop_words, by = "word")

all_pos_review_words_clothing%>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(25, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Word Frequencies: positively classified reviews")

#**Sentence-based** sentiment analysis: number of positive sentences - number of negative sentences
#(this code generates some warnings on 'double punctuation')
# This needs to be done by review as it decomposes into sentences
clothing_df_small_1 <- clothing_df_small[,-c(13:19)]
clothing_df_small_1 <- cbind(clothing_df_small_1, sentence_sent_polarity = 0*cbind(1:nrow(clothing_df_small_1)),
                    sentence_sent_syu      = 0*cbind(1:nrow(clothing_df_small_1)),
                    sentence_sent_afinn    = 0*cbind(1:nrow(clothing_df_small_1)),
                    sentence_sent_bing     = 0*cbind(1:nrow(clothing_df_small_1)),
                    sentence_sent_nrc      = 0*cbind(1:nrow(clothing_df_small_1)))

totalNoSentences1 = 0
localSentences1 <- get_sentences(clothing_df_small_1[1,]$Description)
totalNoSentences1 = totalNoSentences1 + length(localSentences1)
#sum total sign of each sentence
for (j in 1:nrow(clothing_df_small_1 )) {
  # print progress
  if (j%%1000==0){
    print(100*j/nrow(clothing_df_small_1 ))
  }
  localSentences1 <- get_sentences(clothing_df_small_1[j,]$Description)
  totalNoSentences1 = totalNoSentences1 + length(localSentences1)
  
  clothing_df_small_1[j,]$sentence_sent_polarity <- polarity(localSentences1)$all[,"polarity"] %>% 
    sign() %>% sum()
  
  #clothing_df_small_backup[j,]$sentence_sent_syu <-
   # get_sentiment(localSentences1, method = "syuzhet", language = "english") %>% sign()  %>% sum()
  
  #clothing_df_small_backup[j,]$sentence_sent_afinn <-
  #  get_sentiment(localSentences1, method = "afinn", language = "english") %>% sign()  %>% sum()
  
  clothing_df_small_1[j,]$sentence_sent_bing <-
    get_sentiment(localSentences1, method = "bing", language = "english") %>% sign()  %>% sum()
  
 # clothing_df_backup2[j,]$sentence_sent_nrc <-
   # get_sentiment(localSentences, method = "nrc", language = "english") %>% sign()  %>% sum()
}
sum(is.na(clothing_df_small_1))
clothing_df_small_1[is.na(clothing_df_small_1)] <- 0

#Save the dataframe for further use
#Get sentence-level sentiment scores
#(this code generates some warnings on 'double punctuation')
save(clothing_df_small_1, file="Saved_clothing_df_small_1.Rda")
load("Saved_clothing_df_small_1.Rda")
write.csv(clothing_df_small_1, "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Text ANalytics/Assignment 1/clothing_df_small_1.csv",row.names = F)

a <- as.data.frame(clothing_df_small_1)
all_sentences_clothing <- data.frame(sentence    = rep(0, totalNoSentences1),
                            sentiment   = rep(0, totalNoSentences1),
                            polarity    = rep(0, totalNoSentences1),
                            Clothing.ID     = rep(a[1,"Clothing.ID"],     totalNoSentences1),
                            Rating = rep(a[1,"Rating"], totalNoSentences1), 
                            stringsAsFactors = FALSE)

sentenceIndex1 = 1
for (j in 1:nrow(a)) {
  # show some progress 
  if (j%%2000==0){
    print(100*j/nrow(a))
  }
  
  localSentences1 <- a[j,]$Description %>% get_sentences()
  
  all_sentences_clothing[sentenceIndex1 : (sentenceIndex1+length(localSentences1)-1), "sentence"] <- localSentences1
  
  all_sentences_clothing[sentenceIndex1 : (sentenceIndex1+length(localSentences1)-1), "sentiment"] <- get_sentiment(localSentences1, method = "bing")
  all_sentences_clothing[sentenceIndex1 : (sentenceIndex1+length(localSentences1)-1), "polarity"] <- polarity(localSentences1)$all[,"polarity"]
  
  all_sentences_clothing[sentenceIndex1 : (sentenceIndex1+length(localSentences1)-1), "Clothing.ID"]  <- a[j,]$Clothing.ID
  all_sentences_clothing[sentenceIndex1 : (sentenceIndex1+length(localSentences1)-1), "Rating"] <- a[j,]$Rating
  
  sentenceIndex1 = sentenceIndex1 + length(localSentences1)
}

#Split into positive and negative sentences

save(all_sentences_clothing, file="Saved_all_sentences_clothing.Rda")
#load("Saved_all_sentences.Rda")
all_pos_sentences_clothing <- all_sentences_clothing  %>% filter(polarity>0)
all_neg_sentences_clothing <- all_sentences_clothing %>% filter(polarity<0)

#Remove stop words and look at words that occur often in pos or neg sentences

all_neg_sentences_words_clothing <- all_neg_sentences_clothing  %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")
all_pos_sentences_words_clothing <- all_pos_sentences_clothing %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")

#Look at difference of frequency of words in positive vs negative sentences

# Get counts of words in pos (and neg) sentences
all_sentence_words_clothing <- full_join(all_pos_sentences_words_clothing %>% count(word, sort=TRUE),
                                         all_neg_sentences_words_clothing %>% count(word, sort=TRUE),
                                by="word")
all_sentence_words_clothing[is.na(all_sentence_words_clothing$n.x), "n.x"] <- 0
all_sentence_words_clothing[is.na(all_sentence_words_clothing$n.y), "n.y"] <- 0

# Normalize counts by total number of words in each group and calculate ratio
all_sentence_words_clothing$n.x  <- all_sentence_words_clothing$n.x/sum(all_sentence_words_clothing$n.x)
all_sentence_words_clothing$n.y  <- all_sentence_words_clothing$n.y/sum(all_sentence_words_clothing$n.y)
all_sentence_words_clothing$diff <- all_sentence_words_clothing$n.x-all_sentence_words_clothing$n.y

all_sentence_words_clothing%>%
  mutate(word = reorder(word, -diff)) %>%           
  top_n(-50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific negative words")

all_sentence_words_clothing%>%
  mutate(word = reorder(word,diff)) %>%           
  top_n(50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific positive words")

#Focus op nouns: First create a "POS tagger"
library(RDRPOSTagger)
POS_specs_clothing <- rdr_model(language = "English", annotation =  "POS")
?rdr_model
#Identify all nouns and save them in a file (in data.frame is very slow, file is also slow (but a bit less so))
#Write header of file
?file
outFile_clothing <- file("nouns", open="w")
write("doc_id,token_id,token,pos,Rating,sentiment", outFile_clothing);
close(outFile_clothing)
# Process all sentences (POS tagger is really specific on bad sentences, 
# so we do additional pre-processing)
library(data.table)

startIndex_clothing = 1
for (j in startIndex_clothing:nrow(all_sentences_clothing)) {
  if (j%%1000==0){
    print(100*j/nrow(all_sentences_clothing))
  }
  to_analyse_clothing <-  all_sentences_clothing[j,"sentence"]  %>% 
    {gsub("(^ *[,\\.)]+)+","",.)} %>%  # remove leading , or . or ) (also repetitions)
    {gsub("(\\(|\\))"," ", .)}         # remove ( and )
  to_analyse_clothing <-  to_analyse_clothing  %>% gsub("  +"," ", .) # remove double spaces
  
  if(nchar(to_analyse_clothing)>1) {
    
    sentence_nouns_clothing <- tryCatch(
      rdr_pos(POS_specs_clothing, to_analyse_clothing,  doc_id = all_sentences_clothing[j,"Clothing.ID"]),
      error = function(e) 
        print(cat(" POS fails on: ", to_analyse_clothing, " sentence: ", j ))
    )
    if (!is.null(sentence_nouns_clothing))
    {
      sentence_nouns_clothing <- sentence_nouns_clothing %>% filter(pos == "NN")
      if (nrow(sentence_nouns_clothing)>0)
      {
        sentences_nouns_clothing <- data.frame(sentence_nouns_clothing, Rating = all_sentences_clothing[j,"Rating"],
                                      sentiment =  all_sentences_clothing[j,"sentiment"])
        fwrite(sentences_nouns_clothing, file="nouns", append=TRUE)
      }
    }
  }
}
all_sentences_nouns_clothing <- read.csv("nouns")
all_sentences_nouns_clothing$token <- as.character(all_sentences_nouns_clothing$token)

#Plot frequencies of nouns in neg sentences
all_neg_sentences_nouns_clothing <- all_sentences_nouns_clothing %>%
  filter(all_sentences_nouns_clothing$sentiment<0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")
all_neg_sentences_nouns_clothing %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurrences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (negative sentences)")
#Same for positive sentences

all_pos_sentences_nouns_clothing <- all_sentences_nouns_clothing %>%
  filter(all_sentences_nouns_clothing$sentiment>0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_pos_sentences_nouns_clothing %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (positive sentences)")

#Comparison clouds

avgsentiment_nouns_clothing <-
  group_by(all_sentences_nouns_clothing, token) %>%
  summarize(m = mean(sentiment), count = n()) %>%
  arrange(desc(abs(m)))
avgsentiment_nouns_clothing

#Create actual comparison cloud
avgsentiment_nouns_clothing$Positive_nouns <-  avgsentiment_nouns_clothing$count*(avgsentiment_nouns_clothing$m > 0)
avgsentiment_nouns_clothing$Negative_nouns <-  avgsentiment_nouns_clothing$count*(avgsentiment_nouns_clothing$m < -0)
avgsentiment_nouns_clothing <- as.data.frame(avgsentiment_nouns_clothing)
rownames(avgsentiment_nouns_clothing) <-  avgsentiment_nouns_clothing$token

comparison.cloud(avgsentiment_nouns_clothing[,c("Positive_nouns","Negative_nouns")], 
                 scale=c(4, 0.5), max.words=100, title.size=1)

#Score each word on emotion.

tmp_clothing <- all_sentences_nouns_clothing %>% group_by(token) %>% summarise(cnt=n())  
emotion_clothing <- get_nrc_sentiment(tmp_clothing$token)
rownames(emotion_clothing) <- tmp_clothing$token
emotion_clothing <- emotion_clothing*tmp_clothing$cnt
comparison.cloud(emotion_clothing[,c("anger","fear","disgust","anticipation","joy","sadness","surprise","trust")],
                 scale=c(3, 0.3), title.size=1)
#####----Dress----#####
clothing_df_dress <- clothing_df %>% filter(Department.Name == "Dresses")

###---Histogram of general rating for all products---###
pos_reviews_clothing_dress <- clothing_df_dress %>% filter(Rating == 5)
neg_reviews_clothing_dress <- clothing_df_dress %>% filter(Rating == 1|Rating ==2)

ggplot(clothing_df_dress ,aes(polarity_no_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_dress, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_dress, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_dress ,aes(polarity_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_dress, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_dress, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

#**Sentence-based** sentiment analysis: number of positive sentences - number of negative sentences
#(this code generates some warnings on 'double punctuation')
# This needs to be done by review as it decomposes into sentences
clothing_df_dress_1 <- clothing_df_dress[,-c(13:19)]
clothing_df_dress_1 <- cbind(clothing_df_dress_1, sentence_sent_polarity = 0*cbind(1:nrow(clothing_df_dress_1)),
                             sentence_sent_syu      = 0*cbind(1:nrow(clothing_df_dress_1)),
                             sentence_sent_afinn    = 0*cbind(1:nrow(clothing_df_dress_1)),
                             sentence_sent_bing     = 0*cbind(1:nrow(clothing_df_dress_1)),
                             sentence_sent_nrc      = 0*cbind(1:nrow(clothing_df_dress_1)))

totalNoSentences_dress = 0
localSentences_dress <- get_sentences(clothing_df_dress_1[1,]$Description)
totalNoSentences_dress = totalNoSentences_dress + length(localSentences_dress)
#sum total sign of each sentence
for (j in 1:nrow(clothing_df_dress_1 )) {
  # print progress
  if (j%%1000==0){
    print(100*j/nrow(clothing_df_dress_1 ))
  }
  localSentences_dress <- get_sentences(clothing_df_dress_1[j,]$Description)
  totalNoSentences_dress = totalNoSentences_dress + length(localSentences_dress)
  
  clothing_df_dress_1[j,]$sentence_sent_polarity <- polarity(localSentences_dress)$all[,"polarity"] %>% 
    sign() %>% sum()
  
  #clothing_df_dress_1[j,]$sentence_sent_syu <-
  # get_sentiment(localSentences_dress, method = "syuzhet", language = "english") %>% sign()  %>% sum()
  
  #clothing_df_dress[j,]$sentence_sent_afinn <-
  #  get_sentiment(localSentences_dress, method = "afinn", language = "english") %>% sign()  %>% sum()
  
  clothing_df_dress_1[j,]$sentence_sent_bing <-
    get_sentiment(localSentences_dress, method = "bing", language = "english") %>% sign()  %>% sum()
  
  # clothing_df_backup2[j,]$sentence_sent_nrc <-
  # get_sentiment(localSentences, method = "nrc", language = "english") %>% sign()  %>% sum()
}
sum(is.na(clothing_df_dress_1))
clothing_df_dress_1[is.na(clothing_df_dress_1)] <- 0

#Save the dataframe for further use
#Get sentence-level sentiment scores
#(this code generates some warnings on 'double punctuation')
save(clothing_df_dress_1, file="Saved_clothing_df_dress_1.Rda")
load("Saved_clothing_df_dress_1.Rda")
write.csv(clothing_df_dress_1, "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Text ANalytics/Assignment 1/clothing_df_dress_1.csv",row.names = F)

dress <- as.data.frame(clothing_df_dress_1)
all_sentences_dress <- data.frame(sentence    = rep(0, totalNoSentences_dress),
                                     sentiment   = rep(0, totalNoSentences_dress),
                                     polarity    = rep(0, totalNoSentences_dress),
                                     Clothing.ID     = rep(a[1,"Clothing.ID"],totalNoSentences_dress),
                                     Rating = rep(a[1,"Rating"], totalNoSentences_dress), 
                                     stringsAsFactors = FALSE)

sentenceIndex_dress = 1
for (j in 1:nrow(dress)) {
  # show some progress 
  if (j%%2000==0){
    print(100*j/nrow(dress))
  }
  
  localSentences_dress <- dress[j,]$Description %>% get_sentences()
  
  all_sentences_dress[sentenceIndex_dress : (sentenceIndex_dress+length(localSentences_dress)-1), "sentence"] <- localSentences_dress
  
  all_sentences_dress[sentenceIndex_dress : (sentenceIndex_dress+length(localSentences_dress)-1), "sentiment"] <- get_sentiment(localSentences_dress, method = "bing")
  all_sentences_dress[sentenceIndex_dress : (sentenceIndex_dress+length(localSentences_dress)-1), "polarity"] <- polarity(localSentences_dress)$all[,"polarity"]
  
  all_sentences_dress[sentenceIndex_dress : (sentenceIndex_dress+length(localSentences_dress)-1), "Clothing.ID"]  <- dress[j,]$Clothing.ID
  all_sentences_dress[sentenceIndex_dress : (sentenceIndex_dress+length(localSentences_dress)-1), "Rating"] <- dress[j,]$Rating
  
  sentenceIndex_dress = sentenceIndex_dress + length(localSentences_dress)
}

#Split into positive and negative sentences

save(all_sentences_dress, file="Saved_all_sentences_dress.Rda")
load("Saved_all_dress.Rda")
all_pos_sentences_dress <- all_sentences_dress  %>% filter(polarity>0)
all_neg_sentences_dress <- all_sentences_dress %>% filter(polarity<0)

#Remove stop words and look at words that occur often in pos or neg sentences

all_neg_sentences_words_dress <- all_neg_sentences_dress  %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")
all_pos_sentences_words_dress <- all_pos_sentences_dress %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")

# Get counts of words in pos (and neg) sentences
all_sentence_words_dress <- full_join(all_pos_sentences_words_dress %>% count(word, sort=TRUE),
                                         all_neg_sentences_words_dress %>% count(word, sort=TRUE),
                                         by="word")
all_sentence_words_dress[is.na(all_sentence_words_dress$n.x), "n.x"] <- 0
all_sentence_words_dress[is.na(all_sentence_words_dress$n.y), "n.y"] <- 0

# Normalize counts by total number of words in each group and calculate ratio
all_sentence_words_dress$n.x  <- all_sentence_words_dress$n.x/sum(all_sentence_words_dress$n.x)
all_sentence_words_dress$n.y  <- all_sentence_words_dress$n.y/sum(all_sentence_words_dress$n.y)
all_sentence_words_dress$diff <- all_sentence_words_dress$n.x-all_sentence_words_dress$n.y

all_sentence_words_dress%>%
  mutate(word = reorder(word, -diff)) %>%           
  top_n(-50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific negative words")

all_sentence_words_dress%>%
  mutate(word = reorder(word,diff)) %>%           
  top_n(50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific positive words")

#Focus op nouns: First create a "POS tagger"
library(RDRPOSTagger)
POS_specs_dress <- rdr_model(language = "English", annotation =  "POS")

#Identify all nouns and save them in a file (in data.frame is very slow, file is also slow (but a bit less so))
#Write header of file

outFile_dress <- file("nouns", open="w")
write("doc_id,token_id,token,pos,Rating,sentiment", outFile_dress);
close(outFile_dress)
# Process all sentences (POS tagger is really specific on bad sentences, 
# so we do additional pre-processing)
library(data.table)

startIndex_dress = 1
for (j in startIndex_dress:nrow(all_sentences_dress)) {
  if (j%%1000==0){
    print(100*j/nrow(all_sentences_dress))
  }
  to_analyse_dress <-  all_sentences_dress[j,"sentence"]  %>% 
    {gsub("(^ *[,\\.)]+)+","",.)} %>%  # remove leading , or . or ) (also repetitions)
    {gsub("(\\(|\\))"," ", .)}         # remove ( and )
  to_analyse_dress <-  to_analyse_dress  %>% gsub("  +"," ", .) # remove double spaces
  
  if(nchar(to_analyse_dress)>1) {
    
    sentence_nouns_dress <- tryCatch(
      rdr_pos(POS_specs_dress, to_analyse_dress,  doc_id = all_sentences_dress[j,"Clothing.ID"]),
      error = function(e) 
        print(cat(" POS fails on: ", to_analyse_dress, " sentence: ", j ))
    )
    if (!is.null(sentence_nouns_dress))
    {
      sentence_nouns_dress <- sentence_nouns_dress %>% filter(pos == "NN")
      if (nrow(sentence_nouns_dress)>0)
      {
        sentences_nouns_dress <- data.frame(sentence_nouns_dress, Rating = all_sentences_dress[j,"Rating"],
                                               sentiment =  all_sentences_dress[j,"sentiment"])
        fwrite(sentences_nouns_dress, file="nouns", append=TRUE)
      }
    }
  }
}
all_sentences_nouns_dress <- read.csv("nouns")
all_sentences_nouns_dress$token <- as.character(all_sentences_nouns_dress$token)

#Plot frequencies of nouns in neg sentences
all_neg_sentences_nouns_dress <- all_sentences_nouns_dress %>%
  filter(all_sentences_nouns_dress$sentiment<0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")
all_neg_sentences_nouns_dress %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurrences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (negative sentences)")
#Same for positive sentences

all_pos_sentences_nouns_dress <- all_sentences_nouns_dress %>%
  filter(all_sentences_nouns_dress$sentiment>0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_pos_sentences_nouns_dress %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (positive sentences)")

#Comparison clouds

avgsentiment_nouns_dress <-
  group_by(all_sentences_nouns_dress, token) %>%
  summarize(m = mean(sentiment), count = n()) %>%
  arrange(desc(abs(m)))
avgsentiment_nouns_dress

#Create actual comparison cloud
avgsentiment_nouns_dress$Positive_nouns <-  avgsentiment_nouns_dress$count*(avgsentiment_nouns_dress$m > 0)
avgsentiment_nouns_dress$Negative_nouns <-  avgsentiment_nouns_dress$count*(avgsentiment_nouns_dress$m < -0)
avgsentiment_nouns_dress <- as.data.frame(avgsentiment_nouns_dress)
rownames(avgsentiment_nouns_dress) <-  avgsentiment_nouns_dress$token

comparison.cloud(avgsentiment_nouns_dress[,c("Positive_nouns","Negative_nouns")], 
                 scale=c(4, 0.5), max.words=100, title.size=1)

#####----Tops----#####
clothing_df_top <- clothing_df %>% filter(Department.Name == "Tops")
###---Histogram of general rating for all products---###
pos_reviews_clothing_top <- clothing_df_top %>% filter(Rating == 5)
neg_reviews_clothing_top<- clothing_df_top %>% filter(Rating == 1|Rating ==2)

ggplot(clothing_df_top,aes(polarity_no_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_top, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_top, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_top,aes(polarity_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_top, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_top, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

#**Sentence-based** sentiment analysis: number of positive sentences - number of negative sentences
#(this code generates some warnings on 'double punctuation')
# This needs to be done by review as it decomposes into sentences
clothing_df_top_1 <- clothing_df_top[,-c(13:19)]
clothing_df_top_1 <- cbind(clothing_df_top_1, sentence_sent_polarity = 0*cbind(1:nrow(clothing_df_top_1)),
                             sentence_sent_syu      = 0*cbind(1:nrow(clothing_df_top_1)),
                             sentence_sent_afinn    = 0*cbind(1:nrow(clothing_df_top_1)),
                             sentence_sent_bing     = 0*cbind(1:nrow(clothing_df_top_1)),
                             sentence_sent_nrc      = 0*cbind(1:nrow(clothing_df_top_1)))

totalNoSentences_top = 0
localSentences_top <- get_sentences(clothing_df_top_1[1,]$Description)
totalNoSentences_top = totalNoSentences_top + length(localSentences_top)
#sum total sign of each sentence
for (j in 1:nrow(clothing_df_top_1 )) {
  # print progress
  if (j%%1000==0){
    print(100*j/nrow(clothing_df_top_1 ))
  }
  localSentences_top <- get_sentences(clothing_df_top_1[j,]$Description)
  totalNoSentences_top = totalNoSentences_top + length(localSentences_top)
  
  clothing_df_top_1[j,]$sentence_sent_polarity <- polarity(localSentences_top)$all[,"polarity"] %>% 
    sign() %>% sum()
  
  #clothing_df_top_1[j,]$sentence_sent_syu <-
  # get_sentiment(localSentences_top, method = "syuzhet", language = "english") %>% sign()  %>% sum()
  
  #clothing_df_top_1[j,]$sentence_sent_afinn <-
  #  get_sentiment(localSentences_top, method = "afinn", language = "english") %>% sign()  %>% sum()
  
  clothing_df_top_1[j,]$sentence_sent_bing <-
    get_sentiment(localSentences_top, method = "bing", language = "english") %>% sign()  %>% sum()
  
  # clothing_df_top_1[j,]$sentence_sent_nrc <-
  # get_sentiment(localSentences_top, method = "nrc", language = "english") %>% sign()  %>% sum()
}
sum(is.na(clothing_df_top_1))
clothing_df_top_1[is.na(clothing_df_top_1)] <- 0

#Save the dataframe for further use
#Get sentence-level sentiment scores
#(this code generates some warnings on 'double punctuation')
save(clothing_df_top_1, file="Saved_clothing_df_top_1.Rda")
load("Saved_clothing_df_top_1.Rda")
write.csv(clothing_df_top_1, "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Text ANalytics/Assignment 1/clothing_df_top_1.csv",row.names = F)

top <- as.data.frame(clothing_df_top_1)
all_sentences_top <- data.frame(sentence    = rep(0, totalNoSentences_top),
                                  sentiment   = rep(0, totalNoSentences_top),
                                  polarity    = rep(0, totalNoSentences_top),
                                  Clothing.ID     = rep(a[1,"Clothing.ID"],totalNoSentences_top),
                                  Rating = rep(a[1,"Rating"], totalNoSentences_top), 
                                  stringsAsFactors = FALSE)

sentenceIndex_top = 1
for (j in 1:nrow(top)) {
  # show some progress 
  if (j%%2000==0){
    print(100*j/nrow(top))
  }
  
  localSentences_top <- top[j,]$Description %>% get_sentences()
  
  all_sentences_top[sentenceIndex_top : (sentenceIndex_top+length(localSentences_top)-1), "sentence"] <- localSentences_top
  
  all_sentences_top[sentenceIndex_top : (sentenceIndex_top+length(localSentences_top)-1), "sentiment"] <- get_sentiment(localSentences_top, method = "bing")
  all_sentences_top[sentenceIndex_top : (sentenceIndex_top+length(localSentences_top)-1), "polarity"] <- polarity(localSentences_top)$all[,"polarity"]
  
  all_sentences_top[sentenceIndex_top : (sentenceIndex_top+length(localSentences_top)-1), "Clothing.ID"]  <- top[j,]$Clothing.ID
  all_sentences_top[sentenceIndex_top : (sentenceIndex_top+length(localSentences_top)-1), "Rating"] <- top[j,]$Rating
  
  sentenceIndex_top = sentenceIndex_top + length(localSentences_top)
}

#Split into positive and negative sentences

save(all_sentences_top, file="Saved_all_sentences_top.Rda")
load("Saved_all_sentences_top.Rda")
all_pos_sentences_top <- all_sentences_top  %>% filter(polarity>0)
all_neg_sentences_top <- all_sentences_top %>% filter(polarity<0)

#Remove stop words and look at words that occur often in pos or neg sentences

all_neg_sentences_words_top <- all_neg_sentences_top  %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")
all_pos_sentences_words_top <- all_pos_sentences_top %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")

# Get counts of words in pos (and neg) sentences
all_sentence_words_top <- full_join(all_pos_sentences_words_top %>% count(word, sort=TRUE),
                                      all_neg_sentences_words_top %>% count(word, sort=TRUE),
                                      by="word")
all_sentence_words_top[is.na(all_sentence_words_top$n.x), "n.x"] <- 0
all_sentence_words_top[is.na(all_sentence_words_top$n.y), "n.y"] <- 0

# Normalize counts by total number of words in each group and calculate ratio
all_sentence_words_top$n.x  <- all_sentence_words_top$n.x/sum(all_sentence_words_top$n.x)
all_sentence_words_top$n.y  <- all_sentence_words_top$n.y/sum(all_sentence_words_top$n.y)
all_sentence_words_top$diff <- all_sentence_words_top$n.x-all_sentence_words_top$n.y

all_sentence_words_top%>%
  mutate(word = reorder(word, -diff)) %>%           
  top_n(-50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific negative words")

all_sentence_words_top%>%
  mutate(word = reorder(word,diff)) %>%           
  top_n(50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific positive words")

#Focus op nouns: First create a "POS tagger"
library(RDRPOSTagger)
POS_specs_top <- rdr_model(language = "English", annotation =  "POS")

#Identify all nouns and save them in a file (in data.frame is very slow, file is also slow (but a bit less so))
#Write header of file

outFile_top <- file("nouns", open="w")
write("doc_id,token_id,token,pos,Rating,sentiment", outFile_top);
close(outFile_top)
# Process all sentences (POS tagger is really specific on bad sentences, 
# so we do additional pre-processing)
library(data.table)

startIndex_top = 1
for (j in startIndex_top:nrow(all_sentences_top)) {
  if (j%%1000==0){
    print(100*j/nrow(all_sentences_top))
  }
  to_analyse_top <-  all_sentences_top[j,"sentence"]  %>% 
    {gsub("(^ *[,\\.)]+)+","",.)} %>%  # remove leading , or . or ) (also repetitions)
    {gsub("(\\(|\\))"," ", .)}         # remove ( and )
  to_analyse_top <-  to_analyse_top  %>% gsub("  +"," ", .) # remove double spaces
  
  if(nchar(to_analyse_top)>1) {
    
    sentence_nouns_top <- tryCatch(
      rdr_pos(POS_specs_top, to_analyse_top,  doc_id = all_sentences_top[j,"Clothing.ID"]),
      error = function(e) 
        print(cat(" POS fails on: ", to_analyse_top, " sentence: ", j ))
    )
    if (!is.null(sentence_nouns_top))
    {
      sentence_nouns_top <- sentence_nouns_top %>% filter(pos == "NN")
      if (nrow(sentence_nouns_top)>0)
      {
        sentences_nouns_top <- data.frame(sentence_nouns_top, Rating = all_sentences_top[j,"Rating"],
                                            sentiment =  all_sentences_top[j,"sentiment"])
        fwrite(sentences_nouns_top, file="nouns", append=TRUE)
      }
    }
  }
}
all_sentences_nouns_top <- read.csv("nouns")
all_sentences_nouns_top$token <- as.character(all_sentences_nouns_top$token)

#Plot frequencies of nouns in neg sentences
all_neg_sentences_nouns_top <- all_sentences_nouns_top %>%
  filter(all_sentences_nouns_top$sentiment<0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")
all_neg_sentences_nouns_top %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurrences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (negative sentences)")
#Same for positive sentences

all_pos_sentences_nouns_top <- all_sentences_nouns_top %>%
  filter(all_sentences_nouns_top$sentiment>0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_pos_sentences_nouns_top %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (positive sentences)")

#Comparison clouds

avgsentiment_nouns_top <-
  group_by(all_sentences_nouns_top, token) %>%
  summarize(m = mean(sentiment), count = n()) %>%
  arrange(desc(abs(m)))
avgsentiment_nouns_top

#Create actual comparison cloud
avgsentiment_nouns_top$Positive_nouns <-  avgsentiment_nouns_top$count*(avgsentiment_nouns_top$m > 0)
avgsentiment_nouns_top$Negative_nouns <-  avgsentiment_nouns_top$count*(avgsentiment_nouns_top$m < -0)
avgsentiment_nouns_top <- as.data.frame(avgsentiment_nouns_top)
rownames(avgsentiment_nouns_top) <-  avgsentiment_nouns_top$token

comparison.cloud(avgsentiment_nouns_top[,c("Positive_nouns","Negative_nouns")], 
                 scale=c(4, 0.5), max.words=100, title.size=1)

#####----Bottoms----#####
clothing_df_bot <- clothing_df %>% filter(Department.Name == "Bottoms")
###---Histogram of general rating for all products---###
pos_reviews_clothing_bot <- clothing_df_bot %>% filter(Rating == 5)
neg_reviews_clothing_bot<- clothing_df_bot %>% filter(Rating == 1|Rating ==2)

ggplot(clothing_df_bot,aes(polarity_no_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_bot, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_bot, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_bot,aes(polarity_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_bot, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_bot, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

#**Sentence-based** sentiment analysis: number of positive sentences - number of negative sentences
#(this code generates some warnings on 'double punctuation')
# This needs to be done by review as it decomposes into sentences
clothing_df_bot_1 <- clothing_df_bot[,-c(13:19)]
clothing_df_bot_1 <- cbind(clothing_df_bot_1, sentence_sent_polarity = 0*cbind(1:nrow(clothing_df_bot_1)),
                           sentence_sent_syu      = 0*cbind(1:nrow(clothing_df_bot_1)),
                           sentence_sent_afinn    = 0*cbind(1:nrow(clothing_df_bot_1)),
                           sentence_sent_bing     = 0*cbind(1:nrow(clothing_df_bot_1)),
                           sentence_sent_nrc      = 0*cbind(1:nrow(clothing_df_bot_1)))

totalNoSentences_bot = 0
localSentences_bot <- get_sentences(clothing_df_bot_1[1,]$Description)
totalNoSentences_bot = totalNoSentences_bot + length(localSentences_bot)
#sum total sign of each sentence
for (j in 1:nrow(clothing_df_bot_1 )) {
  # print progress
  if (j%%1000==0){
    print(100*j/nrow(clothing_df_bot_1 ))
  }
  localSentences_bot <- get_sentences(clothing_df_bot_1[j,]$Description)
  totalNoSentences_bot = totalNoSentences_bot + length(localSentences_bot)
  
  clothing_df_bot_1[j,]$sentence_sent_polarity <- polarity(localSentences_bot)$all[,"polarity"] %>% 
    sign() %>% sum()
  
  #clothing_df_bot_1[j,]$sentence_sent_syu <-
  # get_sentiment(localSentences_bot, method = "syuzhet", language = "english") %>% sign()  %>% sum()
  
  #clothing_df_bot_1[j,]$sentence_sent_afinn <-
  #  get_sentiment(localSentences_bot, method = "afinn", language = "english") %>% sign()  %>% sum()
  
  clothing_df_bot_1[j,]$sentence_sent_bing <-
    get_sentiment(localSentences_bot, method = "bing", language = "english") %>% sign()  %>% sum()
  
  # clothing_df_top_1[j,]$sentence_sent_nrc <-
  # get_sentiment(localSentences_top, method = "nrc", language = "english") %>% sign()  %>% sum()
}
sum(is.na(clothing_df_bot_1))
clothing_df_bot_1[is.na(clothing_df_bot_1)] <- 0

#Save the dataframe for further use
#Get sentence-level sentiment scores
#(this code generates some warnings on 'double punctuation')
save(clothing_df_bot_1, file="Saved_clothing_df_bot_1.Rda")
load("Saved_clothing_df_bot_1.Rda")
write.csv(clothing_df_bot_1, "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Text ANalytics/Assignment 1/clothing_df_bot_1.csv",row.names = F)

bot <- as.data.frame(clothing_df_bot_1)
all_sentences_bot <- data.frame(sentence    = rep(0, totalNoSentences_bot),
                                sentiment   = rep(0, totalNoSentences_bot),
                                polarity    = rep(0, totalNoSentences_bot),
                                Clothing.ID     = rep(bot[1,"Clothing.ID"],totalNoSentences_bot),
                                Rating = rep(bot[1,"Rating"], totalNoSentences_bot), 
                                stringsAsFactors = FALSE)

sentenceIndex_bot = 1
for (j in 1:nrow(bot)) {
  # show some progress 
  if (j%%2000==0){
    print(100*j/nrow(bot))
  }
  
  localSentences_bot <- bot[j,]$Description %>% get_sentences()
  
  all_sentences_bot[sentenceIndex_bot : (sentenceIndex_bot+length(localSentences_bot)-1), "sentence"] <- localSentences_bot
  
  all_sentences_bot[sentenceIndex_bot : (sentenceIndex_bot+length(localSentences_bot)-1), "sentiment"] <- get_sentiment(localSentences_bot, method = "bing")
  all_sentences_bot[sentenceIndex_bot : (sentenceIndex_bot+length(localSentences_bot)-1), "polarity"] <- polarity(localSentences_bot)$all[,"polarity"]
  
  all_sentences_bot[sentenceIndex_bot : (sentenceIndex_bot+length(localSentences_bot)-1), "Clothing.ID"]  <- bot[j,]$Clothing.ID
  all_sentences_bot[sentenceIndex_bot : (sentenceIndex_bot+length(localSentences_bot)-1), "Rating"] <- bot[j,]$Rating
  
  sentenceIndex_bot = sentenceIndex_bot + length(localSentences_bot)
}

#Split into positive and negative sentences

save(all_sentences_bot, file="Saved_all_sentences_bot.Rda")
load("Saved_all_sentences_bot.Rda")
all_pos_sentences_bot <- all_sentences_bot  %>% filter(polarity>0)
all_neg_sentences_bot <- all_sentences_bot %>% filter(polarity<0)

#Remove stop words and look at words that occur often in pos or neg sentences

all_neg_sentences_words_bot <- all_neg_sentences_bot  %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")
all_pos_sentences_words_bot <- all_pos_sentences_bot %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")

# Get counts of words in pos (and neg) sentences
all_sentence_words_bot <- full_join(all_pos_sentences_words_bot %>% count(word, sort=TRUE),
                                    all_neg_sentences_words_bot %>% count(word, sort=TRUE),
                                    by="word")
all_sentence_words_bot[is.na(all_sentence_words_bot$n.x), "n.x"] <- 0
all_sentence_words_bot[is.na(all_sentence_words_bot$n.y), "n.y"] <- 0

# Normalize counts by total number of words in each group and calculate ratio
all_sentence_words_bot$n.x  <- all_sentence_words_bot$n.x/sum(all_sentence_words_bot$n.x)
all_sentence_words_bot$n.y  <- all_sentence_words_bot$n.y/sum(all_sentence_words_bot$n.y)
all_sentence_words_bot$diff <- all_sentence_words_bot$n.x-all_sentence_words_bot$n.y

all_sentence_words_bot%>%
  mutate(word = reorder(word, -diff)) %>%           
  top_n(-50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific negative words")

all_sentence_words_bot%>%
  mutate(word = reorder(word,diff)) %>%           
  top_n(50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific positive words")

#Focus op nouns: First create a "POS tagger"
library(RDRPOSTagger)
POS_specs_bot <- rdr_model(language = "English", annotation =  "POS")

#Identify all nouns and save them in a file (in data.frame is very slow, file is also slow (but a bit less so))
#Write header of file

outFile_bot <- file("nouns", open="w")
write("doc_id,token_id,token,pos,Rating,sentiment", outFile_bot);
close(outFile_bot)
# Process all sentences (POS tagger is really specific on bad sentences, 
# so we do additional pre-processing)
library(data.table)

startIndex_bot = 1
for (j in startIndex_bot:nrow(all_sentences_bot)) {
  if (j%%1000==0){
    print(100*j/nrow(all_sentences_bot))
  }
  to_analyse_bot <-  all_sentences_bot[j,"sentence"]  %>% 
    {gsub("(^ *[,\\.)]+)+","",.)} %>%  # remove leading , or . or ) (also repetitions)
    {gsub("(\\(|\\))"," ", .)}         # remove ( and )
  to_analyse_bot <-  to_analyse_bot  %>% gsub("  +"," ", .) # remove double spaces
  
  if(nchar(to_analyse_bot)>1) {
    
    sentence_nouns_bot <- tryCatch(
      rdr_pos(POS_specs_bot, to_analyse_bot,  doc_id = all_sentences_bot[j,"Clothing.ID"]),
      error = function(e) 
        print(cat(" POS fails on: ", to_analyse_bot, " sentence: ", j ))
    )
    if (!is.null(sentence_nouns_bot))
    {
      sentence_nouns_bot <- sentence_nouns_bot %>% filter(pos == "NN")
      if (nrow(sentence_nouns_bot)>0)
      {
        sentences_nouns_bot <- data.frame(sentence_nouns_bot, Rating = all_sentences_bot[j,"Rating"],
                                          sentiment =  all_sentences_bot[j,"sentiment"])
        fwrite(sentences_nouns_bot, file="nouns", append=TRUE)
      }
    }
  }
}
all_sentences_nouns_bot <- read.csv("nouns")
all_sentences_nouns_bot$token <- as.character(all_sentences_nouns_bot$token)

#Plot frequencies of nouns in neg sentences
all_neg_sentences_nouns_bot <- all_sentences_nouns_bot %>%
  filter(all_sentences_nouns_bot$sentiment<0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_neg_sentences_nouns_bot %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurrences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (negative sentences)")
#Same for positive sentences

all_pos_sentences_nouns_bot <- all_sentences_nouns_bot %>%
  filter(all_sentences_nouns_bot$sentiment>0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_pos_sentences_nouns_bot %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (positive sentences)")

#Comparison clouds

avgsentiment_nouns_bot <-
  group_by(all_sentences_nouns_bot, token) %>%
  summarize(m = mean(sentiment), count = n()) %>%
  arrange(desc(abs(m)))
avgsentiment_nouns_bot

#Create actual comparison cloud
avgsentiment_nouns_bot$Positive_nouns <-  avgsentiment_nouns_bot$count*(avgsentiment_nouns_bot$m > 0)
avgsentiment_nouns_bot$Negative_nouns <-  avgsentiment_nouns_bot$count*(avgsentiment_nouns_bot$m < -0)
avgsentiment_nouns_bot <- as.data.frame(avgsentiment_nouns_bot)
rownames(avgsentiment_nouns_bot) <-  avgsentiment_nouns_bot$token

comparison.cloud(avgsentiment_nouns_bot[,c("Positive_nouns","Negative_nouns")], 
                 scale=c(4, 0.5), max.words=100, title.size=1)

#####-----Intimate-----#####
clothing_df_int <- clothing_df %>% filter(Department.Name == "Intimate")
###---Histogram of general rating for all products---###
pos_reviews_clothing_int <- clothing_df_int %>% filter(Rating == 5)
neg_reviews_clothing_int<- clothing_df_int %>% filter(Rating == 1|Rating ==2)

ggplot(clothing_df_int,aes(polarity_no_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_int, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_int, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_int,aes(polarity_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_int, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_int, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

#**Sentence-based** sentiment analysis: number of positive sentences - number of negative sentences
#(this code generates some warnings on 'double punctuation')
# This needs to be done by review as it decomposes into sentences
clothing_df_int_1 <- clothing_df_int[,-c(13:19)]
clothing_df_int_1 <- cbind(clothing_df_int_1, sentence_sent_polarity = 0*cbind(1:nrow(clothing_df_int_1)),
                           sentence_sent_syu      = 0*cbind(1:nrow(clothing_df_int_1)),
                           sentence_sent_afinn    = 0*cbind(1:nrow(clothing_df_int_1)),
                           sentence_sent_bing     = 0*cbind(1:nrow(clothing_df_int_1)),
                           sentence_sent_nrc      = 0*cbind(1:nrow(clothing_df_int_1)))

totalNoSentences_int = 0
localSentences_int <- get_sentences(clothing_df_int_1[1,]$Description)
totalNoSentences_int = totalNoSentences_int + length(localSentences_int)
#sum total sign of each sentence
for (j in 1:nrow(clothing_df_int_1 )) {
  # print progress
  if (j%%1000==0){
    print(100*j/nrow(clothing_df_int_1 ))
  }
  localSentences_int <- get_sentences(clothing_df_int_1[j,]$Description)
  totalNoSentences_int = totalNoSentences_int + length(localSentences_int)
  
  clothing_df_int_1[j,]$sentence_sent_polarity <- polarity(localSentences_int)$all[,"polarity"] %>% 
    sign() %>% sum()
  
  #clothing_df_int_1[j,]$sentence_sent_syu <-
  # get_sentiment(localSentences_int, method = "syuzhet", language = "english") %>% sign()  %>% sum()
  
  #clothing_df_int_1[j,]$sentence_sent_afinn <-
  #  get_sentiment(localSentences_int, method = "afinn", language = "english") %>% sign()  %>% sum()
  
  clothing_df_int_1[j,]$sentence_sent_bing <-
    get_sentiment(localSentences_int, method = "bing", language = "english") %>% sign()  %>% sum()
  
  # clothing_df_int_1[j,]$sentence_sent_nrc <-
  # get_sentiment(localSentences_int, method = "nrc", language = "english") %>% sign()  %>% sum()
}
sum(is.na(clothing_df_int_1))
clothing_df_int_1[is.na(clothing_df_int_1)] <- 0

#Save the dataframe for further use
#Get sentence-level sentiment scores
#(this code generates some warnings on 'double punctuation')
save(clothing_df_int_1, file="Saved_clothing_df_int_1.Rda")
load("Saved_clothing_df_int_1.Rda")
write.csv(clothing_df_int_1, "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Text ANalytics/Assignment 1/clothing_df_int_1.csv",row.names = F)

int <- as.data.frame(clothing_df_int_1)
all_sentences_int <- data.frame(sentence    = rep(0, totalNoSentences_int),
                                sentiment   = rep(0, totalNoSentences_int),
                                polarity    = rep(0, totalNoSentences_int),
                                Clothing.ID     = rep(int[1,"Clothing.ID"],totalNoSentences_int),
                                Rating = rep(int[1,"Rating"], totalNoSentences_int), 
                                stringsAsFactors = FALSE)

sentenceIndex_int = 1
for (j in 1:nrow(int)) {
  # show some progress 
  if (j%%2000==0){
    print(100*j/nrow(int))
  }
  
  localSentences_int <- int[j,]$Description %>% get_sentences()
  
  all_sentences_int[sentenceIndex_int : (sentenceIndex_int+length(localSentences_int)-1), "sentence"] <- localSentences_int
  
  all_sentences_int[sentenceIndex_int : (sentenceIndex_int+length(localSentences_int)-1), "sentiment"] <- get_sentiment(localSentences_int, method = "bing")
  all_sentences_int[sentenceIndex_int : (sentenceIndex_int+length(localSentences_int)-1), "polarity"] <- polarity(localSentences_int)$all[,"polarity"]
  
  all_sentences_int[sentenceIndex_int : (sentenceIndex_int+length(localSentences_int)-1), "Clothing.ID"]  <- int[j,]$Clothing.ID
  all_sentences_int[sentenceIndex_int : (sentenceIndex_int+length(localSentences_int)-1), "Rating"] <- int[j,]$Rating
  
  sentenceIndex_int = sentenceIndex_int + length(localSentences_int)
}

#Split into positive and negative sentences

save(all_sentences_int, file="Saved_all_sentences_int.Rda")
load("Saved_all_sentences_int.Rda")
all_pos_sentences_int <- all_sentences_int  %>% filter(polarity>0)
all_neg_sentences_int <- all_sentences_int %>% filter(polarity<0)

#Remove stop words and look at words that occur often in pos or neg sentences

all_neg_sentences_words_int <- all_neg_sentences_int  %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")
all_pos_sentences_words_int <- all_pos_sentences_int %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")

# Get counts of words in pos (and neg) sentences
all_sentence_words_int <- full_join(all_pos_sentences_words_int %>% count(word, sort=TRUE),
                                    all_neg_sentences_words_int %>% count(word, sort=TRUE),
                                    by="word")
all_sentence_words_int[is.na(all_sentence_words_int$n.x), "n.x"] <- 0
all_sentence_words_int[is.na(all_sentence_words_int$n.y), "n.y"] <- 0


# Normalize counts by total number of words in each group and calculate ratio
all_sentence_words_int$n.x  <- all_sentence_words_int$n.x/sum(all_sentence_words_int$n.x)
all_sentence_words_int$n.y  <- all_sentence_words_int$n.y/sum(all_sentence_words_int$n.y)
all_sentence_words_int$diff <- all_sentence_words_int$n.x-all_sentence_words_int$n.y

all_sentence_words_int%>%
  mutate(word = reorder(word, -diff)) %>%           
  top_n(-50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific negative words")

all_sentence_words_int%>%
  mutate(word = reorder(word,diff)) %>%           
  top_n(50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific positive words")

#Focus op nouns: First create a "POS tagger"
library(RDRPOSTagger)
POS_specs_int <- rdr_model(language = "English", annotation =  "POS")

#Identify all nouns and save them in a file (in data.frame is very slow, file is also slow (but a bit less so))
#Write header of file

outFile_int <- file("nouns", open="w")
write("doc_id,token_id,token,pos,Rating,sentiment", outFile_int);
close(outFile_int)
# Process all sentences (POS tagger is really specific on bad sentences, 
# so we do additional pre-processing)
library(data.table)

startIndex_int = 1
for (j in startIndex_bot:nrow(all_sentences_int)) {
  if (j%%1000==0){
    print(100*j/nrow(all_sentences_int))
  }
  to_analyse_int <-  all_sentences_int[j,"sentence"]  %>% 
    {gsub("(^ *[,\\.)]+)+","",.)} %>%  # remove leading , or . or ) (also repetitions)
    {gsub("(\\(|\\))"," ", .)}         # remove ( and )
  to_analyse_int <-  to_analyse_int  %>% gsub("  +"," ", .) # remove double spaces
  
  if(nchar(to_analyse_int)>1) {
    
    sentence_nouns_int <- tryCatch(
      rdr_pos(POS_specs_int, to_analyse_int,  doc_id = all_sentences_int[j,"Clothing.ID"]),
      error = function(e) 
        print(cat(" POS fails on: ", to_analyse_int, " sentence: ", j ))
    )
    if (!is.null(sentence_nouns_int))
    {
      sentence_nouns_int <- sentence_nouns_int %>% filter(pos == "NN")
      if (nrow(sentence_nouns_int)>0)
      {
        sentences_nouns_int <- data.frame(sentence_nouns_int, Rating = all_sentences_int[j,"Rating"],
                                          sentiment =  all_sentences_int[j,"sentiment"])
        fwrite(sentences_nouns_int, file="nouns", append=TRUE)
      }
    }
  }
}
all_sentences_nouns_int <- read.csv("nouns")
all_sentences_nouns_int$token <- as.character(all_sentences_nouns_int$token)

#Plot frequencies of nouns in neg sentences
all_neg_sentences_nouns_int <- all_sentences_nouns_int %>%
  filter(all_sentences_nouns_int$sentiment<0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_neg_sentences_nouns_int %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurrences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (negative sentences)")
#Same for positive sentences

all_pos_sentences_nouns_int <- all_sentences_nouns_int %>%
  filter(all_sentences_nouns_int$sentiment>0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_pos_sentences_nouns_int %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (positive sentences)")

#Comparison clouds

avgsentiment_nouns_int <-
  group_by(all_sentences_nouns_int, token) %>%
  summarize(m = mean(sentiment), count = n()) %>%
  arrange(desc(abs(m)))
avgsentiment_nouns_int

#Create actual comparison cloud
avgsentiment_nouns_int$Positive_nouns <-  avgsentiment_nouns_int$count*(avgsentiment_nouns_int$m > 0)
avgsentiment_nouns_int$Negative_nouns <-  avgsentiment_nouns_int$count*(avgsentiment_nouns_int$m < -0)
avgsentiment_nouns_int <- as.data.frame(avgsentiment_nouns_int)
rownames(avgsentiment_nouns_int) <-  avgsentiment_nouns_int$token

comparison.cloud(avgsentiment_nouns_int[,c("Positive_nouns","Negative_nouns")], 
                 scale=c(4, 0.5), max.words=100, title.size=1)

####----Jacket----####
clothing_df_jac <- clothing_df %>% filter(Department.Name == "Jackets")
###---Histogram of general rating for all products---###
pos_reviews_clothing_jac <- clothing_df_jac %>% filter(Rating == 5)
neg_reviews_clothing_jac <- clothing_df_jac %>% filter(Rating == 1|Rating ==2)

ggplot(clothing_df_jac,aes(polarity_no_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_jac, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_jac, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

ggplot(clothing_df_jac,aes(polarity_valence)) +
  geom_density(aes(fill = "Happy"), data =  pos_reviews_clothing_jac, alpha = 0.5) +
  geom_density(aes(fill = "Unhappy"), data = neg_reviews_clothing_jac, alpha = 0.5) +
  scale_colour_manual("Polarity", values = c("green", "red"), aesthetics = "fill")

#**Sentence-based** sentiment analysis: number of positive sentences - number of negative sentences
#(this code generates some warnings on 'double punctuation')
# This needs to be done by review as it decomposes into sentences
clothing_df_jac_1 <- clothing_df_jac[,-c(13:19)]
clothing_df_jac_1 <- cbind(clothing_df_jac_1, sentence_sent_polarity = 0*cbind(1:nrow(clothing_df_jac_1)),
                           sentence_sent_syu      = 0*cbind(1:nrow(clothing_df_jac_1)),
                           sentence_sent_afinn    = 0*cbind(1:nrow(clothing_df_jac_1)),
                           sentence_sent_bing     = 0*cbind(1:nrow(clothing_df_jac_1)),
                           sentence_sent_nrc      = 0*cbind(1:nrow(clothing_df_jac_1)))

totalNoSentences_jac = 0
localSentences_jac <- get_sentences(clothing_df_jac_1[1,]$Description)
totalNoSentences_jac = totalNoSentences_jac + length(localSentences_jac)
#sum total sign of each sentence
for (j in 1:nrow(clothing_df_jac_1 )) {
  # print progress
  if (j%%1000==0){
    print(100*j/nrow(clothing_df_jac_1 ))
  }
  localSentences_jac <- get_sentences(clothing_df_jac_1[j,]$Description)
  totalNoSentences_jac = totalNoSentences_jac + length(localSentences_jac)
  
  clothing_df_jac_1[j,]$sentence_sent_polarity <- polarity(localSentences_jac)$all[,"polarity"] %>% 
    sign() %>% sum()
  
  #clothing_df_jac_1[j,]$sentence_sent_syu <-
  # get_sentiment(localSentences_jac, method = "syuzhet", language = "english") %>% sign()  %>% sum()
  
  #clothing_df_jac_1[j,]$sentence_sent_afinn <-
  #  get_sentiment(localSentences_jac, method = "afinn", language = "english") %>% sign()  %>% sum()
  
  clothing_df_jac_1[j,]$sentence_sent_bing <-
    get_sentiment(localSentences_jac, method = "bing", language = "english") %>% sign()  %>% sum()
  
  # clothing_df_int_1[j,]$sentence_sent_nrc <-
  # get_sentiment(localSentences_int, method = "nrc", language = "english") %>% sign()  %>% sum()
}
sum(is.na(clothing_df_jac_1))
clothing_df_jac_1[is.na(clothing_df_jac_1)] <- 0

#Save the dataframe for further use
#Get sentence-level sentiment scores
#(this code generates some warnings on 'double punctuation')
save(clothing_df_jac_1, file="Saved_clothing_df_jac_1.Rda")
load("Saved_clothing_df_jac_1.Rda")
write.csv(clothing_df_jac_1, "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Text ANalytics/Assignment 1/clothing_df_jac_1.csv",row.names = F)

jac <- as.data.frame(clothing_df_jac_1)
all_sentences_jac <- data.frame(sentence    = rep(0, totalNoSentences_jac),
                                sentiment   = rep(0, totalNoSentences_jac),
                                polarity    = rep(0, totalNoSentences_jac),
                                Clothing.ID     = rep(jac[1,"Clothing.ID"],totalNoSentences_jac),
                                Rating = rep(jac[1,"Rating"], totalNoSentences_jac), 
                                stringsAsFactors = FALSE)

sentenceIndex_jac = 1
for (j in 1:nrow(jac)) {
  # show some progress 
  if (j%%2000==0){
    print(100*j/nrow(jac))
  }
  
  localSentences_jac <- jac[j,]$Description %>% get_sentences()
  
  all_sentences_jac[sentenceIndex_jac : (sentenceIndex_jac+length(localSentences_jac)-1), "sentence"] <- localSentences_jac
  
  all_sentences_jac[sentenceIndex_jac : (sentenceIndex_jac+length(localSentences_jac)-1), "sentiment"] <- get_sentiment(localSentences_jac, method = "bing")
  all_sentences_jac[sentenceIndex_jac : (sentenceIndex_jac+length(localSentences_jac)-1), "polarity"] <- polarity(localSentences_jac)$all[,"polarity"]
  
  all_sentences_jac[sentenceIndex_jac : (sentenceIndex_jac+length(localSentences_jac)-1), "Clothing.ID"]  <- jac[j,]$Clothing.ID
  all_sentences_jac[sentenceIndex_jac : (sentenceIndex_jac+length(localSentences_jac)-1), "Rating"] <- jac[j,]$Rating
  
  sentenceIndex_jac = sentenceIndex_jac + length(localSentences_jac)
}

#Split into positive and negative sentences

save(all_sentences_jac, file="Saved_all_sentences_jac.Rda")
load("Saved_all_sentences_jac.Rda")
all_pos_sentences_jac <- all_sentences_jac  %>% filter(polarity>0)
all_neg_sentences_jac <- all_sentences_jac %>% filter(polarity<0)

#Remove stop words and look at words that occur often in pos or neg sentences

all_neg_sentences_words_jac <- all_neg_sentences_jac  %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")
all_pos_sentences_words_jac <- all_pos_sentences_jac %>%
  unnest_tokens(word, sentence) %>%
  anti_join(stop_words, by = "word")

# Get counts of words in pos (and neg) sentences
all_sentence_words_jac <- full_join(all_pos_sentences_words_jac %>% count(word, sort=TRUE),
                                    all_neg_sentences_words_jac %>% count(word, sort=TRUE),
                                    by="word")
all_sentence_words_jac[is.na(all_sentence_words_jac$n.x), "n.x"] <- 0
all_sentence_words_jac[is.na(all_sentence_words_jac$n.y), "n.y"] <- 0


# Normalize counts by total number of words in each group and calculate ratio
all_sentence_words_jac$n.x  <- all_sentence_words_jac$n.x/sum(all_sentence_words_jac$n.x)
all_sentence_words_jac$n.y  <- all_sentence_words_jac$n.y/sum(all_sentence_words_jac$n.y)
all_sentence_words_jac$diff <- all_sentence_words_jac$n.x-all_sentence_words_jac$n.y

all_sentence_words_jac%>%
  mutate(word = reorder(word, -diff)) %>%           
  top_n(-50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific negative words")

all_sentence_words_jac%>%
  mutate(word = reorder(word,diff)) %>%           
  top_n(50, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (pos-neg)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific positive words")

#Focus op nouns: First create a "POS tagger"
library(RDRPOSTagger)
POS_specs_jac <- rdr_model(language = "English", annotation =  "POS")

#Identify all nouns and save them in a file (in data.frame is very slow, file is also slow (but a bit less so))
#Write header of file

outFile_jac <- file("nouns", open="w")
write("doc_id,token_id,token,pos,Rating,sentiment", outFile_jac);
close(outFile_jac)
# Process all sentences (POS tagger is really specific on bad sentences, 
# so we do additional pre-processing)
library(data.table)

startIndex_jac = 1
for (j in startIndex_jac:nrow(all_sentences_jac)) {
  if (j%%1000==0){
    print(100*j/nrow(all_sentences_jac))
  }
  to_analyse_jac <-  all_sentences_jac[j,"sentence"]  %>% 
    {gsub("(^ *[,\\.)]+)+","",.)} %>%  # remove leading , or . or ) (also repetitions)
    {gsub("(\\(|\\))"," ", .)}         # remove ( and )
  to_analyse_jac <-  to_analyse_jac  %>% gsub("  +"," ", .) # remove double spaces
  
  if(nchar(to_analyse_jac)>1) {
    
    sentence_nouns_jac <- tryCatch(
      rdr_pos(POS_specs_jac, to_analyse_jac,  doc_id = all_sentences_jac[j,"Clothing.ID"]),
      error = function(e) 
        print(cat(" POS fails on: ", to_analyse_jac, " sentence: ", j ))
    )
    if (!is.null(sentence_nouns_jac))
    {
      sentence_nouns_jac <- sentence_nouns_jac %>% filter(pos == "NN")
      if (nrow(sentence_nouns_jac)>0)
      {
        sentences_nouns_jac <- data.frame(sentence_nouns_jac, Rating = all_sentences_jac[j,"Rating"],
                                          sentiment =  all_sentences_jac[j,"sentiment"])
        fwrite(sentences_nouns_jac, file="nouns", append=TRUE)
      }
    }
  }
}
all_sentences_nouns_jac <- read.csv("nouns")
all_sentences_nouns_jac$token <- as.character(all_sentences_nouns_jac$token)

#Plot frequencies of nouns in neg sentences
all_neg_sentences_nouns_jac <- all_sentences_nouns_jac %>%
  filter(all_sentences_nouns_jac$sentiment<0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_neg_sentences_nouns_jac %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurrences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (negative sentences)")
#Same for positive sentences

all_pos_sentences_nouns_jac <- all_sentences_nouns_jac %>%
  filter(all_sentences_nouns_jac$sentiment>0) %>%
  unnest_tokens(word,token) %>%
  anti_join(stop_words, by = "word")

all_pos_sentences_nouns_jac %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(50, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Nouns Frequency Histogram (positive sentences)")

#Comparison clouds

avgsentiment_nouns_jac <-
  group_by(all_sentences_nouns_jac, token) %>%
  summarize(m = mean(sentiment), count = n()) %>%
  arrange(desc(abs(m)))
avgsentiment_nouns_jac

#Create actual comparison cloud
avgsentiment_nouns_jac$Positive_nouns <-  avgsentiment_nouns_jac$count*(avgsentiment_nouns_jac$m > 0)
avgsentiment_nouns_jac$Negative_nouns <-  avgsentiment_nouns_jac$count*(avgsentiment_nouns_jac$m < -0)
avgsentiment_nouns_jac <- as.data.frame(avgsentiment_nouns_jac)
rownames(avgsentiment_nouns_jac) <-  avgsentiment_nouns_jac$token

comparison.cloud(avgsentiment_nouns_jac[,c("Positive_nouns","Negative_nouns")], 
                 scale=c(4, 0.5), max.words=100, title.size=1)
