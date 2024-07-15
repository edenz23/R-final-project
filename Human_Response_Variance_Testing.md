---
title: "Human_Response_Variance_Testing"
author: "Team 8"
date: "2024-06-10"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Import survey data and run tests on it

```{r load-packages, message = FALSE}
library(tidyverse)
library(qdap)
library(sentimentr)
```

## Modifications on datasets from originals:

#### survey_srs.csv - email templates -\> survey_srs_with_GPT_responses.csv

-   add ChatGPT response for each email - for comparison and analysis

-   add 'Weighted sentiment', 'Number of words' and 'Flesch reading ease score' measurements for each one

#### survey_responses_anon.tsv - user responses

-   add 'Weighted sentiment', 'Number of words' and 'Flesch reading ease score' measurements for each response

-   filter invalid responses, that cannot run through processing

-   calculate response rate ("999" appearnses) for each email by each user's response to that email

```{r}
file_list <- list.files(path = "data", pattern = "\\.[tc]sv$", full.names = TRUE)
```

```{r}
# The email templates used in the survey, smart responses and GPT.
data_with_gpt <- read.csv(file_list[2])
```

```{r}
# The user responses dataset
user_responses <- read.csv(file_list[1], sep = "\t")
```

```{r, warning=FALSE}
# this section shows how many "999" are in each email - marking the number of responses people would have not answered if they werent requiered to.
email_responses_fields <- c("e_frnd_1", "e_frnd_2", "e_frnd_3", "e_frnd_4", "e_frnd_5", "e_fmly_1", "e_fmly_2", "e_fmly_3", "e_fmly_4", "e_fmly_5", "e_work_1", "e_work_2", "e_work_3", "e_work_4", "e_work_5", "e_other_1", "e_other_2", "e_other_3", "e_other_4", "e_other_5")

count_num_of_999 <- function(email_qid) {
  all_responses_for_email <- user_responses %>%
    select(email_qid)
  
  num_of_999 <- 0
  counter <- 1
  while (counter <= 240) {  # go over all 240 emails
    h <- nth(all_responses_for_email, counter)
    is_999_present <- str_detect(h, regex(".*999.*", ignore_case = TRUE))
    if (is_999_present == TRUE) {
      num_of_999 = num_of_999 + 1
    }
    counter <- counter + 1
  }
  return(num_of_999)
}

for (eml in email_responses_fields) {
  num_of_999 <- count_num_of_999(eml)
  cat("for email -", eml," there are:", num_of_999, "'999' appearences, that is:", round(num_of_999/240*100, 2), "% of all responses\n")
  # add the number as a new field to DB 'survey_srs'
}

```

```{r}
# set functions to use as rowwise calculations on the user responses + SR and GPT

calc_sentiment <- function(string) {  # function to calculate the setiment of a given sentence
  sentiment(string) %>%
    group_by(element_id) %>%  
    summarise(mean_sentiment = mean(sentiment)) %>%
    pluck("mean_sentiment", 1)
}

calc_num_of_words <- function(string) {  # function to calculate the number of words of a given sentence
  num_of_words_df <- sentiment(string) %>%
                      select(word_count)
  return(sum(num_of_words_df, na.rm=TRUE))
}

calc_Flesch_reading_ease_score <- function(string) {  # function to calculate the Flesch_reading_ease of a given sentence
  
  tryCatch(
    expr = {
        num_of_words <- calc_num_of_words(string)
        num_of_syllables <- syllable_sum(string)
        num_of_sentences <- sentiment(string) %>% count(element_id) %>% pluck("n", 1)
        
        return(206.835 - 1.015*(num_of_words/num_of_sentences) - 84.6*(num_of_syllables/num_of_words))  # the higher - the easier to read (opposite correlation)
    },
    error = function(e){ 
        return(-10000)  # return defulat value of -10000 in case something went wrong
    }
  )
}

v_calc_Flesch_reading_ease_score <- Vectorize(calc_Flesch_reading_ease_score)
```

```{r}
# add analysis calculation results to the table
data_with_GPT_sr_analysis <- data_with_gpt %>%
  rowwise() %>%
  mutate(GPT_sentiment = calc_sentiment(GPT)) %>%  # calculate the sentiment score of the GPT response
  mutate(GPT_num_of_words = calc_num_of_words(GPT)) %>% # calculate the number of words of the GPT response
  mutate(GPT_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(GPT)) %>% # calculate the Flesch_reading_ease score of the GPT response
  mutate(sr1_sentiment = calc_sentiment(sr1)) %>%  
  mutate(sr1_num_of_words = calc_num_of_words(sr1)) %>% 
  mutate(sr1_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(sr1)) %>%
  mutate(sr2_sentiment = calc_sentiment(sr2)) %>%  
  mutate(sr2_num_of_words = calc_num_of_words(sr2)) %>% 
  mutate(sr2_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(sr2)) %>%
  mutate(sr3_sentiment = calc_sentiment(sr3)) %>%  
  mutate(sr3_num_of_words = calc_num_of_words(sr3)) %>% 
  mutate(sr3_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(sr3))
```

```{r}
# comparison stats between the most and least responded emails
user_responses_with_frnd_5_other_1_analysis <- user_responses %>%
  select(ResponseId, e_frnd_5,e_other_1) %>%
  rowwise() %>%
  mutate(e_frnd_5_sentiment = calc_sentiment(e_frnd_5)) %>%  # calculate the sentiment score of the GPT response
  mutate(e_frnd_5_num_of_words = calc_num_of_words(e_frnd_5)) %>% # calculate the number of words of the GPT response
  mutate(e_frnd_5_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(e_frnd_5)) %>%
  mutate(e_other_1_sentiment = calc_sentiment(e_other_1)) %>%  # calculate the sentiment score of the GPT response
  mutate(e_other_1_num_of_words = calc_num_of_words(e_other_1)) %>% # calculate the number of words of the GPT response
  mutate(e_other_1_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(e_other_1)) %>%
  filter(e_other_1_Flesch_reading_ease != -10000) %>% # remove cases where result had an error (results with error are set to value: -10000)
  filter(e_frnd_5_Flesch_reading_ease != -10000) # remove cases where result had an error (results with error are set to value: -10000)


```

```{r}
# plotting the comparison
ggplot(user_responses_with_frnd_5_other_1_analysis) + 
  geom_density(aes(x=e_other_1_num_of_words, fill="Least responded email: 27.92%"), alpha=0.7) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_5_num_of_words, fill="Most responded email: 86.25%"), alpha=0.7) + # density for num_of_words of all user responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(GPT_num_of_words) %>% mean(), colour = "blue") + # mean num of words for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr1_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr2_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr3_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr3 results
  labs(title = "People's response variance between most responded and least responded emails", subtitle = "differentiated by number of words", x="Number of words")

ggplot(user_responses_with_frnd_5_other_1_analysis) + 
  geom_density(aes(x=e_other_1_Flesch_reading_ease, fill="Least responded email: 27.92%"), alpha=0.7) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_5_Flesch_reading_ease, fill="Most responded email: 86.25%"), alpha=0.7) + # density for num_of_words of all user responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(GPT_Flesch_reading_ease) %>% mean(), colour = "blue") + # mean num of words for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr1_Flesch_reading_ease) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr2_Flesch_reading_ease) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr3_Flesch_reading_ease) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr3 results
  labs(title = "People's response variance between most responded and least responded emails", subtitle = "differentiated by Flesch reading ease score", x="Readability score")

ggplot(user_responses_with_frnd_5_other_1_analysis) + 
  geom_density(aes(x=e_other_1_sentiment, fill="Least responded email: 27.92%"), alpha=0.7) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_5_sentiment, fill="Most responded email: 86.25%"), alpha=0.7) + # density for num_of_words of all user responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(GPT_sentiment) %>% mean(), colour = "blue") + # mean num of words for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr1_sentiment) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr2_sentiment) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr3_sentiment) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr3 results
  labs(title = "People's response variance between most responded and least responded emails", subtitle = "differentiated by Sentiment analysis", x="Sentiment score")
```

```{r}
# combine all 5 frnd emails to 1 dataframe for analysis
user_responses_with_frnd_analysis <- user_responses %>%
  select(ResponseId, e_frnd_1, e_frnd_2, e_frnd_3, e_frnd_4, e_frnd_5) %>%
  rowwise() %>%
  mutate(e_frnd_1_sentiment = calc_sentiment(e_frnd_1)) %>% # calculate the sentiment score of the GPT response
  mutate(e_frnd_1_num_of_words = calc_num_of_words(e_frnd_1)) %>% # calculate the number of words of the GPT response
  mutate(e_frnd_1_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(e_frnd_1)) %>%
  mutate(e_frnd_2_sentiment = calc_sentiment(e_frnd_2)) %>% 
  mutate(e_frnd_2_num_of_words = calc_num_of_words(e_frnd_2)) %>% 
  mutate(e_frnd_2_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(e_frnd_2)) %>%
  mutate(e_frnd_3_sentiment = calc_sentiment(e_frnd_3)) %>% 
  mutate(e_frnd_3_num_of_words = calc_num_of_words(e_frnd_3)) %>% 
  mutate(e_frnd_3_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(e_frnd_3)) %>%
  mutate(e_frnd_4_sentiment = calc_sentiment(e_frnd_4)) %>% 
  mutate(e_frnd_4_num_of_words = calc_num_of_words(e_frnd_4)) %>% 
  mutate(e_frnd_4_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(e_frnd_4)) %>%
  mutate(e_frnd_5_sentiment = calc_sentiment(e_frnd_5)) %>% 
  mutate(e_frnd_5_num_of_words = calc_num_of_words(e_frnd_5)) %>% 
  mutate(e_frnd_5_Flesch_reading_ease = v_calc_Flesch_reading_ease_score(e_frnd_5)) %>%
  
  filter(e_frnd_1_Flesch_reading_ease != -10000) %>%
  filter(e_frnd_2_Flesch_reading_ease != -10000) %>%
  filter(e_frnd_3_Flesch_reading_ease != -10000) %>%
  filter(e_frnd_4_Flesch_reading_ease != -10000) %>%
  filter(e_frnd_5_Flesch_reading_ease != -10000) # remove cases where result had an error (results with error are set to value: -10000)

```

```{r}
# plotting the stats for the 5 frnd emails
ggplot(user_responses_with_frnd_analysis) + 
  geom_density(aes(x=e_frnd_1_num_of_words, fill="friend 1"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_2_num_of_words, fill="friend 2"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_3_num_of_words, fill="friend 3"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_4_num_of_words, fill="friend 4"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_5_num_of_words, fill="friend 5"), alpha=0.5) + # density for num_of_words of all user responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(GPT_num_of_words) %>% mean(), colour = "blue") + # mean num of words for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr1_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr2_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr3_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr3 results
  labs(title = "People's response variance between all 'friend' category emails", subtitle = "differentiated by number of words, compared to GPT and SR mean 'friend' results", x="Number of words", y="density") + 
  annotate("label", x=45, y=0.075, label= "friend_1: 85.83% response rate\nfriend_2: 70.00% response rate\nfriend_3: 84.58% response rate\nfriend_4: 72.08% response rate\nfriend_5: 86.25% response rate")

ggplot(user_responses_with_frnd_analysis) + 
  geom_density(aes(x=e_frnd_1_Flesch_reading_ease, fill="friend 1"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_2_Flesch_reading_ease, fill="friend 2"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_3_Flesch_reading_ease, fill="friend 3"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_4_Flesch_reading_ease, fill="friend 4"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_5_Flesch_reading_ease, fill="friend 5"), alpha=0.5) + # density for num_of_words of all user responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(GPT_Flesch_reading_ease) %>% mean(), colour = "blue") + # mean num of words for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr1_Flesch_reading_ease) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr2_Flesch_reading_ease) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr3_Flesch_reading_ease) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr3 results
  labs(title = "People's response variance between all 'friend' category emails", subtitle = "differentiated by Flesch_reading_ease, compared to GPT and SR mean 'friend' results", x="Flesch reading ease", y="density") + 
  annotate("label", x=-50, y=0.02, label= "friend_1: 85.83% response rate\nfriend_2: 70.00% response rate\nfriend_3: 84.58% response rate\nfriend_4: 72.08% response rate\nfriend_5: 86.25% response rate")

ggplot(user_responses_with_frnd_analysis) + 
  geom_density(aes(x=e_frnd_1_sentiment, fill="friend 1"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_2_sentiment, fill="friend 2"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_3_sentiment, fill="friend 3"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_4_sentiment, fill="friend 4"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_frnd_5_sentiment, fill="friend 5"), alpha=0.5) + # density for num_of_words of all user responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(GPT_sentiment) %>% mean(), colour = "blue") + # mean num of words for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr1_sentiment) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr2_sentiment) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_frnd_", qid)) %>% pull(sr3_sentiment) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr3 results
  labs(title = "People's response variance between all 'friend' category emails", subtitle = "differentiated by sentiment, compared to GPT and SR mean 'friend' results", x="Sentiment", y="density") + 
  annotate("label", x=0.805, y=4, label= "friend_1: 85.83% response rate\nfriend_2: 70.00% response rate\nfriend_3: 84.58% response rate\nfriend_4: 72.08% response rate\nfriend_5: 86.25% response rate")
```

```{r}
# combine all 20 emails to 1 dataframe AND only save the average value of each measurment (num_of_words, sentiment, readability score)
user_responses_with_mean_analysis <- user_responses %>%
  select(ResponseId, e_frnd_1, e_frnd_2, e_frnd_3, e_frnd_4, e_frnd_5, e_fmly_1, e_fmly_2, e_fmly_3, e_fmly_4, e_fmly_5, e_work_1, e_work_2, e_work_3, e_work_4, e_work_5, e_other_1, e_other_2, e_other_3, e_other_4, e_other_5) %>%
  rowwise() %>%
  
  mutate(e_frnd_mean_sentiment = (calc_sentiment(e_frnd_1) + calc_sentiment(e_frnd_2) + calc_sentiment(e_frnd_3) + calc_sentiment(e_frnd_4) + calc_sentiment(e_frnd_5))/5) %>% 
  mutate(e_frnd_mean_num_of_words = (calc_num_of_words(e_frnd_1) + calc_num_of_words(e_frnd_2) + calc_num_of_words(e_frnd_3) + calc_num_of_words(e_frnd_4) + calc_num_of_words(e_frnd_5))/5) %>% 
  mutate(e_frnd_mean_Flesch_reading_ease = (v_calc_Flesch_reading_ease_score(e_frnd_1) + v_calc_Flesch_reading_ease_score(e_frnd_2) + v_calc_Flesch_reading_ease_score(e_frnd_3) +      v_calc_Flesch_reading_ease_score(e_frnd_4) + v_calc_Flesch_reading_ease_score(e_frnd_5))/5) %>%
  filter(e_frnd_mean_Flesch_reading_ease > -1000) %>% # remove cases where result had an error (results with error are set to value: -10000, with average we devide by 5, so set a threshold of -1000)
  
  mutate(e_fmly_mean_sentiment = (calc_sentiment(e_fmly_1) + calc_sentiment(e_fmly_2) + calc_sentiment(e_fmly_3) + calc_sentiment(e_fmly_4) + calc_sentiment(e_fmly_5))/5) %>% 
  mutate(e_fmly_mean_num_of_words = (calc_num_of_words(e_fmly_1) + calc_num_of_words(e_fmly_2) + calc_num_of_words(e_fmly_3) + calc_num_of_words(e_fmly_4) + calc_num_of_words(e_fmly_5))/5) %>% 
  mutate(e_fmly_mean_Flesch_reading_ease = (v_calc_Flesch_reading_ease_score(e_fmly_1) + v_calc_Flesch_reading_ease_score(e_fmly_2) + v_calc_Flesch_reading_ease_score(e_fmly_3) +      v_calc_Flesch_reading_ease_score(e_fmly_4) + v_calc_Flesch_reading_ease_score(e_fmly_5))/5) %>%
  filter(e_fmly_mean_Flesch_reading_ease > -1000) %>% # remove cases where result had an error (results with error are set to value: -10000, with average we devide by 5, so set a threshold of -1000)
  
  mutate(e_work_mean_sentiment = (calc_sentiment(e_work_1) + calc_sentiment(e_work_2) + calc_sentiment(e_work_3) + calc_sentiment(e_work_4) + calc_sentiment(e_work_5))/5) %>% 
  mutate(e_work_mean_num_of_words = (calc_num_of_words(e_work_1) + calc_num_of_words(e_work_2) + calc_num_of_words(e_work_3) + calc_num_of_words(e_work_4) + calc_num_of_words(e_work_5))/5) %>% 
  mutate(e_work_mean_Flesch_reading_ease = (v_calc_Flesch_reading_ease_score(e_work_1) + v_calc_Flesch_reading_ease_score(e_work_2) + v_calc_Flesch_reading_ease_score(e_work_3) +      v_calc_Flesch_reading_ease_score(e_work_4) + v_calc_Flesch_reading_ease_score(e_work_5))/5) %>%
  filter(e_work_mean_Flesch_reading_ease > -1000) %>% # remove cases where result had an error (results with error are set to value: -10000, with average we devide by 5, so set a threshold of -1000)

  mutate(e_other_mean_sentiment = (calc_sentiment(e_other_1) + calc_sentiment(e_other_2) + calc_sentiment(e_other_3) + calc_sentiment(e_other_4) + calc_sentiment(e_other_5))/5) %>% 
  mutate(e_other_mean_num_of_words = (calc_num_of_words(e_other_1) + calc_num_of_words(e_other_2) + calc_num_of_words(e_other_3) + calc_num_of_words(e_other_4) + calc_num_of_words(e_other_5))/5) %>% 
  mutate(e_other_mean_Flesch_reading_ease = (v_calc_Flesch_reading_ease_score(e_other_1) + v_calc_Flesch_reading_ease_score(e_other_2) + v_calc_Flesch_reading_ease_score(e_other_3) +      v_calc_Flesch_reading_ease_score(e_other_4) + v_calc_Flesch_reading_ease_score(e_other_5))/5) %>%
  filter(e_other_mean_Flesch_reading_ease > -1000) # remove cases where result had an error (results with error are set to value: -10000, with average we devide by 5, so set a threshold of -1000)

```

```{r}
# plotting 20 emails means stats by category and their graphs
ggplot(user_responses_with_mean_analysis) + 
  geom_density(aes(x=e_frnd_mean_num_of_words, fill="friends"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_fmly_mean_num_of_words, fill="family"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_work_mean_num_of_words, fill="work"), alpha=0.5) + # density for num_of_words of all user responses
  geom_density(aes(x=e_other_mean_num_of_words, fill="other"), alpha=0.5) + # density for num_of_words of all user responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(GPT_num_of_words) %>% mean(), colour = "blue") + # mean num of words for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr1_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr2_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr3_num_of_words) %>% mean(), colour = "red") + # mean num of words for ALL frnd sr3 results
  labs(title = "People's response variance between all emails, by category ", subtitle = "differentiated by number of words, compared to GPT and SR mean results", x="Number of words", y="density")

ggplot(user_responses_with_mean_analysis) + 
  geom_density(aes(x=e_frnd_mean_Flesch_reading_ease, fill="friends"), alpha=0.5) + # density for Flesch_reading_ease of all friends responses
  geom_density(aes(x=e_fmly_mean_Flesch_reading_ease, fill="family"), alpha=0.5) + # density for Flesch_reading_ease of all family responses
  geom_density(aes(x=e_work_mean_Flesch_reading_ease, fill="work"), alpha=0.5) + # density for Flesch_reading_ease of all work responses
  geom_density(aes(x=e_other_mean_Flesch_reading_ease, fill="other"), alpha=0.5) + # density for Flesch_reading_ease of all other responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(GPT_Flesch_reading_ease) %>% mean(), colour = "blue") + # mean Flesch_reading_ease for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr1_Flesch_reading_ease) %>% mean(), colour = "red") + # mean Flesch_reading_ease for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr2_Flesch_reading_ease) %>% mean(), colour = "red") + # mean Flesch_reading_ease for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr3_Flesch_reading_ease) %>% mean(), colour = "red") + # mean Flesch_reading_ease for ALL frnd sr3 results
  labs(title = "People's response variance between all emails, by category ", subtitle = "differentiated by Flesch reading ease score, compared to GPT and SR mean results", x="Flesch reading ease score", y="density")

ggplot(user_responses_with_mean_analysis) + 
  geom_density(aes(x=e_frnd_mean_sentiment, fill="friends"), alpha=0.5) + # density for sentiment of all friends responses
  geom_density(aes(x=e_fmly_mean_sentiment, fill="family"), alpha=0.5) + # density for sentiment of all family responses
  geom_density(aes(x=e_work_mean_sentiment, fill="work"), alpha=0.5) + # density for sentiment of all work responses
  geom_density(aes(x=e_other_mean_sentiment, fill="other"), alpha=0.5) + # density for sentiment of all other responses
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(GPT_sentiment) %>% mean(), colour = "blue") + # mean sentiment for ALL frnd GPT results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr1_sentiment) %>% mean(), colour = "red") + # mean sentiment for ALL frnd sr1 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr2_sentiment) %>% mean(), colour = "red") + # mean sentiment for ALL frnd sr2 results
  geom_vline(xintercept = data_with_GPT_sr_analysis %>% filter(grepl("e_", qid)) %>% pull(sr3_sentiment) %>% mean(), colour = "red") + # mean sentiment for ALL frnd sr3 results
  labs(title = "People's response variance between all emails, by category ", subtitle = "differentiated by sentiment score, compared to GPT and SR mean results", x="sentiment", y="density")
```
