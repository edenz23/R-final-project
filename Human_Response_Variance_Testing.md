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



```{r}
users_data <- user_responses %>%
  select(all_of(email_responses_fields))
print(head(users_data))
```




```{r}
library(dplyr)
library(stringr)
library(spacyr)
library(politeness)
# Select necessary fields
users_data <- user_responses %>%
  select(all_of(email_responses_fields))
# Function to calculate average length for a single column
calculate_average_length <- function(text) {
  mean(str_count(text, "\\w+"), na.rm = TRUE)
}
# Initialize an empty dataframe to store summarized results
length_df <- data.frame(question = character(), average_length = numeric(), stringsAsFactors = FALSE)
# Loop through each selected column and calculate average length
for (col in colnames(users_data)) {
  column_data <- users_data[[col]]
  # Ensure the column data is a character vector
  column_data <- as.character(column_data)
  # Remove occurrences of "999"
  column_data <- str_replace_all(column_data, "999", "")
  # Calculate average length
  avg_length <- calculate_average_length(column_data)
  # Store the result in a dataframe
  length_df <- rbind(length_df, data.frame(question = col, average_length = avg_length))
}
# Function to calculate average readability for a single column
calculate_average_readability <- function(texts) {
  readability_scores <- quanteda.textstats::textstat_readability(texts, measure = "Flesch")$Flesch
  mean(readability_scores, na.rm = TRUE)
}
# Initialize an empty dataframe to store summarized results
readability_df <- data.frame(question = character(), average_readability = numeric(), stringsAsFactors = FALSE)
# Loop through each selected column and calculate average readability
for (col in colnames(users_data)) {
  column_data <- users_data[[col]]
  # Ensure the column data is a character vector
  column_data <- as.character(column_data)
  # Calculate average readability
  avg_readability <- calculate_average_readability(column_data)
  # Store the result in a dataframe
  readability_df <- rbind(readability_df, data.frame(question = col, average_readability = avg_readability))}
# Function to calculate average sentiment for a single column
calculate_average_sentiment <- function(texts) {
  sentiment_scores <- sentimentr::sentiment(texts)$sentiment
  mean(sentiment_scores, na.rm = TRUE)
}
# Initialize an empty dataframe to store summarized results
sentiment_df <- data.frame(question = character(), average_sentiment = numeric(), stringsAsFactors = FALSE)
# Loop through each selected column and calculate average sentiment
for (col in colnames(users_data)) {
  column_data <- users_data[[col]]
  # Ensure the column data is a character vector
  column_data <- as.character(column_data)
  # Calculate average sentiment
  avg_sentiment <- calculate_average_sentiment(column_data)
  # Store the result in a dataframe
  sentiment_df <- rbind(sentiment_df, data.frame(question = col, average_sentiment = avg_sentiment))
}
# Function to calculate politeness for a single column
calculate_average_politeness <- function(texts) {
  politeness_scores <- politeness::politeness(texts)
  average_politeness <- rowMeans(politeness_scores, na.rm = TRUE)
  return(mean(average_politeness, na.rm = TRUE))
}
# Initialize an empty dataframe to store summarized results
politeness_df <- data.frame(question = character(), average_politeness = numeric(), stringsAsFactors = FALSE)
# Loop through each selected column and calculate average politeness
for (col in colnames(users_data)) {
  column_data <- users_data[[col]]
  # Ensure the column data is a character vector
  column_data <- as.character(column_data)
  # Calculate average politeness
  avg_politeness <- calculate_average_politeness(column_data)
  # Store the result in a dataframe
  politeness_df <- rbind(politeness_df, data.frame(question = col, average_politeness = avg_politeness))
}
# Combine dataframes based on the 'question' column
combined_df <- politeness_df %>%
  full_join(length_df, by = "question") %>%
  full_join(sentiment_df, by = "question") %>%
  full_join(readability_df, by = "question")

# Display the first few rows of the combined dataframe
print(combined_df)

```


```{r}
#Calculate word lengths for each response
survey_data <- data_with_gpt %>%
  select(qid, GPT, sr1, sr2, sr3) %>%  # Ensure we are working only with relevant columns
  mutate(
    GPT_length = sapply(strsplit(GPT, "\\s+"), length),
    sr1_length = sapply(strsplit(sr1, "\\s+"), length),
    sr2_length = sapply(strsplit(sr2, "\\s+"), length),
    sr3_length = sapply(strsplit(sr3, "\\s+"), length),
    # Combine sr1, sr2, sr3 into a single column
    combined_sr = paste(sr1, sr2, sr3, sep = " ")
  )

#Calculate the average length for sr1, sr2, sr3
survey_data <- survey_data %>%
  mutate(
    sr_length = rowMeans(select(., sr1_length, sr2_length, sr3_length), na.rm = TRUE)
  )

#Calculate politeness scores
gpt_politeness <- politeness(survey_data$GPT, parser = "none")
sr_politeness <- politeness(survey_data$combined_sr, parser = "none")

# Extract average politeness scores
gpt_politeness_score <- rowMeans(as.matrix(gpt_politeness), na.rm = TRUE)
combined_sr_politeness_score <- rowMeans(as.matrix(sr_politeness), na.rm = TRUE)

#Create quanteda corpus for readability calculations
corpus_gpt <- quanteda::corpus(survey_data$GPT)
corpus_combined_sr <- quanteda::corpus(survey_data$combined_sr)

# Calculate readability scores
readability_gpt <- quanteda.textstats::textstat_readability(corpus_gpt, measure = "Flesch") %>% pull(Flesch)
readability_combined_sr <- quanteda.textstats::textstat_readability(corpus_combined_sr, measure = "Flesch") %>% pull(Flesch)

#Calculate sentiment scores
gpt_sentiment <- sentimentr::sentiment(survey_data$GPT) %>% 
  group_by(element_id) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE)) %>% 
  pull(sentiment)

sr_sentiment <- sentimentr::sentiment(survey_data$combined_sr) %>% 
  group_by(element_id) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE)) %>% 
  pull(sentiment)
# Step 6: Add the politeness, readability, and sentiment scores to survey_data
survey_data <- survey_data %>%
  mutate(
    GPT_politeness = gpt_politeness_score,
    sr_politeness = combined_sr_politeness_score,
    GPT_readability = readability_gpt,
    sr_readability = readability_combined_sr,
    GPT_sentiment = gpt_sentiment,
    sr_sentiment = sr_sentiment
  )

# Summarize the data
summary_data <- survey_data %>%
  select(qid, 
         GPT_length, sr_length, 
         GPT_politeness, sr_politeness, 
         GPT_readability, sr_readability, 
         GPT_sentiment, sr_sentiment)

summary_data <- summary_data %>%
  rename(question = qid)
# Perform the join
final_combined_data <- combined_df %>%
  left_join(summary_data, by = "question")

ength_data <- final_combined_data %>%
  select(question, average_length, GPT_length, sr_length) %>%
  pivot_longer(cols = c(average_length, GPT_length, sr_length),
               names_to = "Source",
               values_to = "Length") %>%
  mutate(Source = recode(Source,
                         "average_length" = "Human",
                         "GPT_length" = "GPT",
                         "sr_length" = "SR"))
# Reshape data to long format for plotting sentiments
sentiment_data <- final_combined_data %>%
  select(question, average_sentiment, GPT_sentiment, sr_sentiment) %>%
  pivot_longer(cols = c(average_sentiment, GPT_sentiment, sr_sentiment),
               names_to = "Source",
               values_to = "Sentiment") %>%
  mutate(Source = recode(Source,
                         "average_sentiment" = "Human",
                         "GPT_sentiment" = "GPT",
                         "sr_sentiment" = "SR"))
# Reshape data to long format for plotting politeness
politeness_data <- final_combined_data %>%
  select(question, average_politeness, GPT_politeness, sr_politeness) %>%
  pivot_longer(cols = c(average_politeness, GPT_politeness, sr_politeness),
               names_to = "Source",
               values_to = "Politeness") %>%
  mutate(Source = recode(Source,
                         "average_politeness" = "Human",
                         "GPT_politeness" = "GPT",
                         "sr_politeness" = "SR"))
# Reshape data to long format for plotting readability
readability_data <- final_combined_data %>%
  select(question, average_readability, GPT_readability, sr_readability) %>%
  pivot_longer(cols = c(average_readability, GPT_readability, sr_readability),
               names_to = "Source",
               values_to = "Readability") %>%
  mutate(Source = recode(Source,
                         "average_readability" = "Human",
                         "GPT_readability" = "GPT",
                         "sr_readability" = "SR"))
# Plot comparison of lengths
ggplot(length_data, aes(x = Source, y = Length, fill = Source)) +
  geom_boxplot() +
  labs(title = "Comparison of Response Lengths",
       x = "Source",
       y = "Length (words)") +
  theme_minimal()

# Plot comparison of sentiments
ggplot(sentiment_data, aes(x = Source, y = Sentiment, fill = Source)) +
  geom_boxplot() +
  labs(title = "Comparison of Response Sentiments",
       x = "Source",
       y = "Sentiment") +
  theme_minimal()
# Plot comparison of politeness
ggplot(politeness_data, aes(x = Source, y = Politeness, fill = Source)) +
  geom_boxplot() +
  labs(title = "Comparison of Response Politeness",
       x = "Source",
       y = "Politeness") +
  theme_minimal()
# Plot comparison of readability
ggplot(readability_data, aes(x = Source, y = Readability, fill = Source)) +
  geom_boxplot() +
  labs(title = "Comparison of Response Readability",
       x = "Source",
       y = "Readability") +
  theme_minimal()

```

```{r}
library(dplyr)

# Define the normalize function
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

final_combined_data <- final_combined_data %>%
  mutate(
    norm_average_politeness = normalize(average_politeness),
    norm_average_length = normalize(average_length),
    norm_average_sentiment = normalize(average_sentiment),
    norm_average_readability = normalize(average_readability),
    norm_sr_politeness = normalize(sr_politeness),
    norm_sr_length = normalize(sr_length),
    norm_sr_sentiment = normalize(sr_sentiment),
    norm_sr_readability = normalize(sr_readability),
    norm_gpt_politeness = normalize(GPT_politeness),
    norm_gpt_length = normalize(GPT_length),
    norm_gpt_sentiment = normalize(GPT_sentiment),
    norm_gpt_readability = normalize(GPT_readability)
  )
# Calculate squared errors for SR1 and GPT compared to human responses
final_combined_data <- final_combined_data %>%
  mutate(
    politeness_error_sr = (norm_average_politeness - norm_sr_politeness)^2,
    length_error_sr = (norm_average_length - norm_sr_length)^2,
    sentiment_error_sr = (norm_average_sentiment - norm_sr_sentiment)^2,
    readability_error_sr=(norm_average_readability - norm_sr_readability)^2,
    politeness_error_gpt = (norm_average_politeness - norm_gpt_politeness)^2,
    length_error_gpt = (norm_average_length - norm_gpt_length)^2,
    sentiment_error_gpt = (norm_average_sentiment - norm_gpt_sentiment)^2,
    readability_error_gpt = (norm_average_readability - norm_gpt_readability)^2
  )
# Calculate MSE for SR1 and GPT
mse_results <- final_combined_data %>%
  summarise(
    mse_politeness_sr = mean(politeness_error_sr, na.rm = TRUE),
    mse_politeness_gpt = mean(politeness_error_gpt, na.rm = TRUE),
    mse_length_sr = mean(length_error_sr, na.rm = TRUE),
    mse_length_gpt = mean(length_error_gpt, na.rm = TRUE),
    mse_sentiment_sr = mean(sentiment_error_sr, na.rm = TRUE),
    mse_sentiment_gpt = mean(sentiment_error_gpt, na.rm = TRUE),
    mse_readability_sr = mean(readability_error_sr, na.rm = TRUE),
    mse_readability_gpt = mean(readability_error_gpt, na.rm = TRUE)
  )

# Display the MSE results
print(mse_results)
```




