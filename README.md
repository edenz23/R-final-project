# \# SISE2601 Project README

Team 8 - Ella, Keren, Tomer and Eden

### In order to replicate our analysis, you should use "Human_Response_Variance_Testing.rmd" file which will handle everything from loading, cleaning, processing, enriching and plotting

Since some data was added outside the scope of the rmd file processing (for example GPT responses), you should place the provided csv/tsv files in the root directory of the project, inside a folder named 'data'

### libraries used:

-   tidyverse

-   qdap

-   sentimentr

## Modifications on datasets from original form:

#### survey_srs.csv - email templates -\> survey_srs_with_GPT_responses.csv

-   add ChatGPT response for each email - for comparison and analysis

-   add 'Weighted sentiment', 'Number of words' and 'Flesch reading ease score' measurements for both the GPT responses, and the SR responses

#### survey_responses_anon.tsv - user responses

-   add 'Weighted sentiment', 'Number of words' and 'Flesch reading ease score' measurements for each response

-   filter invalid responses, that cannot run through processing

-   calculate response rate ("999" appearnses) for each email by each user's response to that email

Additional extracted data: users response rates for each email, calculated with regular expressions - matching on the users response text, checking whether the user included the string "999" in his response,

the string "999" represents a user marking his response to that email 'if I had received this email in real life, I would not have responded to it',

and that;s because the users were required to give an answer to all emails as part of the survey they were taking part in.

### Data overview:

### DB name: survey_srs_with_GPT_responses

Result of glimpse() function on the dataset:

```         
Rows: 20
Columns: 13
$ qid          <chr> "e_frnd_1", "e_frnd_2", "e_frnd_3", "e_frnd_5", "e_frnd_4", "e_fmly…
$ sender_type  <chr> "friend", "friend", "friend", "friend", "friend", "family", "family…
$ sr1          <chr> "Yum!", "Will do!", "Yes, I'm around.", "I think that's a good idea…
$ sr2          <chr> "Sounds yummy!", "Sounds good!", "Yes!", "Let me think about it.", …
$ sr3          <chr> "I'm down.", "Sounds like a plan!", "No plans yet.", "I'm down for …
$ GPT          <chr> "Hi [Friend's Name],\n\nThat sounds great! How about Saturday at no…
$ sender_email <chr> "natpoor@gmail.com", "Peter.Montero@fmr.com", "grinberg.nir@gmail.c…
$ thread_id    <dbl> 1.605637e+18, 1.607446e+18, 1.608993e+18, 1.608993e+18, 1.608993e+1…
$ msg_id       <dbl> 1.605637e+18, 1.607447e+18, 1.608993e+18, 1.608993e+18, 1.608993e+1…
$ msg_pos      <int> 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 14, 1, 3, 1, 4, 1
$ account      <chr> "Nir Grinberg   (grinberg.nir@gmail.com)", "Nathaniel Poor   (natpo…
$ subject      <chr> "Lunch?", "<friends_sal>! It's Jack", "New place here we come!", "C…
$ txt          <chr> "<friends_sal>,\n\nLet's get lunch this weekend.", "Yeah;  personal…
```

This dataset contains the 20 email templates given to the participants of the survey, where we have 5 emails from each category - (marked by the 'qid' and 'sender_type' fields) "friend", "family", "work" and "other".

The fields "sr1", "sr2", "sr3" represent the SmartResponse options given to the participants for each particular email.

The field "GPT" represent the response ChatGPT generated for each email, while given to context of that email (if its an email from a friend or co-worker etc..)

It also has the emails details such as subject and txt (content of the email for the participant to respond to).

### DB name: survey_responses_anon

Results of glimpse() function on the dataset:

```         
Rows: 240
Columns: 66
$ status                       <chr> "APPROVED", "APPROVED", "APPROVED", "APPROVED", "AP…
$ started_datetime             <chr> "2018-08-21 22:04:51.667000", "2018-08-22 07:52:04.…
$ completed_date_time          <chr> "2018-08-21 22:33:51.862000", "2018-08-22 08:09:55.…
$ time_taken                   <dbl> 1740.195, 1071.129, 608.380, 1147.466, 691.846, 134…
$ age_prolific                 <int> 27, 27, 23, 48, 27, 36, 30, 20, 18, 33, 60, 61, 26,…
$ num_approvals                <int> 42, 31, 256, 11, 531, 282, 481, 44, 29, 454, 179, 4…
$ num_rejections               <int> 0, 2, 1, 0, 2, 3, 0, 3, 0, 3, 2, 0, 1, 0, 0, 0, 1, …
$ prolific_score               <int> 100, 99, 100, 100, 100, 100, 100, 98, 100, 100, 100…
$ reviewed_at_datetime         <chr> "2018-08-22 17:23:57.634000", "2018-08-22 17:22:47.…
$ entered_code                 <chr> "4WXRED1U", "4WXRED1U", "4WXRED1U", "4WXRED1U", "4W…
$ Country.of.Birth             <chr> "Philippines", "India", "United States", "United St…
$ Student.Status               <chr> "Yes", "No", "Yes", "No", "No", "No", "No", "No", "…
$ Employment.Status            <chr> "Unemployed (and job seeking)", "Part-Time", "Part-…
$ Current.Country.of.Residence <chr> "United Kingdom", "Netherlands", "United States", "…
$ First.Language               <chr> "English", "Hindi", "English", "Telugu", "English",…
$ Sex                          <chr> "Female", "Female", "Male", "Female", "Female", "Ma…
$ Nationality                  <chr> "Philippines", "India", "United States", "India", "…
$ Ethnicity2                   <chr> "", "", "White/Caucasian", "", "Black/African Ameri…
$ Fluent.languages             <chr> "", "", "", "", "", "", "", "", "", "", "", "", "",…
$ start_datetime               <chr> "2018-08-21T22:04:51.667Z", "2018-08-22T07:52:04.09…
$ start_diff                   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
$ sampling_frame               <chr> "philippines", "india", "us_white", "india", "us_bl…
$ StartDate                    <chr> "2018-08-21 16:04:54", "2018-08-22 01:52:07", "2018…
$ EndDate                      <chr> "2018-08-21 16:33:49", "2018-08-22 02:09:52", "2018…
$ Status                       <chr> "IP Address", "IP Address", "IP Address", "IP Addre…
$ Progress                     <int> 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 1…
$ Duration..in.seconds.        <int> 1735, 1065, 587, 1136, 683, 1341, 675, 570, 1185, 7…
$ Finished                     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
$ RecordedDate                 <chr> "2018-08-21 16:33:50", "2018-08-22 02:09:53", "2018…
$ ResponseId                   <chr> "R_2CjqczHCyKZsoGb", "R_DJnZBaJJU8GL2Ux", "R_YbOrNF…
$ ExternalReference            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ LocationLatitude             <dbl> 53.55000, 53.24750, 43.76849, 42.20110, 30.87170, 3…
$ LocationLongitude            <dbl> -2.30000305, 6.58900452, -88.01609802, -85.61689758…
$ DistributionChannel          <chr> "anonymous", "anonymous", "anonymous", "anonymous",…
$ UserLanguage                 <chr> "EN", "EN", "EN", "EN", "EN", "EN", "EN", "EN", "EN…
$ gender                       <chr> "Female", "Female", "Male", "Female", "Other / pref…
$ age                          <int> 28, 28, 24, 49, 28, 37, 31, 28, 19, 34, 61, 62, 27,…
$ nationality                  <chr> "Philippines", "India", "United States", "United St…
$ race                         <chr> "", "", "Caucasian/White (non-Hispanic)", "Asian/Pa…
$ edu                          <chr> "Postgraduate degree (MA, MBA, MD, JD, PhD, etc.)",…
$ ses_us                       <chr> "", "", "$150000 or more", "$100,000–$149,999", "$1…
$ ses_world                    <int> 6, 7, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
$ pol_leaning                  <chr> "", "", "Liberal", "Liberal", "Liberal", "Conservat…
$ religion                     <chr> "Somewhat important", "Somewhat important", "Not at…
$ e_frnd_1                     <chr> "Hello! Sure - do you have a special lunch place in…
$ e_frnd_2                     <chr> "Hi, Jack! Good to see you earlier. I will save thi…
$ e_frnd_3                     <chr> "Hi,  Sure! I have free time this weekend - should …
$ e_frnd_4                     <chr> "Hey Mark, these photos look wonderful! Thanks for …
$ e_frnd_5                     <chr> "Are you free on the 15th? And would you like to? I…
$ e_fmly_1                     <chr> "Okay, I'll catch you later, then - let me know whe…
$ e_fmly_2                     <chr> "Hi gramma, Here's a photo of little Jen. She says …
$ e_fmly_3                     <chr> "Cool. I'll check it later! Congrats!! 999", "Wow! …
$ e_fmly_4                     <chr> "Yes, I'm in! Do you need me to bring anything? Cak…
$ e_fmly_5                     <chr> "Interesting! Thanks for this! 999", "Thank you for…
$ e_work_1                     <chr> "Glad I could be of help! Cheers! 999", "You are mo…
$ e_work_2                     <chr> "Hi Linda,  I think I'm free tomorrow and can join …
$ e_work_3                     <chr> "Hey Michael,   Thanks for the offer! I'll give you…
$ e_work_4                     <chr> "Hi, For info, I have now registered for the event.…
$ e_work_5                     <chr> "Hi, Thank you for the information. Regards, 999", …
$ e_other_1                    <chr> "Thanks for the info. Regards, A 999", "Thank you f…
$ e_other_2                    <chr> "Hi Kiran, Thanks for the info but I think I have a…
$ e_other_3                    <chr> "Hi landlord,  Yes, that's fine. Please update me a…
$ e_other_4                    <chr> "Thank you! 999", "Thank you for your understanding…
$ e_other_5                    <chr> "Hi, Thanks for the info! 999", "Thank you for info…
$ comments                     <chr> "", "", "none", "none", "", "", "", "Nope", "", "",…
$ yob                          <int> 1990, 1990, 1994, 1969, 1990, 1981, 1987, 1990, 199…
```

This dataset contains the 240 participants responses, each entry has a result for all 20 emails, each represented by its 'qid'.

Alongside the responses there is additional data on the participants, such as "age", "education" (edu), "religious importance" (religion), "gender", "country of birth" (Country.of.Birth) etc..

In addition to data on the participants, there are general data fields, such as - "start time" (started_datetime), "time taken" (time_taken), whether the participant finish the survey (Finished) etc..

\*\* for additional info, can refer to the 'glimpse()' results, where there are examples for each field.
