################################################################################
################################################################################
#######  Group No.: 13                                                  ########
#######  Group Members: Kiran Maharana, Surajit Mondal, Gayatri Menon   ########
#######  Internship Track: Data Science Using R                         ########
#######  Incharge: Dr. Anand Khandare                                   ########
#######  Project Name: WhatsApp Chat Analysis                           ########
################################################################################
################################################################################


# Loading Libraries

library("rwhatsapp")
library("dplyr")
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
library("ggplot2")
library("plotly")
library("rlang")
library("tidyr")
library("tidyverse")
library("ggimage")
library("tidytext")


# ------------------------------------------------------------------------------
# READING THE DATASET
# ------------------------------------------------------------------------------

chat <- rwa_read("C:/Users/Kiran Maharana/Documents/SEM IV/Summer Internship/dataset.txt")

# Understanding the structure of our Dataset

# Display of Head and Tail of Dataset
head(chat)
tail(chat)


# Display the Datatypes of Columns of Dataset
sapply(chat, class) 


# Display the Number of Columns and Rows in Dataset
ncol(chat)
nrow(chat)


# ------------------------------------------------------------------------------
# Data Pre-processing
# ------------------------------------------------------------------------------

# Remove Messages without Author
chat = chat %>% filter(!is.na(author))


# Dropping column source which is insignificant for our analysis
chat = subset(chat, select = -c(source) )


# ------------------------------------------------------------------------------
# ANALYSIS OF WHATSAPP CHAT
# ------------------------------------------------------------------------------

# ------------------------ Display the messages per day ------------------------

message_day <- chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  theme_bw()+
  geom_bar(stat = "identity",fill = "#0c4c8a") +
  ylab("No. of Messages") + xlab("Days") +
  ggtitle("Messages per Day")

ggplotly(message_day)


# --------------- Display the Number of Messages of each User ------------------

ggplotly(
  chat %>%
    count(author) %>%
    ggplot(aes(x = reorder(author, n), y = n ,fill = author)) +
    geom_bar(stat = "identity") +
    ylab("Totals") + xlab("Group Members") +
    coord_flip() +
    ggtitle("Number of messages sent") +
    theme_minimal()
)


# -------------------------- Emojis Rank per User ------------------------------

plotEmojis <- chat %>%
  unnest(c(emoji, emoji_name)) %>%
  mutate( emoji = str_sub(emoji, end = 1)) %>%
  count(author, emoji, emoji_name, sort = TRUE) %>%
  # Plot top 8 Emojis per User
  group_by(author) %>%
  top_n(n = 8, n) %>%
  slice(1:8) %>% 
  
  # Create an Image URL with the Emoji UNICODE
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0('https://abs.twimg.com/emoji/v2/72x72/',as.hexmode(utf8ToInt(.x)),'.png')) )


# Plot Data
plotEmojis %>% 
  ggplot(aes(x = reorder(emoji, -n), y = n)) +
  geom_col(aes(fill = author, group=author), show.legend = FALSE, width = .20) +
  # Use to fetch an Emoji Ping Image https://abs.twimg.com
  geom_image(aes(image=emoji_url), size=.13) +
  ylab('') +
  xlab('') +
  facet_wrap(~author, ncol = 5, scales = 'free') +
  ggtitle('Most used emojis by users') +
  theme_minimal() +
  theme(axis.text.x = element_blank())


# ----------------------------- Word Count per User ----------------------------

library("stopwords")

to_remove <- c(stopwords(language = "en"),
               "media", "omitted", "ref", "dass", "schon", "mal", "android.s.wt",
               "this", "message", "deleted","ka",'bhi','ye','nono','1','2','3',
               '4','5','surajit','aditya','mondal','karan','h','nai','toh','hi',
               'hua','na','j','int', 'prashant', 'chaurasia', 'jha', 'vinay')

word_count <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  
  # Top 5 words used by the User
  group_by(author) %>%
  top_n(n = 5, n) %>%
  slice(1:20) %>%
  ungroup() %>% 
  arrange(author, desc(n)) %>% 
  mutate(order=row_number()) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = author, color = author)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  ylab('Number of Occurence') +
  xlab('Words Used') +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = 'free') +
  ggtitle('Most Words used by Users') +
  theme_minimal()

ggplotly(word_count)


# ------------------------- Most Active Day in a Week --------------------------

chat <- chat %>% mutate(Dow =  wday(as.Date(chat$time), label=TRUE))
dow <- chat %>% filter(Dow !='') %>% group_by(Dow) %>% summarise(count = n())

active_day <- ggplot(dow,aes(x=Dow,y = count, fill = Dow))+
  geom_bar(stat = "identity")+
  xlab("Days of the week")+
  ylab("Messages")+
  coord_flip()+
  geom_text(aes(label = scales::comma(count)), hjust = 3) +
  ggtitle("Days most active")+
  theme_minimal()

ggplotly(active_day)


# ----------------------- Most Active Member Each Month ------------------------

chat <- chat %>% mutate(months = month(as.POSIXct(chat$time,'%m'),label = TRUE))
mnths <- chat %>% filter(months !='') %>% group_by(months) %>% summarise(mcount = n())
actMember <- chat %>% filter(months != '')%>% group_by(months,author) %>% summarise(scount = n())%>% slice(which.max(scount))
mnthsactMember <-  merge(mnths, actMember,by="months")

ggplot(mnthsactMember)+
  geom_bar(aes(x=months,y = mcount, fill = months),stat = "identity",width = 1)+
  geom_point(aes(x=months,y = scount,color = author),
             size = 4, alpha = 0.5,
             stat = "identity",
  )+
  # geom_text(aes(x=months,y = scount,label = Name), vjust = 0.5,hjust = -1,color ="white")+
  geom_label(aes(x=months,y = scount,label = paste0(author," (",scount,")")),
             fill = 'black', vjust = 0.5,hjust = -0.4,color ="white",alpha = 0.5,size = 3.5
  )+
  xlab("Months")+
  ylab("Messages")+
  coord_flip()+
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most Active Member Each Month")+
  theme_minimal(base_size = 10)


# -------------------------- Most Media Sent by User ---------------------------

media_count = chat %>% 
  group_by(author) %>% 
  filter(text=="<Media omitted>") %>% 
  summarise(count_of_media=n()) %>% 
  arrange(desc(count_of_media))

media_count_1=media_count[1:7,]

p <- media_count_1 %>%
  ggplot(aes(x = reorder(author,count_of_media), y = count_of_media,fill=author)) +
  theme_bw()+
  geom_bar(stat = "identity") +
  ylab("Media count") + xlab("Users") +
  ggtitle("Most media sent")

ggplotly(p)


# ----------------- Member who Deleted most number of Messages -----------------

deleted_messages_count = chat %>% 
  group_by(author) %>% 
  filter(text=="This message was deleted") %>% 
  summarise(count_of_deleted_message=n()) %>% 
  arrange(desc(count_of_deleted_message))

deleted_messages_count_1 = deleted_messages_count[1:7,]

p <- deleted_messages_count_1 %>%
  ggplot(aes(x = reorder(author,count_of_deleted_message), y = count_of_deleted_message,fill=author)) +
  theme_bw()+
  geom_bar(stat = "identity") +
  ylab("count of messages deleted") + xlab("Users") +
  ggtitle("Most number of Deleted Messages by the user")

ggplotly(p)


# ------------------------------------------------------------------------------
# SENTIMENTAL ALANYSIS OF CHATS USING EMOJIS USED
# BY USING NRC WORD-EMOTION LEXICON (EmoLex) 
# ------------------------------------------------------------------------------

# Library for Emoji PNG Image Fetch from https://abs.twimg.com
# Emoji Ranking
plotEmojis <- chat %>%
  unnest(emoji, emoji_name) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  mutate( emoji_name = str_remove(emoji_name, ':.*')) %>% 
  count(emoji, emoji_name) %>% 
  # Plot top 15 Emoji
  top_n(10, n) %>% 
  # Create an image URL with Emoji UNICODE
  arrange(desc(n)) %>%
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0( 'https://abs.twimg.com/emoji/v2/72x72/', as.hexmode(utf8ToInt(.x)),'.png')) 
  )

# --------------- Plot of the Ranking of the most used Emojis ------------------

plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  geom_image(aes(image=emoji_url), size=.045) +
  scale_fill_gradient(low='#2b83ba',high='#d7191c') +
  scale_color_gradient(low='#2b83ba',high='#d7191c') +
  ylab('Emoji count') +
  xlab('Names of emojis') +
  ggtitle('Most used Emojis') +
  coord_flip() +
  theme_minimal() +
  theme()


library("kableExtra")


# Extract Emojis
emoji_chat <- chat %>% 
  unnest(c(emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  mutate( emoji_name = str_remove(emoji_name, ":.*"))


# Tokenize Emoji Names
emoji_chat <- emoji_chat %>% 
  select(author, emoji_name) %>% 
  unnest_tokens(input=emoji_name, output=emoji_words)


# Get another Lexicon with name of feelings
lexico_sentiment <- get_sentiments("nrc")
emoji_emotion <- chat %>%
  select( emoji, emoji_name) %>% 
  unnest( c(emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>%  
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%  
  unnest_tokens(input=emoji_name, output=emoji_words) %>% 
  
  # Remove classification pf NEGATIVe/POSITIVe
  inner_join(lexico_sentiment, by=c("emoji_words"="word")) %>% 
  filter(!sentiment %in% c("negative","positive")) %>% 
  
  # Keep only the 4 most frequent Emoji for each Feeling
  count(emoji, emoji_words, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(4,n) %>% 
  slice(1:4) %>% 
  ungroup() %>% 
  select(-n)


# Putting tables Together
bind_cols(
  slice(emoji_emotion, 01:16),
  slice(emoji_emotion, 17:32)) %>% 
  kable() %>% 
  kable_styling(full_width = F, font_size = 11)


# Join with Emoji
sentiment_chat <- emoji_chat %>% 
  inner_join(lexico_sentiment, by=c("emoji_words"="word")) %>%
  # Remove POSITIVE / NEGATIVE classification 
  filter(!sentiment %in% c("negative","positive"))


# -------------------- Plot of most expressed Emotion --------------------------

expressed_emo <- sentiment_chat %>% 
  count(sentiment) %>% 
  ggplot(aes(x=reorder(sentiment,n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .1) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  coord_flip() +
  ylab("Number of Times Expressed") + xlab("Emotion") +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ggtitle("Most frequently expressed emotion", "Expressed by use of emojis") +
  theme_minimal()

ggplotly(expressed_emo)


#------------------ Most Frequently expressed Emotion by Users -----------------

frequent_emo <- sentiment_chat %>% 
  count(author, sentiment) %>% 
  left_join(filter(lexico_sentiment, sentiment %in% c("negative","positive")),by=c("sentiment"="word")) %>% 
  rename( feeling = sentiment.y) %>% 
  mutate( feeling = ifelse(is.na(feeling), "neutral", feeling)) %>% 
  mutate( feeling = factor(feeling, levels = c("negative", "neutral", "positive"), ordered=T) ) %>% 
  group_by(author) %>%
  top_n(n = 8, n) %>%
  slice(1:8) %>% 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = feeling)) +
  geom_col() +
  scale_fill_manual(values = c("#d7191c","#fdae61", "#1a9641")) +
  ylab("Number of Times Expressed") +
  xlab("Emotion") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free_x") +
  ggtitle("Most frequently expressed emotion ","Expressed by use of emojis") + 
  theme_minimal() + theme(legend.position = "bottom")

ggplotly(frequent_emo)


# ------------------------------------------------------------------------------
# Targeting Advertisement using WhatsApp Chat
# ------------------------------------------------------------------------------

chat$text =tolower(chat$text)

# Unnesting Text
messages <- chat %>%
  unnest(text) %>% 
  count(author, text, sort = TRUE) %>%
  group_by(author) 

messages <- subset(messages, select=-c(n))


# Searching for Companies Name used by Users
keyword <- list("google", "amazon", "microsoft", "linkedin", "youtube", "ipl", 
                "delight", "films", "dream11", "geeksforgeeks", "coursera",
                "udemy")

# Finding Authors who have used the following Keywords
find_keyword <-
  messages$author[grep(keyword[[1]], messages$text)]


# Finding the Text Messages in which the following Keywords are used
find_message <-
  messages$text[grep(keyword[[1]], messages$text)]

ef = data.frame(find_keyword, find_message, company="google")

for (i in keyword) {
  find_keyword <-
    messages$author[grep(i, messages$text)]
  find_message <-
    messages$text[grep(i, messages$text)]
  ff <- data.frame(find_keyword, find_message,company=i)
  ef <- rbind(ef, ff)
}


# Searching for Products Name used by Users
product <- list("classroom", "pizza", "teams", "collab", "livestream", "cricket", 
                "avengers", "python", "scholarship", "javascript", "sql", "aws",
                "connections", "games")


# Finding Authors who have used the following Product
find_message <- 
  messages$text[grep(product[[1]], messages$text)]


# Finding the Text Messages in which the following Product are used
author <-
  messages$author[grep(product[[1]], messages$text)]

gf = data.frame(author, find_message, product="classroom")

for(j in product) {
  author <-
    messages$author[grep(j, messages$text)]
  find_message <-
    messages$text[grep(j, messages$text)]
  bf <- data.frame(author, find_message, product=j)
  gf <- rbind(gf, bf)
}


# Merging
companies_product <- merge(x=ef, y=gf, by="find_message", all.y = TRUE)
companies_product <- subset(companies_product, select=-c(find_message, find_keyword))
companies_product <- data.frame(author=companies_product$author, company=companies_product$company, product=companies_product$product)


# Finding the Text Messages where both Company1 and Services are Mentioned in Same
company1<-list("google", "delight", "microsoft", "google", "coursera", "coursera",
               "udemy", "ipl", "linkedin", "dream11", "geeksforgeeks", "ipl")
service <- list("classroom", "pizza", "teams", "collab", "javascript", "sql",
                "aws", "cricket", "connections", "games", "python", "teams")


ads_target <- companies_product %>%
  filter(company=="google" & product=="classroom")


j=1
for(i in company1) {
  ads1 <- companies_product %>%
    filter(company==i & product==service[j])
  ads_target <- rbind(ads_target, ads1)
  
  j=j+1
}

ads_target <- ads_target %>%
  count(author, company, product, sort=TRUE)


# Targeting Ads on the basis of Product Name used by User
comapany_product <- distinct(companies_product)


# Targeting Ads on the basis of Company and Product Name both used by User
ads_target <- distinct(ads_target)


# ---------------------------- Plotting Data -----------------------------------

ggplotly (
  ads_target %>%
    group_by(author) %>%
    top_n(n=5, n) %>%
    arrange(author, desc(n)) %>%
    mutate(order=row_number()) %>%
    ggplot(aes(x=reorder(product, n), y=n, fill=author, color=author)) +
    geom_col(show.legend=FALSE, width=.1) +
    geom_point(show.legend=FALSE, size=3) +
    ylab('Number of time mentioned by Author') +
    xlab('Company Service') +
    coord_flip() +
    facet_wrap(~author, ncol = 3, scales = 'free') +
    ggtitle('Most favourable Advertisment suggestion') +
    theme_minimal()
)


# ------------------------------------------------------------------------------
# ADDVERTISMENT CLICK PREDICTION
# ------------------------------------------------------------------------------

# Goal of the part is to predict if a particular user is likely to click on particular ad or not based on his feature.
ads <- read.csv("C:/Users/Kiran Maharana/Documents/SEM IV/Summer Internship/advertising.csv",header=T)

ad = subset(ads, select = -c(Ad.Topic.Line,City,Country,Timestamp) )


# Checking for Duplicates
duplicated(ads) 


# Attribute Type Classification
# Determining the type of attributes in the given Dataset
numeric_columns = c('Daily Time Spent on Site', 'Age', 'Area Income', 'Daily Internet Usage' )
categorical_columns = c( 'Ad Topic Line', 'City', 'Male', 'Country', 'Clicked on Ad' )


# Exploratory data analysis
# ----------- What age group does the Dataset majorly consist of? --------------
png(file = "histogram.png")


# Create the histogram.
hist(ads$Age,xlab = "Age",col = "yellow",border = "blue")


# Save the file.
dev.off()

# Here, we can see that most of the internet users are having age in the range of 26 to 42 years

sprintf('Age of the oldest person is: %d years',max(ads$Age))
sprintf('Age of the youngest person is: %d years', min(ads$Age))
sprintf('Average age in dataset is: %f years',mean(ads$Age))


# --------- What is the income distribution in different age groups? -----------
ggplot(ads, aes(x = Age, y = Area.Income)) +
  geom_point(color="green")+
  ggtitle("Age vs Income") +
  theme_minimal()

# Here, we can see that mostly teenagers are higher earners with age group of 20-40 earning 50k-70k


# --------- Which age group is spending maximum time on the internet? ----------
ggplot(ads, aes(x = Age, y = Daily.Internet.Usage)) +
  geom_point(color="blue")+
  ggtitle("Daily Internet Usage") +
  theme_minimal()

# From the above plot its evident that the age group of 25-40 is most active on the internet


# ---------------- Which gender has clicked more on online ads? ----------------
ads1 <- ads %>% group_by(Male) %>% filter(Clicked.on.Ad==1) %>% summarise(clicks_count=n()) %>% arrange(desc(clicks_count)) 

# Based on above data we can see that a greater number of females have clicked on ads compared to male.


# Maximum number of internet users belong to which country in the given dataset?
ads2 <- ads %>% group_by(Country) %>% summarise(number_of_users=n()) %>% arrange(desc(number_of_users))

# Based on the above data frame we can observe that maximum number of users are from France and Czech


# ------------------- Did we match our baseline that we set? -------------------
ads3 <- ads %>% group_by(Clicked.on.Ad) %>% summarise(Clicked_on_Ad=mean(Clicked.on.Ad),Daily_Time_Spent_on_Site=mean(Daily.Time.Spent.on.Site),Age=mean(Age),Area_Income=mean(Area.Income),Daily_Internet_Usage=mean(Daily.Internet.Usage))


# ------------ What is the relationship between different features? ------------
library(psych)
pairs.panels(ads)


# Data Cleaning
library(naivebayes)
library("naniar")
vis_miss(ads,cluster=TRUE)


# Data Model Implementation
str(ad)

ad$Daily.Time.Spent.on.Site <- as.factor(ad$Daily.Time.Spent.on.Site)
ad$Age <- as.factor(ad$Age)
ad$Area.Income <- as.factor(ad$Area.Income)
ad$Daily.Internet.Usage <- as.factor(ad$Daily.Internet.Usage)
ad$Male <- as.factor(ad$Male)
ad$Clicked_Ad <- as.factor(ad$Clicked.on.Ad)
ad = subset(ad, select = -c(Clicked.on.Ad) )

set.seed(1234)
ind <- sample(2, nrow(ad), replace = T, prob = c(0.8, 0.2))
train <- ad[ind == 1,]
test <- ad[ind == 2,]


# ------------------------ Naive Bayes Model -----------------------------------
# ------------------------------------------------------------------------------

model <- naive_bayes(Clicked_Ad ~ ., data = train)
model

plot(model)


# Predict of Train Data
p <- predict(model, train, type = 'prob')
v<- cbind(p, train)


# Predict of Test Data
m<- predict(model, test, type = 'prob')
n<- cbind(m,test)


# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$Clicked_Ad))


# Misclassification
print("Mislassification of Train Data: ")
1 - sum(diag(tab1)) / sum(tab1)  


# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$Clicked_Ad))


# Misclassification
print("Mislassification of Test Data: ")
1 - sum(diag(tab2)) / sum(tab2)


# Model Evaluation
library(caret)

cm <- table(test$Clicked_Ad, p2)
cm

confusionMatrix(cm)


# -------------------------------- Decision Tree -------------------------------
# ------------------------------------------------------------------------------

library(party)

#0000000000000000000000000000000000000000
model1<- ctree(Clicked_Ad ~ ., data=train)
plot(model1)


# Predict of train data
pre <- predict(model1, train, type = 'prob')
vre <- cbind(p, train)


# Predict of test data
mi <- predict(model1, test, type = 'prob')
ni <- cbind(m,test)


# Confusion Matrix - train data
pi1 <- predict(model1, train)
(tab1 <- table(pi1, train$Clicked_Ad))


# Misclassification
print("Mislassification of Train Data: ")
1 - sum(diag(tab1)) / sum(tab1) 

# Confusion Matrix - test data
pi2 <- predict(model1, test)
(tab2 <- table(pi2, test$Clicked_Ad))


# Misclassification
print("Mislassification of Test Data: ")
1 - sum(diag(tab2)) / sum(tab2)


# Model Evaluation
cm <- table(test$Clicked_Ad, pi2)
cm

confusionMatrix(cm)

