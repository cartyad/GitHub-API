install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)
install.packages("plotly")
library(plotly)
install.packages("devtools")
require(devtools)
install.packages("googleVis")
require(googleVis)


# Can be github, linkedin etc depending on application

oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "Access",
                   key = "9a8a24172a1e568def5a",
                   secret = "fd35fadf067cf3046be9a84cef4de4a05b81d1d8")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
?config
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/cartyad/repos", gtoken)


# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "cartyad/datasharing", "created_at"] 



# The code above was sourced from Michael Galarnyk's blog, found at:
# https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08


myData = fromJSON("https://api.github.com/users/cartyad")
myData$followers

#Acquire the usernames of all of the people who follow my GitHub Account
followers = fromJSON("https://api.github.com/users/cartyad/followers")
followers$login 

#Extract the number of accounts which my GitHub account follows from the myData variable
myData$following

#Uses fromJSON() to assign the data regarding the accounts I follow to the variable following
following = fromJSON("https://api.github.com/users/cartyad/following")
following$login 

#This extracts the public repositories which I have attached to my GitHub account from the myData variable
myData$public_repos

#Gives of the names of my public repositories, details of the dates the repositories were created and names of repositiories
repos = fromJSON("https://api.github.com/users/cartyad/repos")
repos$name 
repos$created_at  
repos$full_name 

#Displays my bio from the myData 
myData$bio

#Extracts and displays the data regarding my LowestCommonAncestor project
LCARepos <- fromJSON("https://api.github.com/repos/cartyad/LowestCommonAncestor/commits")
LCARepos$commit$message 

#Interrogate the Github API to extract data from another account by switching the username, num followers, num following, num repositories
femurrayData = fromJSON("https://api.github.com/users/femurray")
femurrayData$followers 
femurrayData$following 
femurrayData$public_repos 
femurrayData$bio 


#Part 2 - Visualisations
'For this part of the assignment, I have elcted to visualise and analyse a separate account to my own.My account is quite new 
and lacks adequate data. So through a google search of active github users, I have selected andrew, Andrew Nesbitt'

myData = GET("https://api.github.com/users/andrew/followers?per_page=100;", gtoken)
stop_for_status(myData)
extract = content(myData)
githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))
githubDB$login

#Extract the usernames of the followers  
id = githubDB$login
user_ids = c(id)

#Create and instantiate a new vector named users which is empty and a dataframe called usersDB which is also empty.
users = c()
usersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer())


#Create and execution of a for loop which adds users to the list
for(i in 1:length(user_ids))
{
  
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, gtoken)
  followingContent = content(followingRequest)
  
  #This If statement determines if a user has no followers, it skips over to the next
  if(length(followingContent) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  
  #Execute a loop to iterate through the following users to the account
  for (j in 1:length(followingLogin))
  {
    #If statement to ensure the list does not contain duplicate values and adds those to the list 
    if (is.element(followingLogin[j], users) == FALSE)
    {
      users[length(users) + 1] = followingLogin[j]
      
      #Extract data on each user
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, gtoken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      #Retrieve data on who each of these users follow
      followingNumber = followingDF2$following
      
      #Extract infora=mation and data on the users which this given user follows
      followersNumber = followingDF2$followers
      
      #Retrieve each user's number of repositories
      reposNumber = followingDF2$public_repos
      
      #Extract the data on the year in which the user joined and created their account
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)
      
      #Add this user's data to the aforementioned dataframe usersDB
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
      
    }
    next
  }
  #If statement which ends once the number of users exceeds 100
  if(length(users) > 100)
  {
    break
  }
  next
}

#Use plotly to graph

Sys.setenv("plotly_username"="cartyad")
Sys.setenv("plotly_api_key"="LIZIJHuZ3vUpowXGoCMj")


# plot repositories v followers coloured by year
plot1 = plot_ly(data = usersDB, x = ~repos, y = ~followers, 
                text = ~paste("Followers: ", followers, "<br>Repositories: ", 
                              repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
usersDB
plot1
api_create(plot1, filename = "Repositories vs Followers")

#Google Visualisation ~ Repositories vs Followers
gVisPlot1Data<-usersDB[,c(4,3)]
gVisPlot1<- gvisScatterChart(gVisPlot1Data, options = list(hAxes="[{title:'Repositories', titleTextStyle: {color: 'blue'}}]", vAxes="[{title:'Followers', titleTextStyle: {color: 'blue'}}]"))
plot(gVisPlot1)


#plot 2 
#following vs followers categorised by year
plot2 = plot_ly(data = usersDB, x = ~following, y = ~followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~dateCreated)
plot2
api_create(plot2, filename = "Following vs Followers")

#Google Visualisation ~ Following vs Followers
gVisPlot2Data<-usersDB[,c(2,3)]
gVisPlot2<- gvisScatterChart(gVisPlot2Data, options = list(hAxes="[{title:'Following', titleTextStyle: {color: 'blue'}}]", vAxes="[{title:'Followers', titleTextStyle: {color: 'blue'}}]"))
plot(gVisPlot2)

#Now ascertaining data on 10 most popular languages of the 100 users populating the usersDB dataframe
languages = c()

for (i in 1:length(users))
{
  RepositoriesUrl = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, gtoken)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  RepositoriesNames = RepositoriesDF$name

  for (j in 1: length(RepositoriesNames))
  {
    RepositoriesUrl2 = paste("https://api.github.com/repos/", users[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, gtoken)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    language = RepositoriesDF2$language
  
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}

#Adds the previously ascertained 10 most popular languages to a table top10Languages
allLanguages = sort(table(languages), increasing=TRUE)
top10Languages = allLanguages[(length(allLanguages)-9):length(allLanguages)]
#converts the table top10Languages into a dataframe
languageDF = as.data.frame(top10Languages)
languageDF
#Plot 3 Top 10 Languages
plot3 = plot_ly(data = languageDF, x = languageDF$languages, y = languageDF$Freq, type = "bar")
plot3
api_create(plot3, filename = "10 Most Popular Languages")

#Google Visualisation ~ Top 10 Programming Languages
gVisPlot3 <- gvisBarChart(languageDF,
                          options = list(title="Top 10 Programming Languages", 
                          hAxes="[{title:'Frequency', titleTextStyle: {color: 'blue'}}]", 
                          vAxes="[{title:'Languages', titleTextStyle: {color: 'blue'}}]",
                          height=500))
plot(gVisPlot3)

###############################################################
#Plot of the most commonly used word in the comments of commits
###############################################################

#Source: https://www.red-gate.com/simple-talk/sql/bi/text-mining-and-sentiment-analysis-with-r/

# Install
install.packages("tm")
install.packages("SnowballC") 
install.packages("wordcloud")  
install.packages("RColorBrewer") 
install.packages("syuzhet") 
install.packages("ggplot2") 
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

############################################################################################################################################
data2 = GET("https://api.github.com/repos/torvalds/linux/comments?per_page=10000;", gtoken)
stop_for_status(data2)
extract2 = content(data2)
githubDB2 = jsonlite::fromJSON(jsonlite::toJSON(extract2))
TextDocOriginal1<-githubDB2$body
TextDoc1 <- Corpus(VectorSource(TextDocOriginal1))
# Convert the text to lower case
TextDoc1 <- tm_map(TextDoc1, content_transformer(tolower))
TextDoc1

# Build a term-document matrix
TextDoc1_dtm <- TermDocumentMatrix(TextDoc1)
dtm_m <- as.matrix(TextDoc1_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


# Find associations 
findAssocs(TextDoc1_dtm, terms = c("perf","pub","fix","add"), corlimit = 0.25)

# Find associations for words that occur at least 50 times
findAssocs(TextDoc1_dtm, terms = findFreqTerms(TextDoc1_dtm, lowfreq = 50), corlimit = 0.25)


'"Bing," "AFINN," and "NRC" are all simple lexicons:  each is a list of words with a precomputed positive or
 negative "score" for each word, and Syuzhet computes the valence of a sentence by simply adding together the 
 scores of every word in it.'

syuzhet_vector <- get_sentiment(TextDoc1, method="syuzhet")
syuzhet_vector[1]
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(TextDoc1, method="bing")
bing_vector[1]
head(bing_vector)
summary(bing_vector)

#affin
afinn_vector <- get_sentiment(TextDoc1, method="afinn")
afinn_vector[1]
head(afinn_vector)
summary(afinn_vector)


vector1<-c()
for(i in 1:length(TextDocOriginal1)){
  vector1<-append(vector1,TextDocOriginal1[[i]]) 
}
vector1  
d<-get_nrc_sentiment(vector1)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[1:length(td)]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
CommentSentimentPlot<-quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Comment Message Sentiments")
CommentSentimentPlot<-gvisBarChart(td_new2,options = list(title="Comment Message Sentiments", 
                                                          hAxes="[{title:'Frequency', titleTextStyle: {color: 'blue'}}]", 
                                                          vAxes="[{title:'Emotion', titleTextStyle: {color: 'blue'}}]",
                                                          height=500))
plot(CommentSentimentPlot)

#################################################################################################################################################

###############################################################
#Plot of the most commonly used word in the message of commits
###############################################################

#Source: https://www.red-gate.com/simple-talk/sql/bi/text-mining-and-sentiment-analysis-with-r/

data2 = GET("https://api.github.com/repos/torvalds/linux/commits?per_page=10000;", gtoken)
stop_for_status(data2)
extract2 = content(data2)
githubDB2 = jsonlite::fromJSON(jsonlite::toJSON(extract2))
TextDocOriginal<-githubDB2$commit$message
TextDocOriginal
TextDoc <- Corpus(VectorSource(TextDocOriginal))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
TextDoc

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d,0)


# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_dtm, terms = c("perf","pub","fix","add"), corlimit = 0.25)

# Find associations for words that occur at least 50 times
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)


'"Bing," "AFINN," and "NRC" are all simple lexicons:  each is a list of words with a precomputed positive or
 negative "score" for each word, and Syuzhet computes the valence of a sentence by simply adding together the 
 scores of every word in it.'

syuzhet_vector <- get_sentiment(TextDoc, method="syuzhet")
syuzhet_vector[1]
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(TextDoc, method="bing")
bing_vector[1]
head(bing_vector)
summary(bing_vector)

#affin
afinn_vector <- get_sentiment(TextDoc, method="afinn")
afinn_vector[1]
head(afinn_vector)
summary(afinn_vector)


vector1<-c()
for(i in 1:length(TextDocOriginal)){
  vector1<-append(vector1,TextDocOriginal[[i]]) 
}
vector1  
d<-get_nrc_sentiment(vector1)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[1:length(td)]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
CommitMessageSentimentPlotquickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Commit Message Sentiments")
CommitMessageSentimentPlot<-gvisBarChart(td_new2,options = list(title="Comment Message Sentiments", 
                                                          hAxes="[{title:'Frequency', titleTextStyle: {color: 'blue'}}]", 
                                                          vAxes="[{title:'Emotion', titleTextStyle: {color: 'blue'}}]",
                                                          height=500))
plot(CommitMessageSentimentPlot)

#######################################################################################################################################################
