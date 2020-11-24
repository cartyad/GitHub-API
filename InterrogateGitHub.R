#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)


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
