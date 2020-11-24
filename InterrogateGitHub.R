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
