# Twitter Auth info
    # api_key = "______________"
    # api_secret = "______________"
    # 
    # access_token =	"______________"
    # access_token_secret =	"______________"


# Load Twitter library
    library(twitteR)


# Setup up Twitter authentication
    setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
    

# Get data from twitter
    raw_data = searchTwitter('manutd', n = 10, lang = 'en')


# Store data into a table    
    twitter_data = twListToDF(raw_data)
    write.csv(twitter_data, file = "twitter_data.csv")
    

    
# Working on Twitter trends
    world_locations = availableTrendLocations()

    chennai_twitter_id = world_locations$woeid[which(world_locations$name == 'Chennai')]
    
    chennai_trends = getTrends(chennai_twitter_id)
    
    write.csv(chennai_trends, file = 'chennai_twitter_trends.csv')
    
    View(chennai_trends)

        