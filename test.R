r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)

base_url <- "https://api.pushshift.io/reddit/search/"
submission_endpoint <- "submission/"


subreddit <- "worldnews"
sort <- "desc"
entries <- 3
timepoint <- 1576399066

# so that we get the top posts
sort_type <- "score"


query_top_posts <- paste0(base_url, submission_endpoint, "?", "subreddit=", subreddit, "&", "sort=", sort, "&", "size=", entries, "&", "before=", timepoint, "&", "sort_type=", sort_type)


query_top_posts
q1 <- fromJSON(query_top_posts)
q2 <- q1$data
