# Sirapat N. 57070503438 Thanaboon 57070503415 Sunhakoch 57070503436
# Imdb 2016 crawler, sorted by popularity pages(1-200) movies(1-20,000)
# Complexity -> Get data of each url in each page = 200x50 = 10,000 loops
# If the data is not provided, we replace empty or blank with NA
# library(rvest) before run this script  #

movies <- data.frame("id"=integer(), "title"=character(), "rating"=double(),
                    "description"=character(),"country"=character(), language=character(),
                    genres=character(), runtime=character(), stringsAsFactors = FALSE)

for(i in 1:200) { # 200 pages 1:200
  imdb_2016_each_page <- paste("http://www.imdb.com/search/title?release_date=2016&page=", i, sep="") %>% read_html()
  for (j in 1:50) { # 50 movies in each page 1:50
    movie_name_url_location <- paste("#main > div > div > div.lister-list > div:nth-child(", 
                                     j, ") > div.lister-item-content > h3 > a", sep="")
    movie_name_url <- imdb_2016_each_page %>% html_nodes(movie_name_url_location) %>% html_attr("href") 
    movie_name_url <- paste("http://www.imdb.com", movie_name_url, sep="") %>% read_html()
    id <- j + (50*(i-1))
    title <- movie_name_url %>% html_nodes("div.title_wrapper > h1") %>% html_text()
    if (identical(title, character(0))) { # some records are missing!
      title <- NA
    }
    rating <- movie_name_url %>% html_nodes("strong > span") %>% html_text() %>% as.numeric()
    if (identical(rating, numeric(0))) { #some movies don't have rating
      rating <- NA
    }
    description <- movie_name_url %>% html_nodes(".summary_text") %>% html_text()
    description <- gsub("(<=[\\s])\\s*|^\\s+|\\s+$", "", description, perl=TRUE) # remove space/tab/newline
    if (identical(description, character(0)) || grepl("Add a Plot", description, fixed=TRUE)) { 
      description <- NA # some movies don't have description
    }
    # Some diferrences of structure (country / language)
    country <- movie_name_url %>% html_nodes("#titleDetails > div:contains('Country') > a") %>% html_text() %>% paste(collapse=',')
    if (country == "") {
      country <- NA
    }
    language <- movie_name_url %>% html_nodes("#titleDetails > div:contains('Language') > a") %>% html_text() %>% paste(collapse=',')
    if (language == "") {
      language <- NA
    }
    genres_location <- paste("#main > div > div > div.lister-list > div:nth-child(", 
                             j, ") > div.lister-item-content > p:nth-child(2) > span.genre", sep="")
    genres <- imdb_2016_each_page %>% html_nodes(genres_location) %>% html_text() 
    genres <- gsub("[\t\r\n]", "", genres) # remove tab/new line
    if (identical(genres, character(0))) { # some movies don't have genres
      genres <- NA
    }
    runtime_location <- paste("#main > div > div > div.lister-list > div:nth-child(", 
                             j, ") > div.lister-item-content > p:nth-child(2) > span.runtime", sep="")
    runtime <- imdb_2016_each_page %>% html_nodes(runtime_location) %>% html_text()
    if (identical(runtime, character(0))) { # some movies don't have runtime
      runtime <- NA
    }
    # add new row to dataframe
    movies <- rbind(movies, data.frame("id"=id, "title"=title, "rating"=rating, 
              "description"=description, "country"=country, "language"=language,
              "genres"=genres, "runtime"=runtime, stringsAsFactors = FALSE))

    cat(sprintf("Crawling... page:%d | movie_id:%d [DONE]\n",i,id))
  }
}
write.csv(movies,file= "../movies_result.csv") # output file movies_result.csv 