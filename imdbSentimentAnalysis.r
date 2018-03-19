library(rvest)

movies <- data.frame("title"=character(), "rating"=double(), "review_rating"=double(), "review"=character(), stringsAsFactors = FALSE)

y2017p1 <- read_html("http://www.imdb.com/search/title?release_date=2017&page=1")

for (i in 8:8) { # 1 movies in this page 6
  movie_name_url_location <- paste("#main > div > div > div.lister-list > div:nth-child(", i, ") > div.lister-item-content > h3 > a", sep="")
  movie_name_url <- paste("http://www.imdb.com", y2017p1 %>% html_nodes(movie_name_url_location) %>% html_attr("href"), sep="")
  movie_page <- movie_name_url %>% read_html()
  
  cat(sprintf("Crawling... page 1 movie No.%d\n",i))
  
  title <- movie_page %>% html_nodes("div.title_wrapper > h1") %>% html_text()
  title <- trimws(title)
  if (identical(title, character(0))) { # some records are missing!
    title <- NA
  }
  
  rating <- movie_page %>% html_nodes("strong > span") %>% html_text() %>% as.numeric()
  if (identical(rating, numeric(0))) { #some movies don't have rating
    rating <- NA
  }
  review_url <- paste("http://www.imdb.com", movie_page %>% html_node("#titleUserReviewsTeaser > div > div.see-more > a:nth-child(3)") %>% html_attr("href"), sep="")
  review_page <- review_url %>% read_html()
  
  total_review <- review_page %>% html_nodes("td:contains('reviews in total')") %>% html_text()
  total_review <- strsplit(total_review,"[^[:digit:]]") %>%  unlist() %>% as.numeric()
  total_review <- total_review[!is.na(total_review)]
  total_page <- floor(total_review / 10)
  
  for(j in 1:total_page) { # every review pages of a movie
    cat(sprintf("Looking at reviews page %d of movie No.%d\n",j,i))
    index <- (j*10) - 10
    current_review_page_url <- gsub("(.*reviews).*",paste("\\1?start=",index,sep=""),review_url)
    current_review_page <- read_html(current_review_page_url)
    for (k in 1:10) {
      cat(sprintf("Reading reviews %d of page %d of movie No.%d\n",k,j,i))
      review_location <- paste("#tn15content > p:nth-child(", 6+(4*k),")",sep="")
      review_rating_location <- paste("#tn15content > div:nth-child(", 5+(4*k), ") > img",sep="")
      review_rating <- current_review_page %>% html_nodes(review_rating_location) %>% html_attr("alt")
      review_rating <- as.numeric( gsub("(.)/10","\\1",review_rating) )
      if (identical(review_rating, numeric(0))) { #some reviews don't have review_rating
        review_rating <- NA
      }
      review <- current_review_page %>% html_nodes(review_location) %>% html_text() 
      if (identical(review, character(0))) { #some reviews don't have review_rating
        review <- NA
      }
      # add new row to dataframe
      movies <- rbind(movies, data.frame("title"=title, "rating"=rating, "review_rating"=review_rating, "review"=review, stringsAsFactors = FALSE))
    }
  }
  write.csv( movies,file= paste("~/Desktop/",title,".csv",sep="") )
  movies <- data.frame("title"=character(), "rating"=double(), "review_rating"=double(), "review"=character(), stringsAsFactors = FALSE)
}

