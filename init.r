install.packages("twitteR")
install.packages("ROAuth")
install.packages("ggplot2")

heroku create --stack cedar-14 --buildpack http://github.com/virtualstaticvoid/heroku-buildpack-r.git#cedar-14
