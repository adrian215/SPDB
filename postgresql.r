# install.packages("RPostgreSQL")
# install.packages("geosphere")
# install.packages("fpc")
require("RPostgreSQL")
library("geosphere")
library("fpc")

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "spdb",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "postgres")

# check for the cartable
dbExistsTable(con, "csiptrace")
res <- dbGetQuery(
  con,
  'select * from csiptrace t where earth_distance (ll_to_earth(53.152122, 23.125016), ll_to_earth(t.latitude, t.longitude)) < 50'
)

selected <- res[res$vehicleid == '218', ]
sorted <- selected[order(selected['regtime']),]

sorted <- calcVelocity(sorted)

stop <- sorted[sorted$velocity < 2, ]
stop[c('latitude', 'longitude')]
mean(stop$latitude)
mean(stop$longitude)

clustered <- cluster(stop)
newLocation <- clustered[clustered$dist == min(clustered$dist), ]



calcVelocity <- function(df) {
  df['velocity'] <- 0
  for (i in 2:nrow(df)) {
    row <- df[i,]
    before <- df[i - 1,]
    dt <- as.numeric(row[['regtime']]) - as.numeric(before[['regtime']])
    s <- distm( c(row[['latitude']], row[['longitude']]), c(before[['latitude']], before[['longitude']]))
    df[i,]$velocity <- s / dt
  }
  df
}

cluster <- function (stop) {
  clustered <- pamk(stop[c('latitude', 'longitude')])
  stopCustered <- as.data.frame(clustered$pamobject$medoids)
  stopCustered$dist <- apply(stopCustered[c(1:2)], 1, function (x, y) {
    distm(c(x['latitude'], x['longitude']), c(53.152122, 23.125016))
  })
  stopCustered
}