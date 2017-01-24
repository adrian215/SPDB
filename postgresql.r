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

loid <- as.integer(1)

checkBusStop(1)

checkBusStop <- function (loid) {
  
  calcVelocity <- function(df) {
    df['velocity'] <- 1000
    
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
      distm(c(x['latitude'], x['longitude']), c(busStop$latitude, busStop$longitude))
    })
    stopCustered
  }
  
  
  print('Wyszukiwanie lokalizacji przystanku: ')
  print(loid)
  select <- "select * from csipstoppoint p where loid = 1"
  
  busStop <- dbGetQuery(con, select)
  print('Pobieranie listy historii lokalizacji pojazdow w poblizu przystanku...')
  res <- dbGetQuery(
    con,
    'select * from csiptrace t where
    earth_distance (ll_to_earth(53.152122, 23.125016), ll_to_earth(t.latitude, t.longitude)) < 50 limit 1000'
  )
  
  data <- res[order(res['regtime']),]
  
  allV <- data.frame()
  for (vehicleid in unique(data$vehicleid)) {
    print('Wyliczanie predkosci dla pojazdu')
    print(vehicleid)
    selected <- data[data$vehicleid == vehicleid, ]
    velocities <- calcVelocity(selected)
    allV <- rbind(allV, velocities)
  }
  
  print('Wyznaczenie punktow postoju pojazdow w poblizu przystanku...')
  stop <- allV[allV$velocity < 1, ]
  print('Optymalizacja punktow postoju...')
  clustered <- cluster(stop)
  
  newLocation <- clustered[clustered$dist == min(clustered$dist), ]
  print('Nowa lokalizacja:')
  print(newLocation)
  print('Poprzednia lokalizacja:')
  print(busStop[c('latitude', 'longitude')])
}