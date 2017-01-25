# install.packages("RPostgreSQL")
# install.packages("geosphere")
# install.packages("fpc")
# install.packages("GetoptLong")
require("RPostgreSQL")
library("geosphere")
library("fpc")
library("GetoptLong")

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "spdb",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "postgres")

# check for the cartable
dbExistsTable(con, "csiptrace")
result <- data.frame()
for (i in 1:1) {
  qqcat("Wykonywanie aktualizacji dla przystanku loid=@{i}")
  stop <- checkBusStop(i)
  result <- rbind(result, stop)
}

write.csv(result, "spdb-result.csv")

checkBusStop <- function (loid) {
  
  calcVelocity <- function(df) {
    df['velocity'] <- 1000
    
    if (nrow(df) < 2) {
      return(df)
    }
    
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
  
  
  qqcat('Wyszukiwanie lokalizacji przystanku: @{loid}\n')
  select <- qq("select * from csipstoppoint p where loid = @{loid}")
  busStop <- dbGetQuery(con, select)
  
  qqcat('Pobieranie listy historii lokalizacji pojazdow w poblizu przystanku...\n')
  select <- qq('select * from csiptrace t where
    earth_distance (ll_to_earth(@{busStop$latitude}, @{busStop$longitude}), ll_to_earth(t.latitude, t.longitude)) < 50 limit 1000')
  res <- dbGetQuery(con, select)
  
  data <- res[order(res['regtime']),]
  
  allV <- data.frame()
  for (vehicleid in unique(data$vehicleid)) {
    qqcat('Wyliczanie predkosci dla pojazdu @{vehicleid} \n')
    selected <- data[data$vehicleid == vehicleid, ]
    velocities <- calcVelocity(selected)
    allV <- rbind(allV, velocities)
  }
  
  qqcat('Wyznaczenie punktow postoju pojazdow w poblizu przystanku...\n')
  stop <- allV[allV$velocity < 1, ]
  qqcat('Optymalizacja punktow postoju...\n')
  busStop$dist <- 0
  tryCatch({
    clustered <- cluster(stop)
    newLocation <- clustered[clustered$dist == min(clustered$dist), ]
    print('Nowa lokalizacja:')
    print(newLocation)
    print('Poprzednia lokalizacja:')
    print(busStop[c('latitude', 'longitude')])
    busStop$latitude <- newLocation$latitude
    busStop$longitude <- newLocation$longitude
    busStop$dist <- newLocation$dist
  }, error = function (e) {
    qqcat("Nie można wyznaczyć kilku grup punktów")
  })
  busStop
}