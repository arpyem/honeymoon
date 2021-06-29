library(tidyverse)
library(googleway)

## API key setup
# set_key(key = Sys.getenv("google"))
# google_keys()

stops = read_csv(file = 'stops.csv')

str(stops)
print.data.frame(stops)

stops$address[1] # "3600 Presidential Blvd, Austin, TX 78719"
test = google_geocode(address = stops$address[1])
test

google_geocode(address = "Walmart, Austin")
google_geocode(address = "Walmart Supercenter, Austin, TX 78719")
google_geocode(address = "Santa Elena Canyon Trail")

# place name works, exact address does not - due to special character?
google_geocode(address = "QI Austin: Modern Asian Kitchen")
google_geocode(address = "835 W 6th St #114, Austin, TX 78703")
google_geocode(address = "835 W 6th St 114, Austin, TX 78703")



test2 = stops %>%
      pmap(function(place_name, address, ...) {
            query = if (is.na(address)) place_name else address
            request = google_geocode(address = query)
            if (request$status != "OK") {
                  request = google_geocode(address = place_name)
            }
            request
      })

geo = test2 %>%
      imap(function(x, i) {
            data.frame(
                  head(x$results$geometry$location, 1),
                  place_name = stops$place_name[i],
                  segement_type = stops$segment_type[i],
                  annotation = stops$annotation[i],
                  id = i
            )
      }) %>%
      bind_rows()

google_map(data = geo) %>%
      add_markers(lat = "lat", lon = "lng")




failures = test2 %>% 
      imap(function(x, i) {
            if (x$status != "OK") i
      }) %>%
      compact() %>%
      unlist()


test2[[1]]$results$formatted_address
test2[[1]]$results$geometry$location

directions_test = google_directions(
      origin = test2[[1]]$results$formatted_address,
      destination = test2[[2]]$results$formatted_address,
      mode = "driving"
)

directions_test$routes$overview_polyline$points %>%
      decode_pl()


google_map() %>%
      add_polylines(data = data.frame(pl = directions_test$routes$overview_polyline$points), polyline = "pl")

