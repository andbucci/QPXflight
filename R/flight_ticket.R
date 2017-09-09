flight_ticket <- function(Apikey, origin, destination, roundtrip, dateorigin, datedeparture = NULL, flexible = FALSE, class = "Economy", earliest1 = "6:00", latest1 = "15:00", earliest2 = "12:00", latest2 = "18:00", connec1 = 90, connec2 = 90, maxprice = "EUR2000"){
  require(RCurl)      # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
  require(jsonlite)
  require(tidyjson)   # https://cran.r-project.org/web/packages/tidyjson/vignettes/introduction-to-tidyjson.html
  require(dplyr)      # for %>% and other dplyr functions
  require(tidyr)
  require(plyr)       # for 'join' sql db
  require(readr)      # parse_number to parse numbers from strings
  require(stringr)
  require(stargazer)
  require(xlsx)
  require(geosphere)
  require(ggmap)
  url <- paste('https://www.googleapis.com/qpxExpress/v1/trips/search?key=', Apikey,  '&alt=json', sep ='')
  n_orig <- length(origin)
  n_dest <- length(destination)
  if (n_orig != n_dest){
    warning('The list of departure airports should be the same of arrival airports')
  }
  airp_mat <- cbind(origin, destination)
  
  if (roundtrip == 0){
    x <- NULL
    for (i in 1:(length(airp_mat[,1]))) 
    {
      #if (airp_mat[i,1] != airp_mat[i,2])
      x[i] <- list(list(
        request = list(
          slice = list(
            #Segment A
            list(origin = airp_mat[i,1], destination = airp_mat[i,2], date = dateorigin, preferredCabin = 'COACH',
                 permittedDepartureTime = list(earliestTime = earliest1, latestTime = latest1),
                 maxConnectionDuration = connec1)),
          passengers = list(adultCount = 1, infantInLapCount = 0, infantInSeatCount = 0, childCount = 0, seniorCount = 0),
          solutions = 20,
          maxPrice = maxprice,
          #(grepl("BEG|TIA|ZRH|OSL|KEF|IST|SVO|LED|ATL|LAX|JFK|DEN|DEW|SFO|YYZ|YVR|MEX|GRU|SCL|LIM|AEP|BOG|SYD|MEL|PER|AKL|TLV|DXB|DEL|BOM|BLR|CCU|PEK|PVG|CAN|CTU|HKG|
          #SIN|ICN|HND|FUK|CGK|JNB|HRG|CMN|LOS|ALG|NBO|TUN|ADD", airp_mat[i,1]) == FALSE)
          refundable = F))) # default: F - refundable AND non-refundable are listed
    }
  } else {
    if (is.null(datedeparture)){
      warning("Insert a date")}
    else{
      x <- NULL
      for (i in 1:(length(airp_mat[,1]))) 
      {
        #if (airp_mat[i,1] != airp_mat[i,2])
        x[i] <- list(list(
          request = list(
            slice = list(
              #Segment A
              list(origin = airp_mat[i,1], destination = airp_mat[i,2], date = dateorigin, preferredCabin = 'COACH',
                   permittedDepartureTime = list(earliestTime = earliest1, latestTime = latest1),
                   maxConnectionDuration = connec1),
              #Segment B
              list(origin = airp_mat[i,2], destination = airp_mat[i,1], date = datedeparture, preferredCabin = 'COACH',
                   permittedDepartureTime = list(earliestTime = earliest2, latestTime = latest2),
                   maxConnectionDuration = connec2)),
            passengers = list(adultCount = 1, infantInLapCount = 0, infantInSeatCount = 0, childCount = 0, seniorCount = 0),
            solutions = 20,
            maxPrice =  maxprice,
            refundable = F))) # default: F - refundable AND non-refundable are listed
      }
    }}
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  datajson <- NULL
  for (i in 1:(length(airp_mat[,1]))) {
    datajson[i] <- postForm(url, .opts=list(postfields=toJSON(x[[i]]), httpheader=headers))
  }#JSON request from the server, it could take several minutes
  
  
  #--------Data extraction-------
  #The necessary information is extracted from the datajson object
  data1 <- NULL
  datalist <- list()
  for (i in 1:(length(airp_mat[,1]))) 
  {
    data1 <- datajson[i] %>%
      enter_object("trips","tripOption") %>%
      gather_array %>%
      spread_values(
        id = jstring("id"),
        saleTotal = jstring("saleTotal")) %>%
      enter_object("slice") %>% gather_array %>%
      spread_values(slice.duration = jstring("duration")) %>%
      enter_object("segment") %>% gather_array %>%
      spread_values(
        segment.id = jstring("id"),
        segment.cabin = jstring("cabin")) %>%
      enter_object("leg") %>% gather_array %>%
      spread_values(
        segment.leg.origin = jstring("origin"),
        segment.leg.destination = jstring("destination"),
        segment.leg.mileage = jnumber("mileage")) %>%
      select(id, saleTotal,slice.duration,
             segment.leg.origin,segment.leg.destination, segment.leg.mileage, segment.cabin)
    datalist[[i]] <- data1
    
  }
  
  
  #We also extract the information about refundable or not refundable tickets
  reflist <- list()
  
  for (i in 1:(length(airp_mat[,1]))) 
  {
    col_ref <- datajson[i] %>%
      enter_object("trips","tripOption") %>%
      gather_array %>%
      spread_values(
        id = jstring("id"),
        saleTotal = jstring("saleTotal")) %>%
      enter_object("pricing") %>% gather_array %>%
      spread_values(pr.refund = jstring("refundable")) %>%
      select(id, pr.refund)
    reflist[[i]] <- col_ref
  }
  
  carrlist <- list()
  
  for (i in 1:(length(airp_mat[,1]))) 
  {
    carrier_a <- datajson[i] %>%
      enter_object("trips","tripOption") %>%
      gather_array %>%
      spread_values(
        id = jstring("id")) %>%
      enter_object("slice") %>% gather_array %>%
      enter_object("segment") %>% gather_array %>%
      enter_object("flight") %>% 
      spread_values(carrier = jstring("carrier"))%>%
      select(id, carrier)
    carrlist[[i]] <- carrier_a
  }
  
  arrival <- list()
  for (k in 1:(length(airp_mat[,1]))){
    arrival_a <- datajson[i] %>%
      enter_object("trips","tripOption") %>%
      gather_array %>%
      spread_values(
        id = jstring("id")) %>%
      enter_object("slice") %>% gather_array %>%
      enter_object("segment") %>% gather_array %>%
      enter_object("leg") %>% gather_array %>%
      spread_values(destination = jstring('destination'),
                    arrival = jstring("arrivalTime"),
                    departure = jstring('departureTime'))%>%
      select(id, departure, arrival, destination)
    arrival[[i]] <- arrival_a
  }
  #----Data Cleaning----------
  
  #We use lists of objects to apply the loop on all the connections considered
  data_items <- list()
  Business <- list()
  businessID <- list()
  flight_data <- list()
  tripClass <- list()
  dimen <- list()
  onlyCoach <- list()
  onlyCoachID <- list()
  key <- list()
  du <- list()
  duSingle <- list()
  dataframe <- list()
  dataframe_1 <- list()
  durationAR <- list()
  mileageAR <- list()
  id_carr <- list()
  carr_1 <- list()
  carrier <- list()
  carrier_1 <- list()
  idcount <- list()
  names_1 <- list()
  ref <- list()
  pd <- list()
  pd1 <- list()
  date <- list()
  economy <- list()
  origin_A <- list()
  origin_R <- list()
  destination_A <- list()
  destination_R <- list()
  arr1 <- list()
  arr2 <- list()
  for (i in 1:(length(airp_mat[,1]))) 
  {
    if (dim(datalist[[i]])[1] != 0){
      id_carr[[i]] <- unique(sort(datalist[[i]]$id))
      carr_1[[i]] <- with(carrlist[[i]], tapply(carrier, id , I))
      carrier_1[[i]] <- matrix(nrow = length(table(carrlist[[i]]$id)), ncol = 10)
      for (k in 1:length(table(carrlist[[i]]$id))){
        for (j in 1:10){
          carrier_1[[i]][k,j] <- carr_1[[i]][[k]][j]
        }
      }
      carrier[[i]] <- cbind(id_carr[[i]], carrier_1[[i]])
      names_1 <- c("Carrier 1", "Carrier 2", "Carrier 3", "Carrier 4", "Carrier 5", "Carrier 6", "Carrier 7", "Carrier 8",
                   "Carrier 9", "Carrier 10")
      colnames(carrier[[i]]) <- c("id", names_1)
      
      reflist[[i]][is.na(reflist[[i]])]<-"FALSE" #Define FALSE where the refundable variable is NA
      reflist[[i]] <- as.data.frame(reflist[[i]])
      datalist[[i]] <- as.data.frame(datalist[[i]])
      data_items[[i]] <- join(datalist[[i]], reflist[[i]], by='id', type='left', match='all') #Join the original database with the refundable vector, merging for the id of the flight
      flight_data[[i]] <- data_items[[i]] #Consider only non-refundable
      
      tripClass[[i]] <- as.data.frame.matrix(table(flight_data[[i]]$id, flight_data[[i]]$segment.cabin))#Define the class of the flight (i.e. Coach, Business, Premium)
      #This has to be changed for Extra-EU flights.
      dimen[[i]] <- dim(tripClass[[i]])
      if (dimen[[i]][2] == 3){
        onlyCoach[[i]] <- tripClass[[i]][tripClass[[i]][,1] == 0 & tripClass[[i]][,3] == 0,]
        Business[[i]] <- tripClass[[i]][tripClass[[i]][,2] == 0 & tripClass[[i]][,3] == 0,] #For extra-EUtripClass[[i]][tripClass[[i]][,2] == 0 & tripClass[[i]][,3] == 0,]
      } else if (dimen[[i]][2] == 2){
        onlyCoach[[i]] <- tripClass[[i]][tripClass[[i]][,1] == 0,] 
        Business[[i]] <- tripClass[[i]][tripClass[[i]][,2] == 0,]#For Extra-EU tripClass[[i]][tripClass[[i]][,2] == 0,] 
      } else {
        onlyCoach[[i]] <- tripClass[[i]]
        Business[[i]] <- tripClass[[i]]
      } #Consider only the cases where every segment belongs to COACH class
      onlyCoachID[[i]] <- sort(rownames(onlyCoach[[i]]))
      businessID[[i]] <- sort(rownames(Business[[i]]))
      dataframe[[i]] <- data.frame(unique(flight_data[[i]]$id), stringsAsFactors = F)#Merge for the single id
      colnames(dataframe[[i]]) <- c('id')
      #dataframe[[i]] <- flight_data[[i]][flight_data[[i]]$id %in% onlyCoachID[[i]],]
      
      
      
      key[[i]] <- paste(flight_data[[i]]$id, flight_data[[i]]$slice.duration, sep = '')
      
      #duration
      du[[i]] <- data.frame(flight_data[[i]][,c(1,3)],key[[i]])
      colnames(du[[i]]) <- c('id', 'slice.duration','key')
      duSingle[[i]] <- data.frame(droplevels(du[[i]][!duplicated(du[[i]]$key),]))
      duSingle[[i]]$slice.duration <- as.numeric(duSingle[[i]]$slice.duration)
      if (length(duSingle[[i]][,1]) != 0)
        durationAR[[i]] <- aggregate(duSingle[[i]]['slice.duration'], by=duSingle[[i]]['id'], sum)#Sum of the duration if there are different segments
      else {
        durationAR[[i]] <- data.frame(id=0,slice.duration=0)
      }
      dataframe[[i]] <- join(dataframe[[i]], durationAR[[i]], by='id', type='left', match='first')
      
      # mileage
      if (length(duSingle[[i]][,1]) != 0)#Sum of the mileage of the segments
        mileageAR[[i]] <- aggregate(flight_data[[i]]['segment.leg.mileage'], by=flight_data[[i]]['id'], sum)
      else{
        mileageAR[[i]] <- data.frame(id=0,slice.duration=0)
      }
      mileageAR[[i]][2] <- mileageAR[[i]][2]*1.60934
      dataframe[[i]] <- join(dataframe[[i]], mileageAR[[i]], by='id', type='left', match='first')
      
      #direct
      if (length(duSingle[[i]][,1]) != 0)
        idcount[[i]] <- as.data.frame(table(data_items[[i]]$id))#Count the number of single id
      else{
        idcount[[i]] <- data.frame(id=0,segment_number=0)
      }
      colnames(idcount[[i]]) <- c('id','Segment_number')
      idcount[[i]]$direct[idcount[[i]]$Segment_number == 2] <- 1 #Direct is 1 if the number of segment is 2 (roundtrip)
      idcount[[i]]$direct[idcount[[i]]$Segment_number != 2] <- 0 #Direct is 0 if the number of segment is different from 2
      dataframe[[i]] <- join(dataframe[[i]], idcount[[i]], by='id', type='left', match='first')
      
      #refundable
      ref[[i]] <- as.data.frame(flight_data[[i]][,c(1,8)])
      dataframe[[i]] <- join(dataframe[[i]], ref[[i]], by='id', type='left', match='first')
      
      # price
      #It should be implemented with the rest of extra-EU country currencies
      #Exchange rate must be updated
      
      
      if (length(duSingle[[i]][,1]) != 0) {
        pd[[i]] <- flight_data[[i]][,1:2]
      }
      else {
        pd[[i]] <- data.frame(id=0, saleTotal=0, price =0, price1 = 0)
      }
      pd1[[i]] <- pd[[i]][,c(1,2)]
      dataframe[[i]] <- join(dataframe[[i]], pd1[[i]], by='id', type='left', match='first')
      
      
      #date
      if (length(duSingle[[i]][,1]) != 0){
        date[[i]] <- as.character(rep(x[[i]]$request$slice[[1]][3], length(dataframe[[i]][,1])))
      }
      else {
        date[[i]] <- as.character(rep(x[[i]]$request$slice[[1]][3], 1))
      }
      #if (length(duSingle[[i]][,1]) != 0){
      #  economy[[i]] <- matrix(nrow = length(dataframe[[i]][,1]))
      #  economy[[i]][!grepl("BEG|TIA|ZRH|OSL|KEF|IST|SVO|LED|ATL|LAX|JFK|DEN|DEW|SFO|YYZ|YVR|MEX|GRU|SCL|LIM|AEP|BOG|SYD|MEL|PER|AKL|TLV|DXB|DEL|BOM|BLR|CCU|PEK|PVG|CAN|CTU|HKG|
      #                      SIN|ICN|HND|FUK|CGK|JNB|HRG|CMN|LOS|ALG|NBO|TUN|ADD", x[[i]]$request$slice[[1]][1])] <- 1 #Define as economy a flight intra-EU and as Business a flight-ExtraEU
      #  economy[[i]][grepl("BEG|TIA|ZRH|OSL|KEF|IST|SVO|LED|ATL|LAX|JFK|DEN|DEW|SFO|YYZ|YVR|MEX|GRU|SCL|LIM|AEP|BOG|SYD|MEL|PER|AKL|TLV|DXB|DEL|BOM|BLR|CCU|PEK|PVG|CAN|CTU|HKG|
      #                     SIN|ICN|HND|FUK|CGK|JNB|HRG|CMN|LOS|ALG|NBO|TUN|ADD", x[[i]]$request$slice[[1]][1])] <- 0
      #}
      #else {
      #  economy[[i]] <- data.frame(x=0)
      #}
      if (length(duSingle[[i]][,1]) != 0){
        dataframe[[i]] <- cbind(dataframe[[i]], date[[i]])
      }
      else {
        dataframe[[i]] <- data.frame(id = "NA", slide.duration = 0, segment.leg.mileage = 0, Segment_number = 0,
                                     direct = 0, pr.refund = "NA", price1 = 0, 
                                     date = as.character(rep(x[[i]]$request$slice[[1]][3], 1)))
      }
      
      # Define Departure and Arrival
      if (length(dataframe[[i]][,1]) != 0){
        origin_A[[i]] <- as.character(rep(airp_mat[i,1], length(dataframe[[i]][,1])))
        destination_A[[i]] <- as.character(rep(airp_mat[i,2], length(dataframe[[i]][,1])))
        origin_R[[i]] <- destination_A[[i]]
        destination_R[[i]] <- origin_A[[i]]}
      else {
        origin_A[[i]] <- airp_mat[i,1]
        destination_A[[i]] <- airp_mat[i,2]
        origin_R[[i]] <- destination_A[[i]]
        destination_R[[i]] <- origin_A[[i]]
      }
      
      #Join all the pieces together
      #dataframe[[i]] <- dataframe[[i]][dataframe[[i]]$id %in% onlyCoachID[[i]],]
      dataframe[[i]] <- as.data.frame(cbind(dataframe[[i]], origin_A[[i]], destination_A[[i]], origin_R[[i]], 
                                            destination_R[[i]]))
      arr2[[i]] <- as.data.frame(arrival[[i]][arrival[[i]]$destination == airp_mat[i,2],])
      arr1[[i]] <- as.data.frame(arr2[[i]][,c(1,3)])
      carrier[[i]] <- as.data.frame(carrier[[i]])
      dataframe[[i]] <- join(dataframe[[i]], carrier[[i]], by='id', type='left', match='first')
      dataframe[[i]] <- join(dataframe[[i]], arr1[[i]], by='id', type='left', match='first')
      
      colnames(dataframe[[i]]) <- c("id", "Duration", "Distance", "Number of Segment", "Direct", "Refundable", "Price",
                                    "Date", "Origin_A", "Destination_A", "Origin_B", "Destination_B", names_1, "Arrival")
    }
    else {
      
      dataframe[[i]] <- data.frame(id = "NA", Duration = 0, Distance = 0, Number_of_segment = 0,
                                   direct = 0, pr.refund = "NA", price1 = 0, 
                                   Date = as.character(rep(x[[i]]$request$slice[[1]][3], 1)), Origin_A = airp_mat[i,1], Destination_A = airp_mat[i,2], 
                                   Origin_R = airp_mat[i,2], Destination_R = airp_mat[i,1], Carrier_1 = "NA",Carrier_2 = "NA",Carrier_3 = "NA",
                                   Carrier_4 = "NA", Carrier_5 = "NA", Carrier_6 = "NA", Carrier_7 = "NA", Carrier_8 = "NA",
                                   Carrier_9 = "NA", Carrier_10 = "NA", Arrival = "NA")
      colnames(dataframe[[i]]) <- c("id", "Duration", "Distance", "Number of Segment", "Direct", "Refundable", "Price",
                                    "Date", "Origin_A", "Destination_A", "Origin_B", "Destination_B", names_1, 'Arrival')
    }
  }
  
  
  
  onlycoach <- do.call('rbind.fill', unique(onlyCoachID))
  onlycoach <- c(onlycoach)
  business <- do.call('rbind.fill', unique(businessID))
  business <- c(business)
  
  #Unlist the dataframe list to a db
  
  db <- rbindlist(dataframe)
  if (flexible == TRUE){
    db <- db[db$Refundable == TRUE,]
  } else{
    db <- db
  }
  
  if (class == "Economy"){
    db <- db[db$id %in% onlycoach,]
  } else if (class == "Business"){
    db <- db[!db$id %in% onlycoach,]
  } else if (class == "Both"){
    db <- db
  } else{
    warning("Insert a valid class")
  }
  
  for (h in 1:length(airport$Origin)){
    db$city1[db$Origin_A == airport$Code[h]] <- airport$City[h]
    db$city2[db$Destination_A == airport$Code[h]] <- airport$City[h]
  }
  
  db$Economy[db$id %in% onlycoach] <- "Economy"
  db$Economy[!db$id %in% onlycoach] <- "Business"
  
  
  
  db$route <- paste(db$Origin_A, db$Origin_B, sep = '-')
  
  db$Currency <- sub("^([[:alpha:]]*).*", "\\1", db$Price)
  
  db$Amount <- as.numeric(str_extract(db$Price, "\\d+\\.*\\d*"))
  
  db
  
}
