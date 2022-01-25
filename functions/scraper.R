# ---------------- #
# --- Packages --- #
# ---------------- #
pacman::p_load(dplyr)
pacman::p_load(XML)
pacman::p_load(stringr)
pacman::p_load(lubridate)

url <- paste0("https://www.nycgovparks.org/facilities/recreationcenters/M260/schedule#Center")

urltxt <- readLines(url)
urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))

doc <- htmlParse(urltxt)
data <- xpathApply(xpathApply(doc, "//td")[[1]], "//p[@class='program']")

left_off_ind <- 1

new_day = F

last_time <- format(strptime("01:00 AM", "%I:%M %p"), format="%H:%M:%S")

looped_data <- tibble()

weekday_df <- tibble(
  day = c(
    "Mon",
    "Tue",
    "Wed",
    "Thu",
    "Fri",
    "Sat",
    "Sun"
  ),
  ind = c(
    seq(1:7)
  )
)

for (i in 1:6){
  
  center <- xmlValue(xpathApply(doc, "//h1"))
  
  day_of_week <- weekday_df[i,1] %>% 
    pull(day)
  
  num_date <- gsub(
    "\n",
    "",
    trimws(str_split(xmlValue(xpathApply(doc, "//th")[[i]]), "y", n = 2, simplify = T)[,2])
  )
  
  current_date <- paste(day_of_week, num_date)
  
  for (act in left_off_ind:length(data)){
    
    building_hours_header <- (xpathApply(doc, "//td")[[i]])
    
    building_hours <- gsub(
      "BuildingHours",
      "",
      gsub(
        "\\s+",
        "",
        xmlValue(xpathApply(building_hours_header, "//div[@class='center-hrs']")[[i]])
      )
    )
    
    room <- xmlValue(data[[act]][["span"]]) 
    
    activity <- xmlValue(data[[act]][["a"]])
    
    if (activity == "Baseline"){
      next
    }
    
    activity_time <- gsub(
      " ",
      "",
      str_split(trimws(xmlValue(data[[act]])), "\n", n = 3, simplify = T)[,1],
    )
    
    end_time <- str_split(activity_time, "-", n = 2, simplify = T)[,2]
    
    if (str_detect(end_time, "a")){
      end_time <- gsub(
        "a",
        " AM",
        end_time
      )
    }else{
      end_time <- gsub(
        "p",
        " PM",
        end_time
      )
    }
    
    act_end_time <- format(strptime(end_time, "%I:%M %p"), format="%H:%M:%S")
    
    if (act_end_time > last_time | new_day){
      last_time <<- act_end_time
      new_day <- F
    }
    
    if (act_end_time < last_time){
      left_off_ind <<- act
      new_day <- T
      break
    }
    
    data_to_append <- tibble(
      center = center,
      day = current_date,
      room = room,
      activity = activity,
      activity_time = activity_time
    )
    
    looped_data <- bind_rows(looped_data, data_to_append)
    
  }
  
}




