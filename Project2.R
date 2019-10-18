# shinyServer(function(input, output){
#   
#   #reruns when input changes i.e. location
#   observe({
#     
#     #looks for data using API
#       #loading necessary packages
#       library(owmr)
#       library(leaflet)
#     
#       # store API key in an environment variable called OWM_API_KEY
#       Sys.setenv(OWM_API_KEY = 'e366d11329936ebfaaf4cf08af0ff523')
#       
#       #choice of city chosen by user
#       choiceCity = search_city_list(input)
#       
#       #converts data into tibble
#       wm_data = find_city(search, units = 'metric') %>%
#         owmr_as_tibble()
#       
#       #creates map
#       output = leaflet() %>%
#         addTiles() %>%
#         add_weather(owm_data,
#                     template = '<b>{{name}}</b>, {{temp}}°C',
#                     icon = owm_data$weather_icon)
#       output
#     
#   })
# }) #end observe
# setup -------------------------------------------------------------------
library(owmr)
library(leaflet)

# store API key in an environment variable called OWM_API_KEY
Sys.setenv(OWM_API_KEY = 'e366d11329936ebfaaf4cf08af0ff523')

# Access to weather API ---------------------------------------------------

wm_data = find_city('London, uk', units = 'metric') %>%
  owmr_as_tibble()
map = leaflet() %>%
  addTiles() %>%
  add_weather(owm_data,
              template = '<b>{{name}}</b>, {{temp}}°C',
              icon = owm_data$weather_icon)
map            

# result = get_forecast('St Andrews', units = 'metric')
# result = result %>% owmr_as_tibble()
# 
# names(find_city('St Andrews'))
# names(result)
# 
# 
# 
# forecast <- get_forecast("London")$list
# weather <- flatten_weather(forecast$weather)
# icons <- get_icon_url(weather$icon)