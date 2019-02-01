# Project-Reality-Hog-server-maps-visualization
Visualization of Project-reality's(popular bf2 mod) maps played on Hog server starting from January 2019. Data is exrtracted to google spreadsheets through R.script from href links of battlerecorder webpage. Then data from spreadsheet is further imported to Tableau Public software to visualize 

Running an auto script through task scheduler to daily refresh/update the data source for the Visualization chart.

library(rvest)
library(magrittr)
library(XML)
library(stringr)
library(googlesheets)

#Register googsheets
hogsheet <- gs_title("HogMapsData")
hog_aas_sheet <- gs_title("HogAAS")
hog_ins_sheet <- gs_title("HogIns")

#formats yesterday's date in a way that matches the text in href links to extract the right date info
brdate <- format(Sys.Date()-1,"tracker_%Y_%m_%d")

#Reads the HTML code from the website
brWeb <- read_html('http://64.237.49.109/br/demos/demos/')

#finds the hrefs link's text located in <div id="listing"> of html
#stores all the maps that contains brdate = yesterday's date
Hogmapnames_data <- brWeb %>% html_nodes(xpath = paste0("//*[@id='listing']/div/a[contains(@href,'",brdate,"')]")) %>% xml_attr("href")

#Converts extracted text into strings by Map's name & GameMode's name
a <- str_sub(Hogmapnames_data,29,-1)
Maps <- str_split(a, "_gpm", simplify = T)[,1]
GameMode <- str_split(str_split(a, "_gpm", simplify = T)[,2], "_", simplify = T)[,2]
Hog_data <- data.frame(Maps,GameMode)

#appends the new data/values to google-spreadsheet
gs_add_row(hogsheet, ws = "Sheet1", input = Hog_data)

#Reads updated hogsheet fom google spreadsheet
h_sheetupdated <- gs_read(hogsheet)

#filters the Maps column by cq condition and groups the column by each map. Stores the count and percentage in new columns
Hog_AAS.pct = h_sheetupdated %>% filter(GameMode == "cq") %>% group_by(Maps) %>%
  summarise(count=n()) %>% 
  mutate(pct=count/sum(count)) %>% arrange(pct)

#filters the Maps column by insurgency condition and groups the column by each map. Stores the count and percentage in new columns
Hog_INS.pct = h_sheetupdated %>% filter(GameMode == "insurgency") %>% group_by(Maps) %>%
  summarise(count=n()) %>% 
  mutate(pct=count/sum(count)) %>% arrange(pct)

#updates the cells in corresponding spreadsheet
gs_edit_cells(hog_aas_sheet, ws = "Sheet1", input = Hog_AAS.pct)
gs_edit_cells(hog_ins_sheet, ws = "Sheet1", input = Hog_INS.pct)
