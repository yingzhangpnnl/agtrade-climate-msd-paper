# Ying Zhang plot circular graph of import-export differences btw two scenarios
# require(tidyr)
library(dplyr)
library(stringr)
library(circlize)
library(RColorBrewer)
library(flipChartBasics)
library(tibble)

rm(list=ls())

Rawdatapath= "./data/"

#region mapping
Regmap <- read.csv("./data/Regmapping.csv", header=TRUE, sep=",",comment.char = "#")

#Read and process queried data
qAgdem = "Agdemand"; file = paste0(Rawdatapath, qAgdem, ".csv")
assign(qAgdem,  tidyr::gather(read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE),
                              "year", "value", -c(scenario, region, input, sector, Units)))

qAgprod = "Agprod"; file = paste0(Rawdatapath, qAgprod, ".csv")
assign(qAgprod,  tidyr::gather(read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE),
                               "year", "value", -c(scenario, region, output,sector, Units)))

qAgsce = "RegAgsource"; file = paste0(Rawdatapath, qAgsce, ".csv")
assign(qAgsce,  tidyr::gather(read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE),
                              "year", "value", -c(scenario, region, sector, subsector, input, technology, Units)))

scList <-unique(Agprod["scenario"])
cropSel <- list("corn","othergrain","rice","wheat") # cereals

# loop for calc import-export for each scenario i, here we are looking only at the SS scenario minus RF, thus c(1,3)
for (i in c(1,3)){

  scTarget<-scList[i,1] # change sc target to e.g. scList[2,1] or scList[3,1] or scList[4,1]

  Agdemand1 <- Agdemand %>% 
    filter(scenario == scTarget) %>%
    mutate(year = as.numeric(substr(year,2,5))) %>% rename(unit = Units, crop = input) %>%
    mutate(crop = if_else(str_detect(crop, "regional"),tolower(substr(crop,nchar("regional")+2, nchar(crop))) , tolower(crop))) %>%
    mutate(crop = if_else(str_detect(crop, "_"),gsub("_","",crop),crop)) %>%
    group_by(region, crop, year, unit) %>% summarise(value = sum(value)) %>% ungroup() %>% 
    filter(crop %in% cropSel) %>% 
    group_by(region,year,unit) %>% summarise(value=sum(value)) %>% ungroup()
  
  #############
  
  Agprod1 <- Agprod %>%
    filter(scenario == scTarget) %>%
    mutate(year = as.numeric(substr(year,2,5))) %>% rename(crop = output, unit = Units) %>%
    mutate(crop = tolower(crop)) %>% 
    filter(crop %in% cropSel) %>% 
    group_by(region,year,unit) %>% summarise(value=sum(value)) %>% ungroup()
  
  ############
  
  RegAgsource1 <- RegAgsource %>%
    filter(scenario == scTarget) %>%
    mutate(year = as.numeric(substr(year,2,5))) %>% rename(unit = Units, crop = sector) %>%
    mutate(crop = if_else(str_detect(crop, "regional"),tolower(substr(crop,nchar("regional")+2, nchar(crop))), tolower(crop))) %>%
    mutate(crop = if_else(str_detect(crop, "_"),gsub("_","",crop),crop)) %>%
    mutate(source = if_else(str_detect(subsector, "domestic"), "domestic", "imported")) %>%
    dplyr::select(region, crop, year, unit, source, value) %>% 
    filter(crop %in% cropSel) %>% 
    group_by(region,year,unit,source) %>% summarise(value=sum(value)) %>% ungroup()
  
  ##############
  #data for plotting sum of crops selected above (cropSel) and the year defined below (yy)
  yy = 2050
  trade <- Agdemand1 %>% rename(consume = value)  %>% 
    left_join(Agprod1 %>% rename(prod= value)) %>% 
    left_join(RegAgsource1 %>% filter(source == "imported")%>% rename(imported = value)) %>%
    mutate(export = prod + imported - consume, domestic = prod - export) %>% 
    filter(year == yy) %>% left_join(Regmap) 

  trade[is.na(trade)]<-0
  assign(paste("tradeSc", i, sep = ""), trade)  

} # end loop for scenarios 


tradeDif <- tradeSc3 %>%  left_join(tradeSc1,by=c("region","year","source","RegID","RegID1")) %>% 
  mutate(imDif=imported.x-imported.y,exDif=export.x-export.y) %>% select(region,year,imDif,exDif,RegID,RegID1)

dat_circular0 <- tradeDif %>% mutate(world = "World", RegID = as.character(RegID)) %>%
  mutate(RegNO = substr(RegID,2,nchar(RegID))) %>% arrange(as.numeric(RegNO)) 

#note that nn is the number of the regions presented after the automated aggregation (based on flow size)
nn = 7 #nn<31 regions
dd <- tradeDif %>% mutate(world = "world", RegID = as.character(RegID)) %>%
  mutate(RegNO = substr(RegID,2,nchar(RegID))) %>% arrange(as.numeric(RegNO))  %>%
  mutate(topplayer = abs(imDif) + abs(exDif)) %>% 
  arrange(desc(topplayer)) %>% top_n(nn) %>% 
  dplyr::select(RegID1) %>% add_row(RegID1 = "World") %>% 
  rename(RegID2 = RegID1) %>% mutate(RegID2 = as.character(RegID2))

dd1 <- unique(dd$RegID2)

`%notin%` <- Negate(`%in%`)

dat_circular <- dat_circular0 %>% select(REG_ex = RegID1, REG_im = world, flow = exDif) %>% 
  bind_rows(dat_circular0 %>% mutate(REG_ex = "World") %>% select(REG_ex, REG_im = RegID1, flow = imDif)) %>%
  mutate(REG_ex = if_else(REG_ex %notin% dd$RegID2, "Other regions", REG_ex)) %>%
  mutate(REG_im = if_else(REG_im %notin% dd$RegID2, "Other regions", REG_im)) %>% 
  group_by(REG_ex, REG_im) %>% summarise(flow = sum(flow)) %>% ungroup()


dat_circular<-dat_circular %>% filter(REG_im=="World",flow>0) %>% 
  arrange(flow) %>% 
  bind_rows(dat_circular %>% 
              filter(REG_im=="World",flow<0) %>% 
              arrange(flow)) %>% 
  bind_rows(dat_circular %>% 
              filter(REG_im!="World",flow<0) %>% 
              arrange(flow)) %>% 
  bind_rows(dat_circular %>% 
              filter(REG_im!="World",flow>0) %>% 
              arrange(desc(flow))) 

linkcol<- dat_circular %>% filter(REG_im=="World") %>% 
  mutate(linkcol1=if_else(flow<0,"#C70039FF","#41D302FF")) %>% 
  bind_rows(dat_circular %>% 
              filter(REG_im!="World") %>%
              mutate(linkcol1=if_else(flow<0,"#C7008E","#02D390FF"))) 
linkcol1<-as.character(linkcol$linkcol1)


png(file = "Figure3.png", width = 3500, height = 3500, res = 500)
chordDiagram(as.data.frame(dat_circular),link.sort = T,link.decreasing = F,
             transparency = 0.5,
             directional = 1,
             direction.type = c("diffHeight", "arrows"), 
             diffHeight = -uh(2, "mm"),
             link.arr.type = "big.arrow"
             , col = linkcol1
             , grid.col = "#CEC4C3FF"
             ,annotationTrack = c("grid")
             ,preAllocateTracks = list(list(track.height = c(0.3))
                                       ,list(track.height = c(0.035))
             ))

circos.track(track.index = 3, panel.fun = function(x, y) {
  circos.axis(h = 1, labels.cex = 0.8)   
}, bg.border = NA)

circos.track(track.index = 1,
             panel.fun = function(x, y) {
               xlim = get.cell.meta.data("xlim")
               xplot = get.cell.meta.data("xplot")
               ylim = get.cell.meta.data("ylim")
               sector.name = get.cell.meta.data("sector.index")
               
               circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                           niceFacing = TRUE, adj = c(0, 0.5), col = "black",
                           cex = 1)
               
             }, bg.border = NA)

dev.off() #dump




