## Scrape Health Issues, correlate w/ mortality and with "internet interest in node"
library(rvest)
library(dplyr)
library(visNetwork)
library(RColorBrewer)
library(plotly)


############# GET HIREARCHY DATA ################
## Make sure to change working directory to read this. I got this by running "GetHirearchy.R" to get this!

HappyFamily <- read.csv("HappyFamily.csv",stringsAsFactors = F)
HappyFamily <- HappyFamily[,-1]

############# CREATE NODES AND EDGES DATA.FRAMES ################

## To have ONE node where everything starts, run this line of code
# HappyFamily$Parent[is.na(HappyFamily$Parent)] <- "DISEASES"

source("https://raw.githubusercontent.com/mexindian/networkEasyMode/master/easyModeNodeEdge.R")
nodesEdges <- easyMode(HappyFamily[,c(1,4,2)],1)
nodes <- nodesEdges[[1]]
nodes <- nodes[nodes$name !="",]
edges <- nodesEdges[[2]]

## Make all edges width=1
edges$value <- 1
edges <- edges[22:nrow(edges),]

## Add some initial node metadata: mouseover=title; display=label; size=value
nodes$code <- nodes$name
nodes$longName <- HappyFamily$Naames[match(nodes$name,HappyFamily$ID)]
# nodes$value <- 1
# nodes$value[nodes$name=="DISEASES"] <- 20
# nodes$value[nodes$name==HappyFamily$ID[HappyFamily$Parent=="DISEASES"]] <- 10

############# NODE SIZE ################
## Get the sizes from "internet popularity"
## RUN "getNodeSize.R" to get this!
nodeSize <- read.csv("nodes.csv",stringsAsFactors = F)
nodeSize <- nodeSize[,c(3,6)]

nodes <- left_join(nodes,nodeSize,by='id')


############# ADD COLOR BY INFECTIOUS DISEASE ################
## 2 options: either ## Parent relationships to color all nodes underneath? 

## I'll use the mortality rates, which were harvested from http://wonder.cdc.gov/mcd-icd10.html. The EXACT results provided
## are loaded into this script in order to not violate any laws. I'm providing this data to the public for reproducibility.
## If doing so violates any laws, please contact me directly at amit@amitkohli.com and I'll remove the dataset immediately.
## the metadata on what query was conducted exactly is provided in the file itself.
diseases <- read.table("Multiple Cause of Death, 1999-2014.txt",
                       header=T, fill = TRUE, sep = "\t")

## Remove metadata by finding "---" and removing everything below (inclusive)
diseases <- diseases[1:grep("---",diseases$Notes)[1]-1,]

## Now, assign mortality data to nodes. Since they have a greater level of disaggregation, we need to summarize
diseases$codes <- gsub("\\..+","",diseases$Underlying.Cause.of.death.Code)

## Disaggregate
tempDF <- diseases %>% group_by(codes) %>% summarize(a = sum(Deaths))
names(tempDF) <- c("name","deaths")
tempDF <- tempDF[tempDF$name != "",]

## sub-Chapter Code
tempDF2 <- diseases %>% group_by(UCD...ICD.Sub.Chapter.Code) %>% summarize(a = sum(Deaths))
names(tempDF2) <- c("name","deaths")
tempDF2$name <- as.character(tempDF2$name)
tempDF2 <- tempDF2[tempDF2$name !="",]

## Chapter Code
tempDF3 <- diseases %>% group_by(UCD...ICD.Chapter.Code) %>% summarize(a = sum(Deaths))
names(tempDF3) <- c("name","deaths")
tempDF3$name <- as.character(tempDF3$name)
tempDF3 <- tempDF3[tempDF3$name !="",]

## Unite them
deaths <- bind_rows(tempDF,tempDF2)
deaths <- bind_rows(deaths,tempDF3)
nodes$deaths <- deaths$deaths[match(nodes$name,deaths$name)]

# nodes$value <- nodes$deaths

## cut deaths into categories, suitable to have a color assigned:
nodes$deathCats <- cut(nodes$deaths,c(max(nodes$deaths,na.rm=T),seq(from=0, to=2500,by=350)))

colReference <- data.frame(cat =c(NA,levels(nodes$deathCats)),
                           col=brewer.pal(length(nodes$deathCats %>% unique), 'OrRd'))

## But NA's is also getting colored... turn that grey
colReference$col <- as.character(colReference$col) 
colReference[is.na(colReference$cat),2] <- "#f0f0f0"

nodes$color <- colReference$col[match(nodes$deathCats,colReference$cat)]

nodes$title <- paste(nodes$longName,"<br> Deaths: ", nodes$deaths, "<br> Internet attention: ",nodes$value,sep="")
nodes$label <- nodes$name 

############# PLOT!!! ################
## test first! :)
visNetwork(head(nodes,400),head(edges,800)) %>% visEdges(smooth = FALSE) %>% 
  visPhysics(stabilization = FALSE) 

## Looks Ok, now commit to it!
## HTML
toSave <- visNetwork(nodes,edges,height = 960) %>% visEdges(smooth = FALSE) %>% visPhysics(stabilization = FALSE)
visSave(toSave,"OverallNetwork.deaths.popularity.html",selfcontained = T)
 
# ## png... but since there's no mouse-over, write all info down in titles
# nodes$title <- paste(nodes$name, "\n", nodes$longName,
#                      "\n Deaths: ", nodes$deaths, 
#                      "\n Internet attention: ",nodes$value,sep="")
# 
# toSave2 <- visNetwork(nodes,edges,height = 9600, width = 9600) %>% visEdges(smooth = FALSE) %>% visPhysics(stabilization = FALSE)
# visExport(toSave2,type = "png","Overall Network",background = "#F8ECC2")

########################## NOW DO ONE PER CATEGORY ####
## First, set the way I want nodes to show:
nodes$title <- paste(nodes$name,"<br>",nodes$longName,"<br> Deaths: ", nodes$deaths,
                     "<br> Internet attention: ",nodes$value,sep="")
nodes$label <- nodes$longName

## Difficult to do this smartly since codes span letters & number ranges... do it stupidly:

HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="A"|
    gsub("(.).+","\\1",HappyFamily$Parent) =="B"] <- "A00-B99"

HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="C"|
                       (gsub("(.).+","\\1",HappyFamily$Parent) =="D" &
                       as.numeric(gsub(".(.).+","\\1",HappyFamily$Parent)) <= 4)] <- "C00-D49"
HappyFamily$Category[(gsub("(.).+","\\1",HappyFamily$Parent) =="D" &
                        as.numeric(gsub(".(.).+","\\1",HappyFamily$Parent)) > 4)] <- "D50-D89"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="E"] <- "E00-E89"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="F"] <-  "F01-F99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="G"] <-  "G00-G99"
HappyFamily$Category[(gsub("(.).+","\\1",HappyFamily$Parent) =="H" &
                        as.numeric(gsub(".(.).+","\\1",HappyFamily$Parent)) <= 5)] <- "H00-H59"
HappyFamily$Category[(gsub("(.).+","\\1",HappyFamily$Parent) =="H" &
                        as.numeric(gsub(".(.).+","\\1",HappyFamily$Parent)) > 5)] <- "H60-H95"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="I"] <-"I00-I99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="J"] <-"J00-J99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="K"] <-"K00-K95"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="L"] <-"L00-L99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="M"] <-"M00-M99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="N"] <-"N00-N99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="O"] <-"O00-O9A"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="P"] <-"P00-P96"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="Q"] <-"Q00-Q99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="R"] <-"R00-R99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="S"|
                        gsub("(.).+","\\1",HappyFamily$Parent) =="T"] <-"S00-T88"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="V"|
                       gsub("(.).+","\\1",HappyFamily$Parent) =="W"|
                       gsub("(.).+","\\1",HappyFamily$Parent) =="X"|
                       gsub("(.).+","\\1",HappyFamily$Parent) =="Y"] <- "V00-Y99"
HappyFamily$Category[gsub("(.).+","\\1",HappyFamily$Parent) =="Z"] <- "Z00-Z99"

## Check if I messed it up:
# HappyFamily[is.na(HappyFamily$Category),] %>% View

## and now loop through each of the Categories to plot
Categories <- HappyFamily$Category%>%unique
Categories <-  Categories[!is.na(Categories)]


for(i in 1:length(Categories)){
  nodesToKeep <- HappyFamily %>% filter(Category==Categories[i]) %>% select(Naames)
  ## And lastly, add in the main node for that category :)
  nodesToKeep <- c(nodesToKeep$Naames,HappyFamily$Naames[HappyFamily$ID == Categories[i]])
  
  ## and now limit the nodes frame to those nodes... we don't need to worry about the edges
  ## since they need nodes to plot :)
  filteredNodes <- nodes[nodes$longName %in% nodesToKeep,]
  
  ## PLOT!!!!!!!! 
  thingie <- visNetwork(filteredNodes,edges,
             main=paste("Hirarchichal Network of ",
                        HappyFamily$Naames[HappyFamily$ID==Categories[i]])) %>% 
    visEdges(smooth = FALSE) %>%
    visPhysics(stabilization = FALSE)
  visSave(thingie,paste("Network.deaths.popularity.",Categories[i],".html",sep=""),selfcontained = T)
  
}

########################## overall bar chart on mortality ####
## Aggregate nodes by code, but then don't put the node, but the name per se
nodes$Category <- HappyFamily$Category[match(nodes$name,HappyFamily$ID)]
nodes$Category <- HappyFamily$Naames[match(nodes$Category,HappyFamily$ID)]

## wrap those nasty long names
wrap_strings  <- function(vector_of_strings,width){as.character(sapply(vector_of_strings,FUN=function(x){paste(strwrap(x,width=width), collapse="\n")}))}
nodes$Category <- wrap_strings(nodes$Category,40)

## OK, do the aggregation, but only for the grandchildren (otherwise there's doublecounting)
nodes[!grepl("-",nodes$name),] %>% 
  group_by(Category) %>%
  summarize(d = sum(deaths,na.rm=T)) %>%
  # arrange(d) %>%
  ggplot(aes(x=Category,y=d)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


