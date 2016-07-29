############# NODE SIZE ################
if(!file.exists("nodes.csv")){
  ## The node sizes should be the largeness of the internet perception of the disease, so go to Bing and 
  ## grab the # of websites that return on an exact query of each specific disease
  
  ## By the time you run this, you should already have the HappyFamily
  DiseaseNames <- data.frame(label=HappyFamily[,c(1,4)] %>% unlist %>% as.character() %>% unique())
  
  for (i in 1:nrow(DiseaseNames)){
    toLookUp <- gsub(" ","%22",paste('http://www.bing.com/search?q="',DiseaseNames$label[i],'"',sep=''))
    a <- read_html(toLookUp)
    a %>% html_nodes(".sb_count") %>%html_text() -> DiseaseNames$value[i]
  }
  ## NOTE!!! The matches are not exact, the quotation marks are being ignored... 
  ##   accept because the relative size is what's important... but... :(
  
  ## OK, clean up the results and divide by 1000
  DiseaseNames$value <-  as.numeric(gsub(",","",gsub(" .+","",DiseaseNames$value)))/1000
  
  write.csv(DiseaseNames,"nodes.csv")
} else {
  DiseaseNames <- read.csv("nodes.csv",stringsAsFactors = F)
  nodes <- nodes[,-1]
}