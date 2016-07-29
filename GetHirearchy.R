############# GET HIREARCHY DATA ################

if(!file.exists("HappyFamily.csv")){
  ## Go to site:http://www.icd10data.com/ICD10CM/Codes and harvest these nodes: ".contentBody li , .identifier a"
  ## Intermediate pages: .noTopPadding li  (x2)
  ## Final pages: .contentBlurb (if you want to go deeper... I didn't)
  
  ## Process: So go to top page, get those links. Then for each of those links, 
  ## go off and get all the links under that (and tie them to the parent). 
  ## Continue like this until you arrive at final page for each category.
  
  a <- read_html("http://www.icd10data.com/ICD10CM/Codes")
  a %>% html_nodes(".contentBody li , .identifier a") -> b
  
  ## I need the IDs, the name, and the link (a bit messy... not sure why.)
  Links <- b %>% html_attr("href")
  Links <- paste("http://www.icd10data.com/",Links[!is.na(Links)],sep="")
  
  Name.ID <- b %>% html_text()
  Naames <- gsub(".+  ","",Name.ID[grep(" ",Name.ID)])
  ID <- Name.ID[-grep(" ",Name.ID)]
  
  Parents <- data.frame(ID,Naames,Links)
  
  ## Now do level 2
  for (i in 1:nrow(Parents)){
    c <- read_html(as.character(Parents$Links[i]))
    c %>% html_nodes(".noTopPadding li") -> d
    
    Links <- d %>% html_children() %>% html_attr("href")
    Links <- paste("http://www.icd10data.com",Links[!is.na(Links)],sep="")
    
    Name.ID <- d %>% html_text()
    Naames <- gsub(".+  ","",Name.ID[grep(" ",Name.ID)])
    ID <- gsub("  .+","",Name.ID[grep(" ",Name.ID)])
    
    TempDF <- data.frame(ID,Naames,Links, Parent=Parents$ID[i])
    
    if(exists("Children")){
      Children <- bind_rows(Children,TempDF)
    } else {Children <- TempDF}
  }
  
  # Children$Links<- gsub("//","/",Children$Links)
  
  ## And one more... level 3!
  for (i in 1:nrow(Children)){
    e <- read_html(as.character(Children$Links[i]))
    e %>% html_nodes(".noTopPadding li") -> f
    
    Links <- f %>% html_children() %>% html_children() %>% html_attr("href")
    Links <- paste("http://www.icd10data.com/",Links[!is.na(Links)],sep="")
    
    Name.ID <- f %>% html_text()
    Naames <- gsub(".+  ","",Name.ID[grep(" ",Name.ID)])
    ID <- gsub("  .+","",Name.ID[grep(" ",Name.ID)])
    
    TempDF <- data.frame(ID,Naames,Links, Parent=Children$ID[i])
    
    if(exists("Grandkids")){
      Grandkids <- bind_rows(Grandkids,TempDF)
    } else {Grandkids <- TempDF}
  }
  
  ## Now join them all together!
  HappyFamily <- bind_rows(Parents,Children,Grandkids)
  
  ## And write it for posterity!!
  write.csv(HappyFamily,"HappyFamily.csv")
} else {
  ## It already exists!
  HappyFamily <- read.csv("HappyFamily.csv",stringsAsFactors = F)
  HappyFamily <- HappyFamily[,-1]
}