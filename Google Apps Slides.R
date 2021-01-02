library(officer)
library(magrittr)
library(tidyverse)
library(kableExtra)
library(benford.analysis)


google <- read_csv('non-repeat-apps.csv')
pres1 <- read_pptx("google apps.pptx") 



#Slide 1
pres1  %<>%  add_slide(layout = "Title Slide", master = "Gallery") %>% 
  ph_with_text(type = "ctrTitle", str = "Google Playstore Apps") %>% 
  ph_with_text(type = "subTitle", str = "By David Anderson")

#Slide 2
image <- "https://www.notebookcheck.net/fileadmin/Notebooks/News/_nc3/20180131_Play_Store_Logo.png"
download.file(image,'image.jpg', mode = 'wb')

pres1  %<>%  add_slide(layout = "Title and Content", master = "Gallery") %>% 
  ph_with_text(type = "title", str = "Introduction and Goals") %>% 
  ph_with_ul(type = "body", index = 1, str_list = c("Mobile Apps has become a Giant Industry:", "2016: 88.3 Billion in Revenue, 90 Billion Downloads", "Millions of Apps. How many could you name?", "Known issue of false reviews/downloads", "Goals of This Study:", "Understand Google Play Apps", "Categorical Breakdown, Average Ratings, Average Reviews, etc.", "Perform Benford Analysis to Investigate Falsely Reviewed Apps"), level_list = c(1,2,2,2,1,2,3,2)) 
  

#Slide 3
pres1  %<>%  add_slide(layout = "Title and Content", master = "Gallery") %>% 
  ph_with_text(type = "title", str = "Data Overview") %>% 
  ph_with_ul(type = "body", index = 1, str_list = c("Kaggle Dataset on Scrapped Information", "About 10,000 Apps", "Dataset With Each Review", "Information Available:", "Number of Reviews", "Overall Rating", "App Size", "Rough Number of Installs","App Category"), level_list = c(1,2,2,1,2,2,2,2,2)) %>% 
  ph_with_img_at(src = "image.jpg",height = 3, width = 3, left = 7, top = 3)

#slide 4

gg1 <- ggplot(google,aes(Reviews/1000000))+geom_histogram()+labs(title = 'Histogram of Number of App Reviews', x = "Number of Reviews (in Millions)", y = "Number of Apps")
gg2 <- ggplot(google,aes(Rating))+geom_density(color = 'black',fill = 'coral')+
  labs(title = "Distribution of App Ratings",x = "Rating", y = "Density")

pres1 %<>% add_slide(layout = "Title and Content", master = "Gallery") %>% 
  ph_with_text(type = "title", str = "Reviews and Ratings Distributions") %>% 
  ph_with_gg(gg1,width = 5,height = 4) %>% 
  ph_with_gg_at(gg2,width = 5,height = 4,left = 7, top = 2)

#Slide 5
categories <- google %>% filter(Rating != 'NaN') %>% group_by(Category) %>% summarise(Number = n(),'Average Rating' = round(mean(Rating),2),'Average Reviews' = round(mean(Reviews),2),Total = sum(Reviews),Percent = (n()/10839)*100,Installs = sum(Installs))
colored2 <- ifelse(categories$Installs > 5000000000, "red", "black")
gg3 <- ggplot(categories)+
  geom_col(aes(Category,Installs/1000000,fill=ifelse(Installs > 5000000000,"A", "B")))+
  scale_fill_manual(guide=FALSE, values=c("red", "black"))+
  theme(axis.text.x = element_text(color = colored2,angle = 75, hjust = 1))+
  labs(title = "Total Installs by Category", y = "Total Installs", x = "Category")
 

pres1 %<>% add_slide(layout = "Title and Content", master = "Gallery") %>% 
  ph_with_text(type = "title", str = "Categories") %>% 
  ph_with_text(type = "body", str = "While the 'Family' category has the most apps, we see that 'Games', 'Communication' and 'Social Media' dominate the downloads") %>% 
  ph_with_gg_at(gg3,width = 8,height = 4,left = 3,top = 3.5)

#Slide 6


pres1 %<>% add_slide(layout = "Two Content", master = "Gallery") %>% 
  ph_with_text(type = "title", str = "Benford Analysis") %>% 
  ph_with_text(type = "body", str = "Searching for apps with false reviews with Benford Analysis, we see that the distribution of digits is actually fairly consistent with Benford's Law. When searching for suspicious apps, I examined the suspect list from Benford's analysis. Each apps' ratio of reviews to installs was used to create the top suspect apps for false reviews.") %>% 
  ph_with_img_at('Rplot.png',width = 6,height = 4, left = .5, top = 2)




print(pres1, target = "google apps.pptx")



