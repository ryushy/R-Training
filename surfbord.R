library(tidyverse)
library(rvest)
library(stringr)

tar_url  <- "https://bo-doya.com/item/117912/"
tar_url  <- "https://bo-doya.com/item/143064/"
csv_all  <- data_frame()

#WebページのURLを指定し、データを取得
performance_html <- read_html(tar_url,encoding = "UTF-8")
media <- html_nodes(performance_html, "table")

media[[1]]%>% html_table()


dt0 = media[[2]] %>% html_table()
names(dt0) = dt0[1,]
dt0 = dt0[-1,]
dt0


FeetChange <- function(feet){

  str_match(feet, "\\(.+?\\)") %>%
    str_replace(pattern = "\\(", replacement = "") %>%
    str_replace(pattern = "cm\\)", replacement = "") %>%
    as.numeric()

}


dt <-
  dt0 %>%
  mutate(length = FeetChange(長さ),
         size   = str_replace(長さ, pattern = "\\(.+?\\)", replacement = ""),
         width  = FeetChange(幅),
         thick  = FeetChange(厚み),
         volume = as.numeric(str_replace(容量,pattern = "L", replacement = ""))
  ) %>%
  select(
    size,
    length,
    width,
    thick,
    volume
  )

dt

plot(dt$length,dt$volume)
plot(dt$width,dt$volume)
plot(dt$thick,dt$volume)

model = lm(data = dt, volume ~ length + width + thick)
summary(model)

model2 = lm(data = dt, volume ~ length  + thick)
summary(model2)


pd = data.frame(
  length = 200, 
  width  = 47.19, 
  thick  = 7
  )

predict(model,pd)
predict(model2,pd)



# model1
length = 185.4
width  = 47.19
thick  = 5.97
(l1 = -55.52222 +  0.17843*length +5.61948*thick + 0.35395*width)


# model2
(l2 = -45.39605 +  0.19248*length + 6.30736*thick )

# model1
length = 198.1
width  = 45.08
thick  = 5.56
(l1 = -55.52222 +  0.17843*length +5.61948*thick + 0.35395*width)


# model2
(l2 = -45.39605 +  0.19248*length + 6.30736*thick )
