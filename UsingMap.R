


library(tidyverse)
set.seed(71)

N <- 15
dat <- tibble(tag1 = sample(LETTERS[1:3], N, replace = TRUE),
              tag2 = sample(letters[1:5], N, replace = TRUE),
              y = rnorm(N),
              x = runif(N))
dat

dat %>% 
  group_by(tag1)

dat %>% 
  group_by(tag1) %>% 
  arrange(tag1) %>% 
  mutate(val = 1,val2 = cumsum(val))  


dat %>%
  arrange(tag1) %>% 
  group_by(tag1) %>% 
  nest() #%>% 
  unnest()
  summary()

my_list <- list(1:2, 3:5)
my_list
    
my_list %>% map(sum)  
  
  f = function(x){x + 1}
  
  my_list %>% map(f)  
  
  my_list %>% map(.,function(x){x + 1} %>% return)  

  
  
  dat_nest <- dat %>% 
    group_nest(tag1) 
    
  dat_nest
  
  dat_nest %>% mutate(x_mean = map(data, ~mean(.$x)))
  dat_nest %>% mutate(x_mean = map(datadata, function(x){mean(x$x)}))
  
  
  dat
  
  
  dat_nest %>% 
    mutate(x = map(data, ~filter(., tag2 != "b") %>% .$x),
           y = map(data, ~filter(., tag2 != "b") %>% .$y),
           ttest = map2(x, y, ~t.test(.x, .y)),
           pval = map_dbl(ttest, ~{ .$p.value }))

  dat_nest
  dat_nest$data
  
  
  dat_nest %>% 
    mutate(x = map(data, ~filter(., tag2 != "b") %>% .$x),
           y = map(data, ~filter(., tag2 != "b") %>% .$y)
           ) 
  
  
  x1 <- list(a = 1:6, b = 3:8)
  x1
  x2 <- list(a = 6:1, b = 10:15)
  x2
  map2(x1, x2, ~ .x + .y)
  
  
  dat = iris
  dat %>% head(3)
  dat %>% str()
  dat %>% select(Species) %>% unlist %>% factor %>% levels()
  
  
  extract_levels <- function(dat, .key){
    dat %>% select(.key) %>% unlist %>% factor %>% levels()
  }
  
  dat %>%
    extract_levels("Species") 
  
  
  dat %>%
    extract_levels("Species") %>% 
    map(~filterd_lm(dat, .))
  
  
  dat_lm = 
    dat %>% 
    group_by(Species) %>% 
    nest()

  dat_lm$Species #中身
  dat_lm[["Species"]]#中身
  dat_lm["Species"]#要約
  
  dat_lm$data  #中身
  dat_lm[["data"]]  #中身
  dat_lm["data"]  #要約
  
  
  dat_lm2 =
    dat_lm %>% mutate(model = map(data,function(x){lm(data = x, Petal.Length ~ Sepal.Width)}))
  
  dat_lm2.1 =
    dat_lm %>% mutate(model = map(data,~lm(data = ., Petal.Length ~ Sepal.Width)))
  
  dat_lm2
  
  dat_lm2.1
  
  summary(dat_lm2$model)
  dat_lm2$model %>% map(.,summary)
  
  dat_lm2 %>% 
    mutate(rsq = map_dbl(model,~summary(.) %>% .$r.squared)) %>% 
    select(Species,rsq) %>% unnest()
  
  
  
  
  
  
  
  
  
  
  
  
  