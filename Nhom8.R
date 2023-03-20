# cài đặt các package
install.packages('readxl')
install.packages('ggplot2') 
install.packages('naniar')
install.packages('tidyr')
install.packages('dplyr')
install.packages('ggplot')
install.packages('missMDA')
install.packages('FatorMineR')
install.packages('Amelia')
install.packages('tidyverse')
install.packages('colorspace')
install.packages('grid')
install.packages('norm')
install.packages('corrplot')
install.packages("GPArotation")
install.packages("corpcor")
install.packages("likert")
install.packages("factoextra")
install.packages("jbryer")
install.packages('visdat')
install.packages("plotly")
install.packages("gganimate")
install.packages("gapminder")
install.packages("gifski")
install.packages("png")
install.packages("lubridate")
library(lubridate)
library(png)
library(gifski)
library("plotly")
library(gganimate)
library(gapminder)
library(corrplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(VIM)
library(visdat)
library(missMDA)
library(FactoMineR)
library(naniar)
library(tidyverse)
library(norm)
library(devtools)
library(factoextra)
library(psych)
library(GPArotation)
library(corpcor)
library(factoextra)
library(likert)
setwd("E:/R/animate")
getwd()


# Mở và đọc file

dataset <- read.csv("E:/R/final.csv", header = TRUE, sep =",")
dataset

# Kiểm tra dữ liệu khuyết thiếu

anyNA(dataset)  #kiểm tra có/không
sum(is.na(dataset)) #kiểm tra số lượng
colSums(is.na(dataset)) #kiểm tra thiếu ở từng biến

##Xem dưới dạng đồ thị = pack visdat
vis_miss(dataset)
aggr(dataset, col = c('#7F152E', '#EDAE01'))
gg_miss_var(dataset, show_pct = TRUE)

## Hiệu chỉnh dữ liệu
dataset$Alcohol_use[dataset$Alcohol_use == "<0.1"] = 0
dataset$UWMortal[dataset$UWMortal== "<0.1"] = 0
dataset$PWE.25[dataset$PWE.25 == "<0.1"] = 0
dataset$Malaria[dataset$Malaria == "<0.1"] =0
dataset$HIV[dataset$HIV == "<0.01"] =0

#chuyển biến Class về dạng numeric
dataset$Class[dataset$Class == "High income"] = 4
dataset$Class[dataset$Class == "Upper middle income"] = 3
dataset$Class[dataset$Class == "Lower middle income"] = 2
dataset$Class[dataset$Class == "Low income"] = 1



## Xử lý khuyết thiếu
pre_param <- prelim.norm(as.matrix(dataset))
thetahat <- em.norm(pre_param)
estimate <- getparam.norm(pre_param, thetahat)
estimate$mu
estimate$sigma
rngseed(1e5)
dataset1 <- imp.norm(pre_param, thetahat, dataset)
dataset1
anyNA(dataset1)
#chuyển các biến về numeric
dataset1 <- edit(dataset1)
# THống kê mô tả

summary(dataset1) # hình dung chung

## Kiểm tra tương quan giữa các biến

  # ma trận tương quan
  datacorr <- dataset1[, c(2:27)]
  datacorr <- datacorr[-2] 
  
  #biểu đồ tương quan
  cor <-cor(datacorr, use = "everything")
  head(round(cor,3))
  corrplot(cor, method="circle")
##Impact of COVID-19 on population health
  
#Biểu đồ về nước có % dân số có tỉ lệ nhiễm và/hoặc tỉ lệ chết cao 
dataset1$SumC <-  abs(dataset1$SumC)
dataset1$SumD <-  abs(dataset1$SumD)
y <-  dataset1 %>%
  filter(SumC > 2776769 )

y <- y$SumC
labels <- dataset1$ï..MemberState 
pie(y, labels, main = "biểu đồ",clockwise = TRUE)

##Biểu đồ về tỉ lệ nước có tỉ lệ nhiễm cao thuộc nhóm nào (biểu đồ tròn 2 biến dân số chết + dân số nhiễm và biến country type)
## Phân loại theo InC
dataset1 %>% 
  ggplot(aes(Class,SumC))+
  geom_col() +
  labs(title="Số dân nhiễm phân loại theo InCome")
## Phân loại theo khu vực
dataset1 %>% 
  ggplot(aes(Rg,SumC))+
  geom_col() +
  labs(title="Số dân nhiễm phân loại theo khu vực")

## Đâu là khu vực được cho là Healthy

lic <- filter(dataset1,Class == 1)
lmi <- filter(dataset1,Class == 2) 
umi <- filter(dataset1,Class == 3) 
hic <- filter(dataset1,Class == 4)
lic1 <- lic$Age_exp
lmi1 <- lmi$Age_exp
umi1 <- umi$Age_exp
hic1 <- hic$Age_exp
tot <- list("low" = lic1, "medium" = lmi1, "high" = umi1, "extra"= hic1)
tot
stripchart(tot,
           main="",
           xlab="Life expect",
           ylab="Group",
           method="jitter",
           col=c("navy blue"),
           pch= 16)

## Tỉ lệ mắc lao/sốt rét/ HIV trên thế giới
  #cho HIV

a <- group_by(dataset1, Rg)
a
a <-  dataset1 %>%
      group_by(Rg) %>%
      summarise(sum(abs(HIV)))
a <- data.frame(a)
ggplot(a, aes(x= Rg, y= sum.abs.HIV..)) + geom_bar(stat = "identity")

  #cho TUBER
b <-  dataset1 %>%
  group_by(Rg) %>%
  summarise(sum(abs(Tuberculosis)))
b <- data.frame(b)
ggplot(a, aes(x= Rg, y= sum.abs.Tuberculosis..)) + geom_bar(stat = "identity")

  #cho MALAR
c <-  dataset1 %>%
  group_by(Rg) %>%
  summarise(sum(abs(Malaria))) 
c <- data.frame(c)
ggplot(c, aes(x= Rg, y= sum.abs.Malaria..)) + geom_bar(stat = "identity")

##Đồ thị xem sự ảnh hưởng của UHC tới HALE

dt <- read.csv("E:/R/HALEbyUHC.csv", header = TRUE, sep = ",")
install.packages("ggplot2")
dataset
library(ggplot2)

dt$Year <- as.Date(dt$Year, origin)



p <- ggplot(dt, aes(UHC, HALE, size = HALE, colour = ï..Location)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~ Rg) +
  theme(plot.title = element_text(size= 12, face ="bold", colour = "black"),
        axis.title.x = element_text(size=10, face ="bold", colour = "black"),
        axis.title.y = element_text(size=10, face ="bold", colour = "black"),
        axis.text.x = element_text(size=8, face ="bold", colour = "black"),
        axis.text.y = element_text(size=8, face ="bold", colour = "black"),
        strip.text.x = element_text(size=8, face ="bold", colour = "black"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  )+
  labs(title = 'Year: {frame_time}', x= 'UHC', y = 'HALE') +
  transition_time(Year) + 
  ease_aes('linear')
animate(p)
animate(p, duration = 4, fps= 20,width = 500, height = 500, renderer = gifski_renderer())

## Phân tích EFA 
  ##phân tích tương quan giữa các biến
  datacorr$PWE.10 <- abs(datacorr$PWE.10)
  datacorr$PWE.25 <- abs(datacorr$PWE.25)
  datacorr$SuicideMortal <- abs(datacorr$SuicideMortal)
  co <- cor(datacorr)
  anyNA(datacorr)
  datacorr
  
  ##Kiểm định bartlelet  n KMO
  
  
  install.packages("psych")
  library(psych)
  
  round(co, 5)
  cortest.bartlett(co)
  KMO(datacorr)
  
  ##Xác định nhân tố chính và minh họa
  
  pca <- prcomp(co, scale = TRUE)
  pca
  eig.va <- get_eigenvalue(co)
  eigen(co)
  ##đồ thị
  fviz_screeplot(pca, n = 25, choice = "eigenvalue", addlabels = TRUE)
  pc2 <- principal(co, nfactors = 3, rotate = "varimax")
  print.psych(pc2, cut = 0.1, sort = TRUE)

  
  ## Kiểm định chron back alpha cho từng nhân tố
  psych::alpha(datacorr[, c(08, 09,17,01, 06)])
  psych::alpha(datacorr[, c(03, 07,04,02)])
  psych::alpha(datacorr[, c(18, 19, 11, 22)], check.keys = TRUE)
