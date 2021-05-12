# Exploring hierarchy in the data

# visualization of order of data

# x and y , both are numeric
#1. barplot
#2. spider plot or radar
#3. word cloud
#4. parallel plot
#5. lollipop plot
#6. circular barplot

###############################################################################
#                               1. barplot                                  #
###############################################################################
# Required data manipulation skills

# actual path D:\RGC work\work in may\RGC\Rworks

setwd("D:/RGC work/work in may/RGC/Rworks") # to set working directory
getwd()  # to see which is our working directory

# reading data from CSV or excel file

#read.csv() function
df= read.csv("iris.csv")
head(df)
summary(df)
str(df)


#read.xlsx() function
df1= readxl::read_xlsx("iris.xlsx")
head(df1)
summary(df1)
str(df1)

# if your data is in specific sheet of excel workbook
df2= readxl::read_xlsx("iris.xlsx",sheet="iris")
head(df2)
summary(df2)
str(df2)

# for the function below we need tidyverse package
library(tidyverse)
# Selecting column

df_f4= select(df,-5)
head(df_f4)

# filtering rows
df_setosa= filter(df,Species=="setosa")
head(df_setosa)
dim(df_setosa)

df_virginica= filter(df,Species=="virginica")
head(df_virginica)
dim(df_virginica)

# gathering columns to make long table
df_long= gather(df,Mes_type,length,1:4)
head(df_long)

#grouping and summarizing data
# group by
df_sumzd=group_by(df_long,Species)
head(df_sumzd)

#Summarize
df_sumzd=group_by(df_long,Species,Mes_type) %>% summarise(mean=mean(length),sd=sd(length))
head(df_sumzd)


# Plot basic barplot
p=ggplot(df_sumzd,aes(x=Species,y=mean))+
   geom_bar()
p



# Plot basic barplot- effect of stat= "identity"
p=ggplot(df_sumzd,aes(x=Species,y=mean))+
   geom_bar(stat="identity")
p

# fill color

p=ggplot(df_sumzd,aes(x=Species,y=mean))+
   geom_bar(stat="identity",fill="red")
p

# fill color - mapping to a variable

p=ggplot(df_sumzd,aes(x=Species,y=mean, fill=Species))+
   geom_bar(stat="identity")
p

# fill colors- stacked bar plot
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity")
p

# fill colors- grouped bar plot 
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge")
p

#Error bars
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))


p

#Error bars width
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25)


p

#Error bars width, size,position
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9))


p

#Error bars width,size, position, alpha
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9),alpha=0.3)


p

#Error bars width,size, position, alpha and labels
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)


p

#Error bars width,size, position, alpha and labels
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   geom_text(position=position_dodge(0.9))


p

#Error bars width,size, position, alpha and labels, label position corrected
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   geom_text(position=position_dodge(0.9),vjust=-1)


p


#Error bars width,size, position, alpha and labels, label position more corrected
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   geom_text(position=position_dodge(0.9),vjust=-0.5, hjust=1.2)


p

#theme classic
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   geom_text(position=position_dodge(0.9),vjust=-0.5, hjust=1)+
   theme_classic()


p


#theme bw
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type,label=mean))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   geom_text(position=position_dodge(0.9),vjust=-0.5, hjust=1.1)+
   theme_bw()


p

# I don't like this plot.

p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
      theme_bw()


p

# coord flip, this to when x axis labels are very long and overlap each other
p=ggplot(df_sumzd,aes(x=Species,y=mean,fill=Mes_type))+
   geom_bar(stat="identity",position="dodge") +
   geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                 size=1,position= position_dodge(0.9), alpha=0.3)+
   theme_bw()+
   coord_flip()


p



#That's it for bar plot.