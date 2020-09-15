library(ggplot2) 
#install.packages("ggalt", dependencies = TRUE)
library(ggalt)   
library(dplyr)
library(foreign)
#install.packages("extrafont")

#library(extrafont)
#font_import()y

#file.choose()
dataset = read.spss("/home/emily/Downloads/W64_Mar20/ATP W64.sav", to.data.frame=TRUE)
blue <- "#0171CE"
red <- "#DE4433"
len <- nrow(dataset)
dataset = select(dataset,COVID_ACT_R_a_W64,COVID_ACT_R_b_W64,COVID_ACT_R_c_W64,COVID_ACT_R_d_W64,COVID_ACT_R_e_W64,F_PARTY_FINAL)

df <- data.frame(act=character(), total=double(), rep=double(), dem=double(), diff=double())

subset = dataset[dataset$COVID_ACT_R_a_W64 == 'Yes, have done this',]
total <- dim(subset)[1]/len
repdata = subset[subset$F_PARTY_FINAL == 'Republican',]
rep <- dim(repdata)[1]/len
demdata = subset[subset$F_PARTY_FINAL == 'Democrat',]
dem <- dim(demdata)[1]/len
de <- list(act="Attended religious services\n in person less often", total=total, rep=rep, dem=dem, diff=rep-dem)
df = rbind(df,de, stringsAsFactors=FALSE)

subset = dataset[dataset$COVID_ACT_R_b_W64 == 'Yes, have done this',]
total <- dim(subset)[1]/len
repdata = subset[subset$F_PARTY_FINAL == 'Republican',]
rep <- dim(repdata)[1]/len
demdata = subset[subset$F_PARTY_FINAL == 'Democrat',]
dem <- dim(demdata)[1]/len
de <- list(act="Watched religious services \nonline or on TV instead of \nattending in person", total=total, rep=rep, dem=dem, diff=rep-dem)
df = rbind(df,de, stringsAsFactors=FALSE)

subset = dataset[dataset$COVID_ACT_R_c_W64 == 'Yes, have done this',]
total <- dim(subset)[1]/len
repdata = subset[subset$F_PARTY_FINAL == 'Republican',]
rep <- dim(repdata)[1]/len
demdata = subset[subset$F_PARTY_FINAL == 'Democrat',]
dem <- dim(demdata)[1]/len
de <- list(act="Prayed for an end \nto the spread of the coronavirus", total=total, rep=rep, dem=dem, diff=rep-dem)
df = rbind(df,de, stringsAsFactors=FALSE)

subset = dataset[dataset$COVID_ACT_R_d_W64 == 'Yes, have done this',]
total <- dim(subset)[1]/len
repdata = subset[subset$F_PARTY_FINAL == 'Republican',]
rep <- dim(repdata)[1]/len
demdata = subset[subset$F_PARTY_FINAL == 'Democrat',]
dem <- dim(demdata)[1]/len
de <- list(act="Used a food delivery service \ninstead of going to a\n restaurant or grocery store", total=total, rep=rep, dem=dem, diff=rep-dem)
df = rbind(df,de, stringsAsFactors=FALSE)

subset = dataset[dataset$COVID_ACT_R_e_W64 == 'Yes, have done this',]
total <- dim(subset)[1]/len
repdata = subset[subset$F_PARTY_FINAL == 'Republican',]
rep <- dim(repdata)[1]/len
demdata = subset[subset$F_PARTY_FINAL == 'Democrat',]
dem <- dim(demdata)[1]/len
de <- list(act="Worked from home", total=total, rep=rep, dem=dem, diff=rep-dem)
df = rbind(df,de, stringsAsFactors=FALSE)

print(df)

gg <- ggplot() + geom_segment(data=df, aes(y=act, yend=act, x=0, xend=.3), color="#b2b2b2", size=0.01)
gg <- gg + geom_dumbbell(data=df, aes(y=act, x=rep, xend=dem), size=1.5, color="#b2b2b2", size_x=3, size_xend = 3, colour_x = red, colour_xend = blue)

# text below points
gg <- gg + geom_text(data=filter(df, act=="Worked from home"),
                     aes(x=rep, y=act, label="Republican"),
                     color=red, size=3, vjust=-2, fontface="bold", family="Times New Roman")
gg <- gg + geom_text(data=filter(df, act=="Worked from home"),
                     aes(x=dem, y=act, label="Democrat"),
                     color=blue, size=3, vjust=-2, fontface="bold", family="Times New Roman")
# text above points
gg <- gg + geom_text(data=df, aes(x=rep, y=act, label=percent_first(rep)),
                     color=red, size=2.75, vjust=2.5, family="Times New Roman")
gg <- gg + geom_text(data=df, color=blue, size=2.75, vjust=2.5, family="Times New Roman",
                     aes(x=dem, y=act, label=percent_first(dem)))
# difference column
#gg <- gg + geom_rect(data=df, aes(xmin=1.05, xmax=1.175, ymin=-Inf, ymax=Inf), fill="#efefe3")
gg <- gg + geom_text(data=df, aes(label=diff, y=act, x=1.1125), fontface="bold", size=3, family="Times New Roman")
gg <- gg + geom_text(data=filter(df, act=="Worked from home"), aes(x=1.1125, y=act, label="DIFF"),
                     color="#7a7d7e", size=3.1, vjust=-2, fontface="bold", family="Times New Roman")
gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(0, .25))
gg <- gg + scale_y_discrete(expand=c(0.075,0))
gg <- gg + labs(x=NULL, y=NULL, title="Coronavirus Activities",
                subtitle="Distinguished by political party",
                caption="Source: Pew Research Center")
gg <- gg + theme_bw(base_family="Times New Roman")
#gg <- gg + theme(panel.grid.major=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
print(gg)
print(df)