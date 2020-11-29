library(ggplot2) 
library(ggalt)   
library(dplyr)
library(tidyr)
library(foreign)
library(likert)

dataset = read.spss("/home/emily/Downloads/W54_Sep19/ATP W54.sav", to.data.frame=TRUE)
par(mfrow=c(1,2))

##Question A##
QA = select(dataset,INC_TIER2_W54,GOVRESP_a_W54)
##Getting data for low income answers
QA = QA[!QA$INC_TIER2_W54 == 'Refused',]
total = QA[QA$INC_TIER2_W54 == 'Lower income',]
LowNum = dim(total)[1]
LowNo = total[total$INC_TIER2_W54 == 'Lower income' & total$GOVRESP_a_W54 =='No, not the responsibility of the federal government to provide',]
LowYes = total[total$INC_TIER2_W54 == 'Lower income' & total$GOVRESP_a_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]

##Getting data for middle income answers
total = QA[QA$INC_TIER2_W54 == 'Middle income',]
MidNum = dim(total)[1]
MidNo = total[total$INC_TIER2_W54 == 'Middle income' & total$GOVRESP_a_W54 =='No, not the responsibility of the federal government to provide',]
MidYes = total[total$INC_TIER2_W54 == 'Middle income' & total$GOVRESP_a_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]

##Getting data for high income answers
total = QA[QA$INC_TIER2_W54 == 'Upper income',]
HighNum = dim(total)[1]
HighNo = total[total$INC_TIER2_W54 == 'Upper income' & total$GOVRESP_a_W54 =='No, not the responsibility of the federal government to provide',]
HighYes = total[total$INC_TIER2_W54 == 'Upper income' & total$GOVRESP_a_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]

dat <- data.frame(
  No = c(dim(LowNo)[1]/LowNum, dim(MidNo)[1]/MidNum, dim(HighNo)[1]/HighNum),
  Yes = c(dim(LowYes)[1]/LowNum, dim(MidYes)[1]/MidNum, dim(HighYes)[1]/HighNum),
  Tiers = c("Low Income", "Middle Income","Upper Income"))

dat_long <- dat %>%gather("Answer", "Percent", -Tiers)
group <-dat[, c("Tiers")]
gg <- ggplot(dat_long, aes(x = Tiers, y = Percent, fill = Answer)) +  geom_col(position = "dodge")
gg <- gg + geom_text(aes(label = scales::percent(Percent), 
                         y = Percent),
                     position = position_dodge(width = 0.9),
                     vjust = -.3)
gg <- gg + labs(x=NULL, y=NULL, title="Is it the responsibility of the federal government\nto provide ____ for all Americans?",
                subtitle="Adequate Standard of Living",
                caption="Source: Pew Research Center")
print(gg)

##Question B##
QA = select(dataset,INC_TIER2_W54,GOVRESP_b_W54)
QA = QA[!QA$INC_TIER2_W54 == 'Refused',]
##Getting data for low income answers
total = QA[QA$INC_TIER2_W54 == 'Lower income',]
LowNum = dim(total)[1]
LowNo = total[total$INC_TIER2_W54 == 'Lower income' & total$GOVRESP_b_W54 =='No, not the responsibility of the federal government to provide',]
LowYes = total[total$INC_TIER2_W54 == 'Lower income' & total$GOVRESP_b_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]

##Getting data for middle income answers
total = QA[QA$INC_TIER2_W54 == 'Middle income',]
MidNum = dim(total)[1]
MidNo = total[total$INC_TIER2_W54 == 'Middle income' & total$GOVRESP_b_W54 =='No, not the responsibility of the federal government to provide',]
MidYes = total[total$INC_TIER2_W54 == 'Middle income' & total$GOVRESP_b_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]

##Getting data for high income answers
total = QA[QA$INC_TIER2_W54 == 'Upper income',]
HighNum = dim(total)[1]
HighNo = total[total$INC_TIER2_W54 == 'Upper income' & total$GOVRESP_b_W54 =='No, not the responsibility of the federal government to provide',]
HighYes = total[total$INC_TIER2_W54 == 'Upper income' & total$GOVRESP_b_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]
HighRef = total[total$INC_TIER2_W54 == 'Refused',]

dat <- data.frame(
  No = c(dim(LowNo[1])[1]/LowNum, dim(MidNo[1])[1]/MidNum, dim(HighNo[1])[1]/HighNum),
  Yes = c(dim(LowYes[1])[1]/LowNum, dim(MidYes[1])[1]/MidNum, dim(HighYes[1])[1]/HighNum),
  Tiers = c("Low Income", "Middle Income","Upper Income")
)
dat_long <- dat %>%gather("Answer", "Percent", -Tiers)
gg <- ggplot(dat_long, aes(x = Tiers, y = Percent, fill = Answer)) +  geom_col(position = "dodge")
gg <- gg + geom_text(aes(label = scales::percent(Percent), 
                         y = Percent),
                     position = position_dodge(width = 0.9),
                     vjust = -.3)
gg <- gg + labs(x=NULL, y=NULL, title="Is it the responsibility of the federal government\nto provide ____ for all Americans?",
     subtitle="Adequate Housing",
     caption="Source: Pew Research Center")
print(gg)

##Question C##
QA = select(dataset,INC_TIER2_W54,GOVRESP_c_W54)
QA = QA[!QA$INC_TIER2_W54 == 'Refused',]
##Getting data for low income answers
total = QA[QA$INC_TIER2_W54 == 'Lower income',]
LowNum = dim(total)[1]
LowNo = total[total$INC_TIER2_W54 == 'Lower income' & total$GOVRESP_c_W54 =='No, not the responsibility of the federal government to provide',]
LowYes = total[total$INC_TIER2_W54 == 'Lower income' & total$GOVRESP_c_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]

##Getting data for middle income answers
total = QA[QA$INC_TIER2_W54 == 'Middle income',]
MidNum = dim(total)[1]
MidNo = total[total$INC_TIER2_W54 == 'Middle income' & total$GOVRESP_c_W54 =='No, not the responsibility of the federal government to provide',]
MidYes = total[total$INC_TIER2_W54 == 'Middle income' & total$GOVRESP_c_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]

##Getting data for high income answers
total = QA[QA$INC_TIER2_W54 == 'Upper income',]
HighNum = dim(total)[1]
HighNo = total[total$INC_TIER2_W54 == 'Upper income' & total$GOVRESP_c_W54 =='No, not the responsibility of the federal government to provide',]
HighYes = total[total$INC_TIER2_W54 == 'Upper income' & total$GOVRESP_c_W54 == 'Yes, a responsibility of the federal government to provide for all Americans',]
HighRef = total[total$INC_TIER2_W54 == 'Refused',]

dat <- data.frame(
  No = c(dim(LowNo[1])[1]/LowNum, dim(MidNo[1])[1]/MidNum, dim(HighNo[1])[1]/HighNum),
  Yes = c(dim(LowYes[1])[1]/LowNum, dim(MidYes[1])[1]/MidNum, dim(HighYes[1])[1]/HighNum),
  Tiers = c("Low Income", "Middle Income","Upper Income")
)
dat_long <- dat %>%gather("Answer", "Percent", -Tiers)
gg <- ggplot(dat_long, aes(x = Tiers, y = Percent, fill = Answer))
gg <- gg +  geom_col(position = "dodge")
gg <- gg + geom_text(aes(label = scales::percent(Percent), 
                                y = Percent),
                                position = position_dodge(width = 0.9),
                                vjust = -.3)
gg <- gg + labs(x=NULL, y=NULL, title="Is it the responsibility of the federal government\nto provide ____ for all Americans?",
                subtitle="Quality K-12 Education",
                caption="Source: Pew Research Center")
print(gg)