library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
set.seed(123)

my36colors <-c( '#E5D2DD', '#53A85F', '#F1BB72', '#F3B1A0', '#D6E7A3', '#57C3F3',
                '#476D87', '#E95C59', '#E59CC4', '#AB3282', '#23452F', '#BD956A', 
                '#8C549C', '#585658', '#9FA3A8', '#E0D4CA', '#5F3D69', '#C5DEBA',
                '#58A4C3', '#E4C755', '#F7F398', '#AA9A59', '#E63863', '#E39A35',
                '#C1E6F3', '#6778AE', '#91D0BE', '#B53E2B', '#712820', '#DCC1DD',
                '#CCE0F5', '#CCC9E6', '#625D9E', '#68A180', '#3A6963', '#968175')

#read data 
data1 <- read.csv('online_survey_data.csv')
data2 <- data1[,1:152]


##Baseline statistics
#1 Distribution map of different regions
data2$Permanent_Address <- factor(data2$Permanent_Address)
plot1=ggplot(data2, aes(x = Permanent_Address, fill = Permanent_Address)) + 
  geom_histogram(stat = 'count')+scale_fill_manual(values = my36colors)+theme_classic()
plot1

#2 Distribution map of gender
data2$Gender <- factor(as.character(data2$Gender))
temp2 <- data.frame(y=as.vector(table(data2$Gender)))
temp2$group <- names(table(data2$Gender))

plot2=ggplot(temp2, aes(x = 3, y = y, fill = group))+geom_col()+coord_polar(theta = "y")+
  xlim(c(0.2, 4))+geom_text(aes(label = y),position = position_stack(vjust = 0.5))+theme_void()
plot2

#3 Distribution map of age
data2$Age <- factor(as.character(data2$Age))
temp3 <- data.frame(y=round(as.vector(table(data2$Age))/length(data2$Age)*100,2))
temp3$group <- paste(names(table(data2$Age)),as.vector(table(data2$Age)),sep = ':')

plot3=ggplot(temp3, aes(x = 3, y = y, fill = group))+geom_col()+coord_polar(theta = "y")+
  xlim(c(0.2, 4))+geom_text(aes(label = y),position = position_stack(vjust = 0.5))+
  theme_void()+scale_fill_manual(values = my36colors)
plot3

#4 Distribution map of vaccine
data2$Vaccination <- factor(as.character(data2$Vaccination))
plot4=ggplot(data2, aes(x = Vaccination, fill = Vaccination)) + 
  geom_histogram(stat = 'count')+scale_fill_manual(values = my36colors)+theme_classic()
plot4

#5 Distribution map of infection status
data2$Positive_or_not <- factor(as.character(data2$Positive_or_not))
temp5 <- data.frame(y=round(as.vector(table(data2$Positive_or_not))/length(data2$Positive_or_not)*100,2))
temp5$group <- paste(names(table(data2$Positive_or_not)),as.vector(table(data2$Positive_or_not)),sep = ':')

plot5=ggplot(temp5, aes(x = 3, y = y, fill = group))+geom_col()+coord_polar(theta = "y")+
  xlim(c(0.2, 4))+geom_text(aes(label = y),position = position_stack(vjust = 0.5))+
  theme_void()+scale_fill_manual(values = my36colors)
plot5

#6、Basic disease distribution
data_percent6 = data.frame()
for (i in colnames(data2)[45:57]){
  print (i)
  prop=round(table(data2[i])[2]/nrow(data2)*100,2)
  count=table(data2[i])[2]
  data_percent6=rbind(data_percent6,data.frame(proportion=prop,Number=count,disease=i))
}

data_percent6 <- data_percent6[order(data_percent6$proportion),]
data_percent6$disease <- factor(data_percent6$disease,levels = data_percent6$disease)
plot6=ggplot(data_percent6, aes(x = disease, y=proportion))+
  coord_flip()+geom_col(color='gray',fill='#5EB5D7')+theme_classic()+
  geom_text(data=data_percent6, aes(label=proportion),col='black',alpha = 1,size=2.5)
plot6

#7、Distribution map of smoke
data2$Smoking_Status_ <- factor(as.character(data2$Smoking_Status_))
temp7 <- data.frame(y=round(as.vector(table(data2$Smoking_Status_))/length(data2$Smoking_Status_)*100,2))
temp7$group <- paste(names(table(data2$Smoking_Status_)),as.vector(table(data2$Smoking_Status_)),sep = ':')

plot7=ggplot(data2, aes(x = Smoking_Status_, fill = Smoking_Status_)) + ggtitle(paste0(temp7$group,collapse = ' '))+
  geom_histogram(stat = 'count')+scale_fill_manual(values = my36colors)+theme_classic()
plot7

#8、Distribution map of drink
data2$Whether_to_drink <- factor(as.character(data2$Whether_to_drink))
temp8 <- data.frame(y=round(as.vector(table(data2$Whether_to_drink))/length(data2$Whether_to_drink)*100,2))
temp8$group <- paste(names(table(data2$Whether_to_drink)),as.vector(table(data2$Whether_to_drink)),sep = ':')

plot8=ggplot(temp8, aes(x = 3, y = y, fill = group))+geom_col()+coord_polar(theta = "y")+xlim(c(0.2, 4))
  geom_text(aes(label = y),position = position_stack(vjust = 0.5))+theme_void()+scale_fill_manual(values = my36colors)
plot8

#9、Date distribution map of sars-cov-2 positive
temp <- subset(data2,Time_of_infection != -3)
temp$Time_of_infection <- factor(as.character(temp$Time_of_infection))
temp9 <- data.frame(y=round(as.vector(table(temp$Time_of_infection))/length(temp$Time_of_infection)*100,2))
temp9$group <- paste(names(table(temp$Time_of_infection)),as.vector(table(temp$Time_of_infection)),sep = ':')

plot9=ggplot(temp9, aes(x = 3, y = y, fill = group))+geom_col()+coord_polar(theta = "y")+
  xlim(c(0.2, 4))+geom_text(aes(label = y),position = position_stack(vjust = 0.5))+
  theme_void()+scale_fill_manual(values = my36colors)

plot9

#10 distribution map of sars-cov-2 positive symptom
data_percent10 = data.frame()
temp <- subset(data2,Positive_or_not==2)
for (i in colnames(temp)[19:41]){
  print (i)
  prop=round(table(temp[i])[2]/nrow(temp)*100,2)
  count=table(temp[i])[2]
  data_percent10=rbind(data_percent10,data.frame(proportion=prop,Number=count,disease=i))
}

data_percent10 <- data_percent10[order(data_percent10$proportion),]
data_percent10$disease <- factor(data_percent10$disease,levels = data_percent10$disease)
plot10=ggplot(data_percent10, aes(x = disease, y=proportion))+coord_flip()+geom_col(color='gray',fill='#5EB5D7')+
  geom_text(data=data_percent10, aes(label=proportion),col='black',alpha = 1,size=2.5)+theme_classic()
plot10

#11 distribution map of Symptom improvement time
temp <- subset(data2,Positive_or_not==2)
temp$Symptom_improvement_time <- factor(as.character(temp$Symptom_improvement_time))
temp11 <- data.frame(y=round(as.vector(table(temp$Symptom_improvement_time))/length(temp$Symptom_improvement_time)*100,2))
temp11$group <- paste(names(table(temp$Symptom_improvement_time)),as.vector(table(temp$Symptom_improvement_time)),sep = ':')

plot11=ggplot(temp11, aes(x = 3, y = y, fill = group))+geom_col()+coord_polar(theta = "y")+xlim(c(0.2, 4))+
  geom_text(aes(label = y),position = position_stack(vjust = 0.5))+theme_void()+scale_fill_manual(values = my36colors)
plot11

#12 distribution map of disease_severity
temp <- subset(data2,Positive_or_not==2 & disease_severity != 6)
temp$disease_severity <- gsub('1|2','0',temp$disease_severity)
temp$disease_severity <- gsub('3|4|5','1',temp$disease_severity)
temp$disease_severity <- factor(as.character(temp$disease_severity))
temp12 <- data.frame(y=round(as.vector(table(temp$disease_severity))/length(temp$disease_severity)*100,2))
temp12$group <- paste(names(table(temp$disease_severity)),as.vector(table(temp$disease_severity)),sep = ':')

plot12=ggplot(temp12, aes(x = 3, y = y, fill = group))+geom_col()+coord_polar(theta = "y")+xlim(c(0.2, 4))+
  geom_text(aes(label = y),position = position_stack(vjust = 0.5))+theme_void()+scale_fill_manual(values = my36colors)
plot12

#13 distribution map of Medium to long-term symptoms
data_percent13 = data.frame()
temp <- subset(data2,Positive_or_not==2)
for (i in colnames(temp)[61:83]){
  print (i)
  prop=round(table(temp[i])[2]/nrow(temp)*100,1)
  count=table(temp[i])[2]
  data_percent13=rbind(data_percent13,data.frame(proportion=prop,Number=count,disease=i,month='month1'))
}

for (i in colnames(temp)[84:106]){
  print (i)
  prop=round(table(temp[i])[2]/nrow(temp)*100,1)
  count=table(temp[i])[2]
  data_percent13=rbind(data_percent13,data.frame(proportion=prop,Number=count,disease=i,month='month2'))
}

for (i in colnames(temp)[107:129]){
  print (i)
  prop=round(table(temp[i])[2]/nrow(temp)*100,1)
  count=table(temp[i])[2]
  data_percent13=rbind(data_percent13,data.frame(proportion=prop,Number=count,disease=i,month='month3'))
}

plot13=ggplot(data_percent13,aes(disease, y=proportion,fill=month))+geom_bar(stat = 'identity', position = 'dodge')+
  geom_text(data=data_percent13, aes(label=proportion,fill=month),col='black',alpha = 1,size=1.5,vjust = 0.5, position = position_dodge(0.9))+
  theme_classic()+scale_fill_manual(values = my36colors) +theme(axis.text.x = element_text(angle = 45,hjust=1))
plot13

##logistic regression analysis
neg <- subset(data2,Positive_or_not==1)
pos <- subset(data2,Positive_or_not==2)

#14 The impact of non vaccine and vaccine dosage groups on symptoms, adjusted for baseline characteristics.
pos$Recent_vaccination_time[pos$Recent_vaccination_time== -3] <- 5 #Change the assignment to set 3 months as the reference variable
adjust_list1 <- colnames(data2)[c(7:9,16,59:60,45:57)]
symptom_list <- colnames(data2)[c(19:39)]
pos_sub <- pos[,c(adjust_list1,symptom_list,'Vaccination')]
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Vaccination',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp2 <- temp2[grep('Recent_vaccination_time5',rownames(temp2),invert = T),]
  temp3 <- round(cbind(temp2,temp_summary$coefficients),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('Vacc',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)

temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.8,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.8,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.8,y=80)
p2

p3=ggarrange(p1,p2,widths = c(1,4))
p3

temp <- subset(temp,pvalue<0.05)

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2

p3=ggarrange(p1,p2,widths = c(1.5,4))
p3

#15 The impact of vaccination date on symptoms, adjusted for baseline characteristics.
adjust_list1 <- colnames(data2)[c(7:9,10,59:60,45:57)]
symptom_list <- colnames(data2)[c(19:39)]
pos_sub <- pos[,c(adjust_list1,symptom_list,'Recent_vaccination_time')]
pos_sub <- subset(pos_sub,Recent_vaccination_time !=5)
pos_sub<- na.omit(pos_sub)
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Recent_vaccination_time',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp3 <- round(cbind(temp2,temp_summary$coefficients),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('Latest',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)


temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.8,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.8,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.8,y=80)
p2

p3=ggarrange(p1,p2,widths = c(1,4))
p3

temp <- subset(temp,pvalue<0.05)

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2

p3=ggarrange(p1,p2,widths = c(1.5,4))
p3

#16、The impact of non vaccine and vaccine combinations on symptoms, adjusted for baseline characteristics.
a=pos
a$Vaccine_backgrounds[a$Vaccination == 0 ] = "0"
a$Vaccine_backgrounds[a$Vaccination == 1 & a$First_dose_of_vaccine ==  1] = "1" #1 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 2 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1 ] = "2" #2 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 2 & a$First_dose_of_vaccine ==  2 & a$Second_dose_of_vaccine ==  2 ] = "3" #2 needle adenovirus
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1] = "4" #3 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  3 & a$Second_dose_of_vaccine ==  3  & a$Third_dose_vaccine ==  3] = "5" #3 needle protein
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  3] = "6" #2 needle inactivation + 1 protein
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==1] = "7" #4 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==3] = "8" #3 needle inactivation + 1 protein
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==2] = "9" #3 needle inactivation + 1 needle adenovirus

adjust_list1 <- colnames(data2)[c(7:9,16,59:60,45:57)]
symptom_list <- colnames(data2)[c(19:39)]
pos_sub <- a[,c(adjust_list1,symptom_list,'Vaccine_backgrounds')]
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Vaccine_backgrounds',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp2 <- temp2[grep('Recent_vaccination_time5',rownames(temp2),invert = T),]
  temp3 <- round(cbind(temp2,temp_summary$coefficients),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('background',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)

temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

temp <- subset(temp,pvalue <0.25)
color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2
p3 = ggarrange(p1,p2,widths = c(1.5,4))

#17、The impact of non vaccine and vaccine combinations on symptoms at 1 month, adjusted for baseline characteristics.
adjust_list1 <- colnames(data2)[c(7:9,16,59:60,45:57)]
symptom_list <- colnames(data2)[c(61:83)]
pos_sub <- a[,c(adjust_list1,symptom_list,'Vaccine_backgrounds')]
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Vaccine_backgrounds',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)$coefficients
  temp_summary  <- temp_summary[grep('Recent_vaccination_time5',rownames(temp_summary ),invert = T),]
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp2 <- temp2[grep('Recent_vaccination_time5',rownames(temp2),invert = T),]
  temp3 <- round(cbind(temp2,temp_summary),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('background',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)

temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

temp <- subset(temp,pvalue <0.05)
color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2

p3=ggarrange(p1,p2,widths = c(1.5,4))
p3

#18 The impact of non vaccine and vaccine combinations on symptoms at 2 months, adjusted for baseline characteristics.
adjust_list1 <- colnames(data2)[c(7:9,16,59:60,45:57)]
symptom_list <- colnames(data2)[c(84:106)]
pos_sub <- a[,c(adjust_list1,symptom_list,'Vaccine_backgrounds')]
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Vaccine_backgrounds',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)$coefficients
  temp_summary  <- temp_summary[grep('Recent_vaccination_time5',rownames(temp_summary ),invert = T),]
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp2 <- temp2[grep('Recent_vaccination_time5',rownames(temp2),invert = T),]
  temp3 <- round(cbind(temp2,temp_summary),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('background',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)

temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

temp <- subset(temp,pvalue <0.05)
color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2

p3=ggarrange(p1,p2,widths = c(1.5,4))
p3

#19 The impact of non vaccine and vaccine combinations on symptoms at 3 months, adjusted for baseline characteristics.
adjust_list1 <- colnames(data2)[c(7:9,59:60,45:57)]
symptom_list <- colnames(data2)[c(107:129)]
pos_sub <- a[,c(adjust_list1,symptom_list,'Vaccine_backgrounds')]
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Vaccine_backgrounds',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)$coefficients
  temp_summary  <- temp_summary[grep('Recent_vaccination_time5',rownames(temp_summary ),invert = T),]
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp2 <- temp2[grep('Recent_vaccination_time5',rownames(temp2),invert = T),]
  temp3 <- round(cbind(temp2,temp_summary),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('background',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)

temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

temp <- subset(temp,pvalue <0.05)

color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2

p3=ggarrange(p1,p2,widths = c(1.5,4))
p3

#20 The impact of vaccination date on symptoms at 1 month, adjusted for baseline characteristics.
a=subset(pos,Recent_vaccination_time !=5)
adjust_list1 <- colnames(data2)[c(7:9,10,59:60,45:57)]
symptom_list <- colnames(data2)[c(61:81)]
pos_sub <- a[,c(adjust_list1,symptom_list,'Recent_vaccination_time')]
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Recent_vaccination_time',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)$coefficients
  temp_summary  <- temp_summary[grep('Recent_vaccination_time5',rownames(temp_summary ),invert = T),]
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp2 <- temp2[grep('Recent_vaccination_time5',rownames(temp2),invert = T),]
  temp3 <- round(cbind(temp2,temp_summary),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('Latest_',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)

temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

temp <- subset(temp,pvalue <0.05)
color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2

#21 The impact of vaccination date on symptoms at 2 months, adjusted for baseline characteristics.
a=subset(pos,Recent_vaccination_time !=5)
adjust_list1 <- colnames(data2)[c(7:9,10,59:60,45:57)]
symptom_list <- colnames(data2)[c(84:104)]
pos_sub <- a[,c(adjust_list1,symptom_list,'Recent_vaccination_time')]
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Recent_vaccination_time',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)$coefficients
  temp_summary  <- temp_summary[grep('Recent_vaccination_time5',rownames(temp_summary ),invert = T),]
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp2 <- temp2[grep('Recent_vaccination_time5',rownames(temp2),invert = T),]
  temp3 <- round(cbind(temp2,temp_summary),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('Latest_va',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)

temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

temp <- subset(temp,pvalue <0.05)
color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2

#22 The impact of vaccination date on symptoms at 3 months, adjusted for baseline characteristics.
a=subset(pos,Recent_vaccination_time !=5)
adjust_list1 <- colnames(data2)[c(7:9,10,59:60,45:57)]
symptom_list <- colnames(data2)[c(107:127)]
pos_sub <- a[,c(adjust_list1,symptom_list,'Recent_vaccination_time')]
pos_sub[,] <- lapply(pos_sub[,],factor)

data.frame1 <- data.frame()
for (j in symptom_list){
  print(j)
  temp1 <- glm(as.formula(paste0(j, "~",paste0(c('Recent_vaccination_time',adjust_list1),collapse = '+'))), data=pos_sub, family = "binomial")
  temp_summary <- summary(temp1)$coefficients
  temp_summary  <- temp_summary[grep('Recent_vaccination_time5',rownames(temp_summary ),invert = T),]
  temp2 <- exp(cbind(OR = coef(temp1), confint(temp1)))
  temp2 <- temp2[grep('Recent_vaccination_time5',rownames(temp2),invert = T),]
  temp3 <- round(cbind(temp2,temp_summary),4)
  temp3 <- cbind(temp3,symptom=j)
  data.frame1 <- rbind(data.frame1,temp3)
}

#forest graph
temp <- data.frame1[grep('Latest_',rownames(data.frame1)),]
temp$dose <- rownames(temp)
temp$dose <- factor(temp$dose,levels = temp$dose)
colnames(temp)[c(2:3,7)] <- c('CI25','CI95','pvalue')
pos_sub[,] <- lapply(pos_sub[,],factor)
temp[,1:7] <- lapply(temp[,1:7],as.numeric)

temp$OR <- round(temp$OR,3)
temp$pvalue <- round(temp$pvalue,3)
temp$CI25 <- round(temp$CI25,3)
temp$CI95 <- round(temp$CI95,3)

temp$CI_OR <- paste0(temp$OR,':',temp$CI25,'~',temp$CI95)
temp$logCI25 <- log2(temp$CI25+1)
temp$logCI95 <- log2(temp$CI95+1)
temp$logOR <- log2(temp$OR+1)

temp <- subset(temp,pvalue <0.05)
color=as.vector(ifelse(temp$OR>1,"red","blue"))

p2 <- ggplot(temp,aes(x=dose,y=OR))+coord_flip()+geom_errorbar(aes(ymin=CI25, ymax=CI95), width=.2,size=0.5,color=color)
p2 <- p2+geom_point(size=1,color=color)+geom_hline(aes(yintercept=1), colour="black", linetype="dashed",size=0.5)
p2 <- p2+theme_classic()+scale_y_continuous(limits=c(0.0, 100))+labs(x="vaccation",y="OR")
p2 <- p2+theme(title=element_text(size=12,face="bold"),axis.text.x=element_text(size=9,color = "black"),axis.text.y=element_text(size=9,color = "black"))
p2 <- p2+ geom_text(data=temp, aes(label=CI_OR),col='black',alpha = 1,size=2.5,y=15)
p2 <- p2+ geom_text(data=temp, aes(label=pvalue),col='black',alpha = 1,size=2.5,y=30)
p2 <- p2+ geom_text(data=temp, aes(label=symptom),col='black',alpha = 1,size=2.5,y=80)
p2

#23 The impact of underlying diseases on the severity of positive participants.
pos<- subset(data2,Positive_or_not==2)
pos <- subset(pos,disease_severity != 6)
pos$disease_severity <- gsub('1|2','0',pos$disease_severity)
pos$disease_severity <- gsub('3|4|5','1',pos$disease_severity)
base_disease_list <- colnames(pos)[c(45:57)]

data_temp1 <- data.frame()
for (i in base_disease_list){
  print(i)
  temp1 <- table(pos[,i],pos$disease_severity)
  temp2 <- as.data.frame(unclass(temp1))
  rownames(temp2) <- c('F','T')
  mild_per <- round(as.numeric(temp2[1,]/sum(temp2[1,]))*100,2)
  severe_per <-  round(as.numeric(temp2[2,]/sum(temp2[2,]))*100,2)
  temp <- t(data.frame(mild=mild_per,severe=severe_per))
  rownames(temp) <- c('F','T')
  temp3 <- cbind(temp2,temp)
  temp3$base_disease <- rep(i,2)
  temp_pvalue <- chisq.test(temp1)
  temp3$Annova=rep(temp_pvalue$p.value,2)
  colnames(temp3)[3:4] <- paste0('A',0:1)
  data_temp1 <- rbind(data_temp1,temp3)
}

data_temp1$mild_illness <- paste0(data_temp1$`0`,' (',data_temp1$A0,'%',')')
data_temp1$Severe_illness <- paste0(data_temp1$`1`,' (',data_temp1$A1,'%',')')
write.csv(data_temp1,'Chi-square test for severity of different underlying diseases.csv')

#24 The impact of improvement time on different underlying diseases
pos<- subset(data2,Positive_or_not==2)
base_disease_list <- colnames(pos)[c(45:57)]

data_temp1 <- data.frame()
for (i in base_disease_list){
  print(i)
  temp1 <- table(pos[,i],pos$Symptom_improvement_time)
  temp2 <- as.data.frame(unclass(temp1))
  rownames(temp2) <- c('F','T')
  mild_per <- round(as.numeric(temp2[1,]/sum(temp2[1,]))*100,2)
  severe_per <-  round(as.numeric(temp2[2,]/sum(temp2[2,]))*100,2)
  temp <- t(data.frame(mild=mild_per,severe=severe_per))
  rownames(temp) <- c('F','T')
  temp3 <- cbind(temp2,temp)
  temp3$base_disease <- rep(i,2)
  temp_pvalue <- chisq.test(temp1)
  temp3$Annova=rep(temp_pvalue$p.value,2)
  colnames(temp3)[7:12] <- paste0('A',1:6)
  data_temp1 <- rbind(data_temp1,temp3)
}

data_temp1$B1 <- paste0(data_temp1$`1`,' (',data_temp1$A1,'%',')')
data_temp1$B2 <- paste0(data_temp1$`2`,' (',data_temp1$A2,'%',')')
data_temp1$B3 <- paste0(data_temp1$`3`,' (',data_temp1$A3,'%',')')
data_temp1$B4 <- paste0(data_temp1$`4`,' (',data_temp1$A4,'%',')')
data_temp1$B5 <- paste0(data_temp1$`5`,' (',data_temp1$A5,'%',')')
data_temp1$B6 <- paste0(data_temp1$`6`,' (',data_temp1$A6,'%',')')

write.csv(data_temp1,'Chi-square test for improvement time of different underlying diseases.csv')

##25 Differences in baseline characteristics between positive and negative participants
base_list1 <- colnames(data2)[c(7:10,16,59:60,45:57)]
data_sub <- data2[,base_list1]
data_temp <- data.frame()

for (i in base_list1){
  print(i)
  temp1 <- chisq.test(table(data2$Positive_or_not,data2[,i]))
  temp2 <- data.frame(squared=temp1$statistic,pvalue=temp1$p.value,df=temp1$parameter,jixian=i,symptom='Positive_or_not')
  data_temp <- rbind(data_temp,temp2)
}
write.csv(data_temp,'Baseline feature frequency.csv')

data_temp1 <- data.frame()
for (i in base_list1){
  print(i)
  temp1 <- table(data2[,i],data2$Positive_or_not)
  temp2 <- as.data.frame(unclass(temp1))
  temp2$jixian <- rep(i,nrow(temp2))
  temp2$group <- rownames(temp2)
  data_temp1 <- rbind(data_temp1,temp2)
}

data_temp1$group1 <- round(data_temp1$`1`/table(data2$Positive_or_not)[1],4)*100
data_temp1$group2 <- round(data_temp1$`2`/table(data2$Positive_or_not)[2],4)*100
data_temp1$group3 <- round(data_temp1$`3`/table(data2$Positive_or_not)[3],4)*100

data_temp1$percent1 <- paste0(data_temp1$`1`,'(',data_temp1$group1,'%',')')
data_temp1$percent2 <- paste0(data_temp1$`2`,'(',data_temp1$group2,'%',')')
data_temp1$percent3 <- paste0(data_temp1$`3`,'(',data_temp1$group3,'%',')')

write.csv(data_temp1,'Baseline feature count.csv')

##26 Family similarity of positive participants
pos<- subset(data2,Positive_or_not==2)
pos_sub=subset(pos,Family_similarity != -2)
pos_sub$Family_similarity <- factor(pos_sub$Family_similarity)

temp <- data.frame(y=round(as.vector(table(pos_sub$Family_similarity))/length(pos_sub$Family_similarity)*100,2))
temp$group <- paste(names(table(pos_sub$Family_similarity)),as.vector(table(pos_sub$Family_similarity)),sep = ':')

temp$count <- as.numeric(gsub('.+:','',temp$group))
temp$time <- gsub(':.+','',temp$group)

plot26=ggbarplot(temp,x = 'time',y = 'count',fill = 'time',palette = my36colors[10:30],label = T)
plot26

#27 Distribution of improvement time among participants with different positive vaccine doses
pos_sub <- pos
pos_sub$Vaccination <- factor(pos_sub$Vaccination)
pos_sub$Symptom_improvement_time <- factor(pos_sub$Symptom_improvement_time)
pvalue1=chisq.test(table(pos_sub$Symptom_improvement_time,pos_sub$Vaccination))

plot27=ggplot(pos_sub, aes(Vaccination)) + labs(y='percent',x='Vaccination times')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=Symptom_improvement_time), position="fill")+scale_fill_manual(values = my36colors)+theme_bw()
plot27

#28 Distribution of improvement time among positive participants with different vaccination dates
pos_sub <- subset(pos,Recent_vaccination_time != (-3))
pos_sub$Recent_vaccination_time <- factor(pos_sub$Recent_vaccination_time)
pos_sub$Symptom_improvement_time <- factor(pos_sub$Symptom_improvement_time)
pvalue1=chisq.test(table(pos_sub$Symptom_improvement_time,pos_sub$Recent_vaccination_time))

plot28=ggplot(pos_sub, aes(Recent_vaccination_time)) + labs(y='percent',x='Latest vaccination time ')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=Symptom_improvement_time), position="fill")+scale_fill_manual(values = my36colors)+theme_bw()
plot28

ggsave('不同接种时间好转时间分布.pdf',plot26,width = 8,height = 3)

#29 不同疫苗背景间好转时间分布 Distribution of improvement time among participants in different vaccine combinations
a=pos
a$Vaccine_backgrounds[a$Vaccination == 0 ] = "0"
a$Vaccine_backgrounds[a$Vaccination == 1 & a$First_dose_of_vaccine ==  1] = "1" #1 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 2 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1 ] = "2" #2 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 2 & a$First_dose_of_vaccine ==  2 & a$Second_dose_of_vaccine ==  2 ] = "3" #2 needle adenovirus
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1] = "4" #3 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  3 & a$Second_dose_of_vaccine ==  3  & a$Third_dose_vaccine ==  3] = "5" #3 needle protein
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  3] = "6" #2 needle inactivation + 1 protein
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==1] = "7" #4 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==3] = "8" #3 needle inactivation + 1 protein
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==2] = "9" #3 needle inactivation + 1 needle adenovirus
pos_sub <- na.omit(a)
pos_sub$Vaccine_backgrounds <- factor(pos_sub$Vaccine_backgrounds)
pos_sub$Symptom_improvement_time <- factor(pos_sub$Symptom_improvement_time)
pvalue1=chisq.test(table(pos_sub$Symptom_improvement_time,pos_sub$Vaccine_backgrounds))

plot29=ggplot(pos_sub, aes(Vaccine_backgrounds)) + labs(y='percent',x='Vaccine_backgrounds')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=Symptom_improvement_time), position="fill")+scale_fill_manual(values = my36colors)+theme_bw()
plot29

#30 Infection status with different vaccine doses
pos_sub <- data2
pos_sub$Vaccination <- factor(pos_sub$Vaccination)
pos_sub$Positive_or_not<- factor(pos_sub$Positive_or_not)
pvalue1=chisq.test(table(pos_sub$Positive_or_not,pos_sub$Vaccination))

plot30=ggplot(pos_sub, aes(Vaccination)) + labs(y='percent',x='Vaccination times')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=Positive_or_not), position="fill")+
  scale_fill_manual(values = my36colors)+theme_bw()
plot30

#31 Infection status at different vaccination dates
pos_sub <- data2
pos_sub <- subset(pos_sub,Recent_vaccination_time != (-3))
pos_sub$Recent_vaccination_time <- factor(pos_sub$Recent_vaccination_time)
pos_sub$Positive_or_not <- factor(pos_sub$Positive_or_not)
pvalue1=chisq.test(table(pos_sub$Positive_or_not,pos_sub$Recent_vaccination_time))

plot31=ggplot(pos_sub, aes(Recent_vaccination_time)) + labs(y='percent',x='Latest vaccination time ')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=Positive_or_not), position="fill")+scale_fill_manual(values = my36colors)+theme_bw()
plot31

#32 Infection status of different vaccine combinations
a=data2
a$Vaccine_backgrounds[a$Vaccination == 0 ] = "0"
a$Vaccine_backgrounds[a$Vaccination == 1 & a$First_dose_of_vaccine ==  1] = "1" #1 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 2 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1 ] = "2" #2 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 2 & a$First_dose_of_vaccine ==  2 & a$Second_dose_of_vaccine ==  2 ] = "3" #2 needle adenovirus
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1] = "4" #3 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  3 & a$Second_dose_of_vaccine ==  3  & a$Third_dose_vaccine ==  3] = "5" #3 needle protein
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  3] = "6" #2 needle inactivation + 1 protein
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==1] = "7" #4 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==3] = "8" #3 needle inactivation + 1 protein
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==2] = "9" #3 needle inactivation + 1 needle adenovirus
pos_sub <- na.omit(a)
pos_sub$Vaccine_backgrounds <- factor(pos_sub$Vaccine_backgrounds)
pos_sub$Positive_or_not<- factor(pos_sub$Positive_or_not)
pvalue1=chisq.test(table(pos_sub$Positive_or_not,pos_sub$Vaccine_backgrounds))

plot32=ggplot(pos_sub, aes(Vaccine_backgrounds)) + labs(y='percent',x='Vaccine_backgrounds')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=Positive_or_not), position="fill")+scale_fill_manual(values = my36colors)+theme_bw()
plot32

##33 Severity distribution of positive participants with different vaccine doses
pos_sub <- pos
pos_sub$Vaccination <- factor(pos_sub$Vaccination)
pos_sub <- subset(pos_sub,disease_severity != 6)
pos_sub$disease_severity <- gsub('1|2','0',pos_sub$disease_severity)#
pos_sub$disease_severity <- gsub('3|4|5','1',pos_sub$disease_severity)
pvalue1=chisq.test(table(pos_sub$disease_severity,pos_sub$Vaccination))

plot33=ggplot(pos_sub, aes(Vaccination)) + labs(y='percent',x='Vaccination times')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=disease_severity), position="fill")+scale_fill_manual(values = my36colors)+theme_bw()
plot33

##34 Severity distribution of positive participants with different vaccination dates
pos_sub <- subset(pos,Recent_vaccination_time != 5)
pos_sub$Recent_vaccination_time <- factor(pos_sub$Recent_vaccination_time)
pos_sub <- subset(pos_sub,disease_severity != 6)
pos_sub$disease_severity <- gsub('1|2','0',pos_sub$disease_severity)#
pos_sub$disease_severity <- gsub('3|4|5','1',pos_sub$disease_severity)
pvalue1=chisq.test(table(pos_sub$disease_severity,pos_sub$Recent_vaccination_time))

plot34=ggplot(pos_sub, aes(Recent_vaccination_time)) + labs(y='percent',x='Latest vaccination time ')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=disease_severity), position="fill")+scale_fill_manual(values = my36colors)+theme_bw()
plot34

#35 Severity distribution of positive participants in different vaccine combinations
a=pos
a$Vaccine_backgrounds[a$Vaccination == 0 ] = "0"
a$Vaccine_backgrounds[a$Vaccination == 1 & a$First_dose_of_vaccine ==  1] = "1" #1 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 2 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1 ] = "2" #2 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 2 & a$First_dose_of_vaccine ==  2 & a$Second_dose_of_vaccine ==  2 ] = "3" #2 needle adenovirus
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1] = "4" #3 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  3 & a$Second_dose_of_vaccine ==  3  & a$Third_dose_vaccine ==  3] = "5" #3 needle protein
a$Vaccine_backgrounds[a$Vaccination == 3 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  3] = "6" #2 needle inactivation + 1 protein
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==1] = "7" #4 needle inactivation
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==3] = "8" #3 needle inactivation + 1 protein
a$Vaccine_backgrounds[a$Vaccination == 4 & a$First_dose_of_vaccine ==  1 & a$Second_dose_of_vaccine ==  1  & a$Third_dose_vaccine ==  1 & a$Fourth_dose_of_vaccine ==2] = "9" #3 needle inactivation + 1 needle adenovirus

pos_sub <- na.omit(a)
pos_sub$Vaccine_backgrounds <- factor(pos_sub$Vaccine_backgrounds)
pos_sub <- subset(pos_sub,disease_severity != 6)
pos_sub$disease_severity <- gsub('1|2','0',pos_sub$disease_severity)
pos_sub$disease_severity <- gsub('3|4|5','1',pos_sub$disease_severity)
pvalue1=chisq.test(table(pos_sub$disease_severity,pos_sub$Vaccine_backgrounds))

plot35=ggplot(pos_sub, aes(Vaccine_backgrounds)) + labs(y='percent',x='Vaccine_backgrounds')+ggtitle(pvalue1$p.value)+
  geom_bar(aes(fill=disease_severity), position="fill")+scale_fill_manual(values = my36colors)+theme_bw()
plot35

#36 Word frequency analysis of other symptoms
library(jiebaRD) 
library(jiebaR) 
library(wordcloud2)

data_other <- read_excel('other_symptoms.xlsx')
data_other <- subset(data_other,number %in% intersect(data2$Serial_number,data_other$number))

data_freq <- freq(data_other$Answer_text)
data_freq <- subset(data_freq,freq>1)

wordcloud2(data_freq,
           backgroundColor = "white",
           shape = 'circle',
           rotateRatio = 0.9,size=0.5,
           color=my36colors
           )