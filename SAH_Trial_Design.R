################################################################################################
# PROGRAMMER: JONATHAN BEALL, PHD AND AKSHITKUMAR MISTRY, MD
# CREATED: 07/18/2023
# EDITED: 08/30/2023
# PURPOSE: COMPARE ALL POSSIBLE SLIDING DICHOTOMIES FOR THE MRS ACROSS WFNS CATEGORIES AND COMPARE 
#          AGAINST THE WILCOXON RANK SUM TEST ("i.e., SHIFT-ANALYSIS")
###############################################################################################
# SETTING WORKING DIRECTORY, LOADING LIBRARIES, READING IN DATA
library(ggpubr)
library(reshape2)
library(samplesize)
library(patchwork)
library(dplyr)
library(cowplot)
library(tidyr)
###############################################################################################
#Distribution of WFNS scores
df <- data.frame(WFNS=c(1,2,3,4,5), Proportion=c(.415,.22,.075,.145,.145))
df$WFNS<-as.factor(df$WFNS)
A<-ggplot(data = df)+
  geom_col(aes(x=WFNS, y=Proportion, fill=WFNS), color="black")+xlab("")+ylab("Proportion in Each Group")+
  scale_x_discrete(position = "top")+ggtitle("A. Expected distribution of WFNS grades")+
  geom_text(aes(x=WFNS, y=Proportion, label=scales::percent(Proportion, accuracy = 0.1)), angle = 0, hjust = -0.1)+
  coord_flip()+scale_y_reverse(labels = scales::percent_format(accuracy = 1))+theme_pubr()+
  theme(plot.title = element_text(face = "bold"))+guides(fill = guide_legend(title = "WFNS Grade"))
A
#Distribution of mRS based on WFNS
MRS_WFNS<-structure(list(Value = c(0.05915493, 0.157746479, 0.532394366, 
                                   0.109859155, 0.030985915, 0.047887324, 0.061971831, 0.042780749, 
                                   0.117647059, 0.454545455, 0.080213904, 0.048128342, 0.069518717, 
                                   0.187165775, 0.051282051, 0.051282051, 0.205128205, 0.179487179, 
                                   0.051282051, 0.179487179, 0.282051282, 0.016216216, 0.032432432, 
                                   0.308108108, 0.124324324, 0.059459459, 0.102702703, 0.356756757, 
                                   0.018072289, 0.018072289, 0.138554217, 0.060240964, 0.042168675, 
                                   0.114457831, 0.608433735), WFNS = c("Grade 1", "Grade 1", "Grade 1", 
                                                                       "Grade 1", "Grade 1", "Grade 1", "Grade 1", "Grade 2", "Grade 2", 
                                                                       "Grade 2", "Grade 2", "Grade 2", "Grade 2", "Grade 2", "Grade 3", 
                                                                       "Grade 3", "Grade 3", "Grade 3", "Grade 3", "Grade 3", "Grade 3", 
                                                                       "Grade 4", "Grade 4", "Grade 4", "Grade 4", "Grade 4", "Grade 4", 
                                                                       "Grade 4", "Grade 5", "Grade 5", "Grade 5", "Grade 5", "Grade 5", 
                                                                       "Grade 5", "Grade 5"), mRS = c("mRS 0", "mRS 1", "mRS 2", "mRS 3", 
                                                                                                      "mRS 4", "mRS 5", "mRS 6", "mRS 0", "mRS 1", "mRS 2", "mRS 3", 
                                                                                                      "mRS 4", "mRS 5", "mRS 6", "mRS 0", "mRS 1", "mRS 2", "mRS 3", 
                                                                                                      "mRS 4", "mRS 5", "mRS 6", "mRS 0", "mRS 1", "mRS 2", "mRS 3", 
                                                                                                      "mRS 4", "mRS 5", "mRS 6", "mRS 0", "mRS 1", "mRS 2", "mRS 3", 
                                                                                                      "mRS 4", "mRS 5", "mRS 6")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                       -35L))
MRS_WFNS$WFNS_n<-as.numeric(sapply(strsplit(MRS_WFNS$WFNS,split = " "), "[[", 2))
MRS_WFNS$MRS_n<-as.numeric(sapply(strsplit(MRS_WFNS$mRS,split = " "), "[[", 2))
#MRS_WFNS$Value<-round(MRS_WFNS$Value,2)
CumulativePrb<-matrix(nrow = 0, ncol = 4)
MRS_WNFS_Table<-matrix(nrow = 5, ncol = 7)
for(w in 1:5)
{
  for(m in 0:6)
  {
    label<-MRS_WFNS[which(MRS_WFNS$WFNS_n==w & MRS_WFNS$MRS_n==m),"Value"]
    loop1<-sum(MRS_WFNS[which(MRS_WFNS$WFNS_n==w & MRS_WFNS$MRS_n<m),"Value"])
    loop2<-sum(MRS_WFNS[which(MRS_WFNS$WFNS_n==w & MRS_WFNS$MRS_n<=m),"Value"])
    val<-(loop2-loop1)/2+loop1
    CumulativePrb<-rbind(CumulativePrb,c(val,label,w,m))
    MRS_WNFS_Table[w,(m+1)]<-(loop2-loop1)
  }
}
colnames(CumulativePrb)<-c("Val","Label","WFNS_n","mRS")
CumulativePrb<-as.data.frame(CumulativePrb)
B<-ggplot(data = MRS_WFNS)+
  geom_col(aes(x = as.factor(WFNS_n) ,y = Value,fill = mRS),color = "black", position = position_stack(reverse = TRUE))+
  scale_fill_brewer(palette="RdBu", direction = -1)+ggtitle("B. Correlation between WFNS grade and mRS")+
  geom_text(data = CumulativePrb,aes(x = WFNS_n,y = (Val), label = scales::percent(Label, accuracy = 1L)), angle = 0)+
  xlab("WFNS Grade")+
  ylab("Proportion in Each Group")+
  coord_flip()+theme_pubr()+theme(plot.title = element_text(face = "bold"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#Final Expected Control mRS distribution
MRS_WFNS$Dist<-c(rep(.415,7),rep(.22,7),rep(.075,7),rep(.145,7),rep(.145,7))
MRS_WFNS$prod<-MRS_WFNS$Value*MRS_WFNS$Dist
fdist<-MRS_WFNS %>%group_by(mRS) %>%summarise(sum = sum(prod))
fdist$Final<-as.factor(c(rep("Final",7)))
fdist$mRS<-as.factor(fdist$mRS)
fdist$textval<-(cumsum(fdist$sum)+c(0,cumsum(fdist$sum)[1:6]))/2
C<-ggplot(data = fdist)+
  geom_col(aes(x = Final ,y = sum,fill = mRS),color = "black", position = position_stack(reverse = TRUE))+
  scale_fill_brewer(palette="RdBu", direction = -1)+
  geom_text(data = fdist,aes(x = Final,y = textval, label = scales::percent(sum, accuracy = 0.1)))+
  ggtitle("C. Expected distribution of mRS scores")+ylab("Proportion in Each Group")+
  coord_flip()+theme_pubr()+theme(plot.title = element_text(face = "bold"))+
  guides(fill = guide_legend(nrow = 1))+rremove("ylab")+rremove("legend")+rremove("y.text")+rremove("y.ticks")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

((A+B+plot_layout(widths = c(1,2)))/C+plot_layout(heights = c(5,1.25)))
#1500 x 550

# SETTING POSSIBLE DICHOTIMIZATION SCHEMES VALUES FOR MRS (here cut point is less than not equal to number)
Possible_Dichot_Points<- seq(1:6)
Possible_Dichot_Schemes<-expand.grid(WFNS1 = Possible_Dichot_Points,
                            WFNS2 = Possible_Dichot_Points,
                            WFNS3 = Possible_Dichot_Points,
                            WFNS4 = Possible_Dichot_Points,
                            WFNS5 = Possible_Dichot_Points)
# CREATING LOGIC CHECK FOR POSSIBLE SLIDING DICHOTOMIES (THAT FOR WNFS GRADE G THE MRS CUTPOINT C_G BE <= CUTPOINT C_(G+1))
GoodSchemes<-Possible_Dichot_Schemes$WFNS1<= Possible_Dichot_Schemes$WFNS2 & 
             Possible_Dichot_Schemes$WFNS2<= Possible_Dichot_Schemes$WFNS3 & 
             Possible_Dichot_Schemes$WFNS3<= Possible_Dichot_Schemes$WFNS4 & 
             Possible_Dichot_Schemes$WFNS4<= Possible_Dichot_Schemes$WFNS5 
# RULES FOR REALISTIC CUTPOINTS IN EACH WFNS GRADE 
c_WFNS1<-3 # for WFNS 1 the cutpoint should be less than 0-2
c_WFNS1_check<-Possible_Dichot_Schemes$WFNS1<=c_WFNS1
c_WFNS5_l<-4
c_WFNS5_u<-5 # WFNS 5 should either be 3 or 4
c_WFNS5_check<-Possible_Dichot_Schemes$WFNS5<=c_WFNS5_u & c_WFNS5_l<=Possible_Dichot_Schemes$WFNS5
Final_Dichot_Schemes<-Possible_Dichot_Schemes[GoodSchemes & c_WFNS1_check & c_WFNS5_check,]
Final_Dichot_Schemes[nrow(Final_Dichot_Schemes) + 1,] <- c(3,3,3,3,3) #adding 0-2
Final_Dichot_Schemes[nrow(Final_Dichot_Schemes) + 1,] <- c(4,4,4,4,4) #adding 0-3 mRS (which his same as4-6)

# SETTING UP MATRIX TO HOLD GOOD OUTCOME PROBABILITIES BASED ON THE THE MRS DISTRIBUTION FOR ALL DICHOTOMY SCHEMES
Probabilities<-matrix(nrow = nrow(Final_Dichot_Schemes), ncol = 5)
for(d in 1:nrow(Final_Dichot_Schemes))
{
  for(w in 1:5)
  {
    # FOR EACH ROW IN POSSIBLE DICHOTOMIZATION SCHEMES GET THE CUMULATIVE SUM # REPRESENT THE PROBABILITY OF OUTCOME
    MRS_Cut<-eval(parse(text=paste("Final_Dichot_Schemes$WFNS",w,sep="")))[d]
    Probabilities[d,w]<-sum(MRS_WFNS[which(MRS_WFNS$WFNS_n==w & MRS_WFNS$MRS_n<MRS_Cut),1])
  }
}

WFNS_Dist<-matrix(c(.415,.22,.075,.145,.145),nrow = 1) #distribution of WFNS grades
WFNS_1_Range<-seq(0,.415,by = 0.005) #ranges of possible WFNS1 allowed
Perc_to_Allocate_Old<-matrix(WFNS_Dist[1,1]-WFNS_1_Range,ncol = 1) #adjusting all proportions based on change in WFNS1
Allocated_Probs_Old<-Perc_to_Allocate_Old%*%(WFNS_Dist[1,2:5]/sum(WFNS_Dist[1,2:5]))
Final_Probs_Hern<-matrix(cbind(WFNS_1_Range,
                               Allocated_Probs_Old[,1]+WFNS_Dist[1,2],
                               Allocated_Probs_Old[,2]+WFNS_Dist[1,3],
                               Allocated_Probs_Old[,3]+WFNS_Dist[1,4],
                               Allocated_Probs_Old[,4]+WFNS_Dist[1,5]),ncol = 5)

#Sample Size Calculation for Two-sample test
ef<-seq(0.1,0.3,by=0.01) #different Effect Size (ef) to test
Population_Outcome_Rates<-(Probabilities) %*% t(Final_Probs_Hern)
test<-do.call(rbind, replicate(length(ef), Population_Outcome_Rates, simplify=FALSE))
a<-cbind(test,c(rep(ef,each=nrow(Population_Outcome_Rates))))
SS<-matrix(nrow = nrow(a),ncol = ncol(a)-1)
for(i in 1:nrow(a))
{
  for(j in 1:ncol(a)-1) {
    # NOTE: SINCE WE ARE LOOKING FOR AN INCREASE IN LOWER VALUES THE PROBABILITY OF OUTCOME 
    # MUST BE LESS THAN 1-THE DESIRED EFFECT SIZE WITH SOME ADDITIONAL ROOM (HENCE -0.01)
    ifelse(a[i,j]<=(1-a[i,ncol(a)]-0.01),
           # SAMPLE SIZE IS 2 SAMPLE TEST OF PROPORTIONS
           SS[i,j]<-round(2*power.prop.test(n = NULL,p1=a[i,j],p2 = (a[i,j]+a[i,ncol(a)]),power = .8,sig.level = 0.05)$n),
           # IF THE EFFECT SIZE IS NOT IN REASONABLE RANGE DO NOTHING
           SS[i,j]<-NA)
  }
}
colnames(SS)<-WFNS_1_Range
SS2<-cbind(a[,ncol(a)],SS)
colnames(SS2)[1]<-"EffectSize"
Final_w_Hern<-as.data.frame(cbind((do.call(rbind, replicate(length(ef), Final_Dichot_Schemes, simplify=FALSE))),SS2))

#Figure for Sample Size vs. Effect Size for 4 dichtomoy schemes
ssef<-Final_w_Hern[,c(1:6,90)]
ssef$Dichot<-paste(ssef$WFNS1,ssef$WFNS2,ssef$WFNS3,ssef$WFNS4,ssef$WFNS5,sep = "")
ssef<-ssef[which(ssef$Dichot=="33333" | ssef$Dichot == "44444" | ssef$Dichot=="11114" |ssef$Dichot=="25555"),]
colnames(ssef)[7]<-"SS"

D<-ggplot(data = ssef)+
  geom_line(aes(x = EffectSize , y = SS, color = Dichot),size = 1.25)+
  scale_color_discrete(labels=c("(Slide) 0,0,0,0,3", "(Slide) 1,4,4,4,4","(Fixed) mRS 0-2","(Fixed) mRS 4-6"))+
  xlab("Effect Size")+ggtitle("A. Sample size estimates for select dichotomy schemes")+
  ylab("Sample Size (80% Power, 5% Type I Error)")+ylim(c(0,800))+
  theme_pubr()+scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.grid.major = element_line(linetype = 2),plot.title = element_text(face = "bold"), legend.position = c(0.8,0.8))+
  guides(color=guide_legend(title="Schemes dichotomizing\nmRS at WFNS 1,2,3,4,5", ncol=1))
D
#dichot dependent sample size
ds<-Final_w_Hern[Final_w_Hern$EffectSize==0.1,c(1:5,47,90)]
ds$WFNS1<-ds$WFNS1-1
ds$WFNS2<-ds$WFNS2-1
ds$WFNS3<-ds$WFNS3-1
ds$WFNS4<-ds$WFNS4-1
ds$WFNS5<-ds$WFNS5-1
ds$Dichot<-paste(ds$WFNS1,ds$WFNS2,ds$WFNS3,ds$WFNS4,ds$WFNS5,sep = "")
ds$Dichot<-factor(ds$Dichot, levels = ds$Dichot[order(ds$Dichot,decreasing = T)])
colnames(ds)[6]<-"Perc0.2"
colnames(ds)[7]<-"Perc0.415"
ds<-gather(ds,perc,ss,Perc0.2:Perc0.415)
s1<-ggplot(ds, aes(x=Dichot, y=ss, color=perc))+geom_point()+theme_pubr()+ylab("Sample Size\n(80% Power, 5% Type I Error, 10% Effect Size)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab("Dichotomization Scheme with mRS Cut Point (\u2264) at WFNS Grade 1,2,3,4,5")+
  scale_color_manual(name="WFNS Grade 1 Included", labels=c('20%', '41.5%'), values = c("red", "blue"))+
  ggtitle("Sample size vs. dichotomy scheme")+
  theme(plot.title = element_text(face = "bold"), panel.grid.major = element_line(linetype = 2))

s1
#1800x600

#Calculate a shift in mRS scores based on log odds for a logistic ordinal regression
Ordinal_Shift<-function(Original_Prop,Shift)
{
  Cumulative_Sum<-cumsum(Original_Prop)
  logit<-log(Cumulative_Sum/(1-Cumulative_Sum))
  new_logit<-logit+Shift #shift applied to the logit
  New_Cumulative_Sum<-exp(new_logit)/(1+exp(new_logit))
  New_Cumulative_Sum[length(New_Cumulative_Sum)]<-1
  New_Prop<-c()
  New_Prop[1]<-New_Cumulative_Sum[1]
  for(i in 2:length(Cumulative_Sum))
  {
    New_Prop[i]<-New_Cumulative_Sum[i]-New_Cumulative_Sum[i-1]
  }
  return(New_Prop)
}
MRS<-round(fdist$sum,3)
basedata<-as.data.frame(cbind(0:6,as.matrix(MRS, ncol = 1,byrow = T)))
colnames(basedata)<-c("MRS","P_MRS")
basedata$MRS<-as.factor(basedata$MRS)
basedata$Group<-"Control"
Shift<-c(seq(0.35,1,by = 0.01))
s=1
SS3<-matrix(nrow = length(Shift),ncol = 2)
for(s in 1:length(Shift))
{
  New<-Ordinal_Shift(Original_Prop=MRS,Shift = Shift[s])
  SampleSize<-n.wilcox.ord(power = 0.8, alpha = 0.05, .5, MRS, New)$`total sample size`
  SS3[s,2]<-SampleSize
}
SS3[,1]<-Shift

fdist4<-structure(list(mRS = structure(1:7, levels = c("mRS 0", "mRS 1", 
                                                       "mRS 2", "mRS 3", "mRS 4", "mRS 5", "mRS 6"), class = "factor"), 
                       sum = Ordinal_Shift(MRS,0.4), Final = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L), levels = "0.4", class = "factor"), 
                       textval = c(0.021625, 0.09515, 0.347025, 0.598425, 0.6707, 
                                   0.7316, 0.885825)), row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame"))
fdist4$textval<-(cumsum(fdist4$sum)+c(0,cumsum(fdist4$sum)[1:6]))/2

fdist5<-structure(list(mRS = structure(1:7, levels = c("mRS 0", "mRS 1", 
                                                       "mRS 2", "mRS 3", "mRS 4", "mRS 5", "mRS 6"), class = "factor"), 
                       sum = Ordinal_Shift(MRS,0.5), Final = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L), levels = "0.5", class = "factor"), 
                       textval = c(0.021625, 0.09515, 0.347025, 0.598425, 0.6707, 
                                   0.7316, 0.885825)), row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame"))
fdist5$textval<-(cumsum(fdist5$sum)+c(0,cumsum(fdist5$sum)[1:6]))/2

fdist6<-structure(list(mRS = structure(1:7, levels = c("mRS 0", "mRS 1", 
                                                       "mRS 2", "mRS 3", "mRS 4", "mRS 5", "mRS 6"), class = "factor"), 
                       sum = Ordinal_Shift(MRS,0.6), Final = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L), levels = "0.6", class = "factor"), 
                       textval = c(0.021625, 0.09515, 0.347025, 0.598425, 0.6707, 
                                   0.7316, 0.885825)), row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame"))
fdist6$textval<-(cumsum(fdist6$sum)+c(0,cumsum(fdist6$sum)[1:6]))/2

fd<-rbind(fdist,fdist4,fdist5,fdist6)
colnames(fd)[3]<-"EffectSize"
fd$EffectSize<-as.character(fd$EffectSize)
fd$EffectSize[1:7]<-"Control"
fd$EffectSize<-factor(fd$EffectSize, levels = c("Control","0.4","0.5","0.6"))

shiftto46 <- data.frame ("Log Odds Shift / Sample Size" = c("0.4 / 652","0.5 / 420","0.6 / 294"),
                         "Equivalent mRS \u22654 Effect / Sample Size" = c("8.5% / 924","10.4% / 606","12.2% / 432"),
                         check.names = FALSE) #samples sizes here are obtained separately
t<-ggtexttable(shiftto46, theme = ttheme("classic", base_size=11), rows = NULL) 

E<-ggplot(data = as.data.frame(SS3))+
  geom_line(aes(x = V1 , y = V2),size = 1.25)+
  xlab("Log Odds Shift in mRS Distribution")+
  ylab("")+ylim(c(0,800))+
  theme_pubr()+ggtitle("B. Sample size estimates for a shift analysis")+
  theme(panel.grid.major = element_line(linetype = 2),plot.title = element_text(face = "bold"))

f<-ggplot(data = fd)+
  geom_col(aes(x = as.factor(EffectSize), y = sum,fill = mRS),color = "black", position = position_stack(reverse = TRUE))+
  scale_fill_brewer(palette="RdBu", direction = -1)+
  geom_text(data = fd,aes(x = EffectSize,y = textval, label = scales::percent(sum, accuracy = 1L)))+
  xlab("Log Odds Shift")+
  ylab("Proportion in Each Group")+
  coord_flip()+theme_pubr()+theme(plot.title = element_text(face = "bold"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_x_discrete(limits=rev)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_pubr()+ggtitle("C. Power of shift vs. dichotomous analysis")+
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")+
  labs(fill=NULL)

ff<-f/t

#################################################################################################
# Removing herniated WFNS 5 patients which are ~30% of the patients, leaving 70.8% of non-hWFNS 5 patients
wfsn5_noHern<-matrix(Final_Probs_Hern[,5]*0.708)
Perc_to_Allocate_noHern<-matrix(Final_Probs_Hern[,5]*(1-0.708), ncol = 1)
Allocated_Probs_noHern<-Perc_to_Allocate_noHern[,1]*(Final_Probs_Hern[,1:4]/rowSums(Final_Probs_Hern[,1:4]))
Final_Probs_No_Hern<-cbind(Allocated_Probs_noHern+Final_Probs_Hern[,1:4], wfsn5_noHern)

Probabilities_no_hern<-matrix(nrow = nrow(Final_Dichot_Schemes), ncol = 5)
MRS_WFNS_no_hern<-MRS_WFNS
MRS_WFNS_no_hern[c(29:35),1]<-c(.02,.03,.25,.20,.05,.10,.35) #estimated non-herniated WFNS 5 distribution
MRS_WFNS_no_hern$Dist<-c(rep(Final_Probs_No_Hern[84,1],7),rep(Final_Probs_No_Hern[84,2],7),
                         rep(Final_Probs_No_Hern[84,3],7),rep(Final_Probs_No_Hern[84,4],7),
                         rep(Final_Probs_No_Hern[84,5],7))
MRS_WFNS_no_hern$prod<-MRS_WFNS_no_hern$Value*MRS_WFNS_no_hern$Dist
for(d in 1:nrow(Final_Dichot_Schemes))
{
  for(w in 1:5)
  {
    # FOR EACH ROW IN POSSIBLE DICHOTOMIZATION SCHEMES GET THE CUMULATIVE SUM # REPRESENT THE PROBABILITY OF OUTCOME
    MRS_Cut<-eval(parse(text=paste("Final_Dichot_Schemes$WFNS",w,sep="")))[d]
    Probabilities_no_hern[d,w]<-sum(MRS_WFNS_no_hern[which(MRS_WFNS_no_hern$WFNS_n==w & MRS_WFNS_no_hern$MRS_n<MRS_Cut),1])
  }
}

Population_Outcome_Rates<-(Probabilities_no_hern) %*% t(Final_Probs_No_Hern)
SS4<-matrix(nrow = nrow(Population_Outcome_Rates),ncol = ncol(Population_Outcome_Rates))
Effect_Size<-0.10
for(i in 1:nrow(Population_Outcome_Rates))
{
  for(j in 1:ncol(Population_Outcome_Rates))
    # NOTE: SINCE WE ARE LOOKING FOR AN INCREASE IN LOWER VALUES THE PROBABILITY OF OUTCOME 
    # MUST BE LESS THAN 1-THE DESIRED EFFECT SIZE WITH SOME ADDITIONAL ROOM (HENCE -0.01)
    if(Population_Outcome_Rates[i,j]<=(1-Effect_Size-0.01))
    {
      # SAMPLE SIZE IS 2 SAMPLE TEST OF PROPORTIONS
      SS4[i,j]<-round(2*power.prop.test(n = NULL,p1=Population_Outcome_Rates[i,j],p2 = (Population_Outcome_Rates[i,j]+Effect_Size),power = .8,sig.level = 0.05)$n)
    }
  if(Population_Outcome_Rates[i,j]>(1-Effect_Size-0.01))
  {
    # IF THE EFFECT SIZE IS NOT IN REASONABLE RANGE DO NOTHING
    SS4[i,j]<-NA
  }
}
colnames(SS4)<-Final_Probs_No_Hern[,1] #should be new WFNS 1 grade proportion from Final_Probs_No_Hern (not WFNS_1_Range)
Final_No_Hern<-as.data.frame(cbind((Final_Dichot_Schemes),SS4))

#Final data frame for dichotomous analyses
Final_No_Hern$Hern<-"Excluded"
Final_w_Hern$Hern<-"Included"
Final_Hern<-subset(Final_w_Hern, Final_w_Hern$EffectSize=="0.1")
Final_Hern<-Final_Hern[,c(1:5,7:91)]
Long_Final_Hern<-gather(Final_Hern, WFNS_1R, SS, "0":"0.415", factor_key=TRUE)
Long_Final_No_Hern<-gather(Final_No_Hern,WFNS_1R, SS, "0":colnames(Final_No_Hern)[ncol(Final_No_Hern)-1], factor_key=TRUE)
Long_Final<-rbind(Long_Final_No_Hern,Long_Final_Hern)

colnames(Long_Final)<-c("WFNS1", "WFNS2", "WFNS3", "WFNS4", "WFNS5","Herniated","Perc_WFNS1","SS")
Long_Final$Dichot<-paste(Long_Final$WFNS1,Long_Final$WFNS2,Long_Final$WFNS3,Long_Final$WFNS4,Long_Final$WFNS5,sep = "")
Long_Final$Perc_WFNS1<-as.numeric(as.character(Long_Final$Perc_WFNS1))
Long_Final$SS<-as.numeric(as.character(Long_Final$SS))
Long_Final$Herniated<-as.factor(Long_Final$Herniated)
Long_Final$Test<-paste("Two Proportions \n Dichot. ",Long_Final$Dichot)

###################################################################################################
# Shift Sample Size 
MRS_WNFS_nohern_Table<-MRS_WNFS_Table
MRS_WNFS_nohern_Table[5,]<-c(.02,.03,.25,.20,.05,.10,.35)
No_Hern<-t(MRS_WNFS_nohern_Table)%*%t(Final_Probs_No_Hern)/colSums(t(MRS_WNFS_nohern_Table)%*%t(Final_Probs_No_Hern))
Hern<-t(MRS_WNFS_Table)%*%t(Final_Probs_Hern)/colSums(t(MRS_WNFS_Table)%*%t(Final_Probs_Hern))

SS5<-matrix(nrow=ncol(Hern),ncol=3)
for(i in 1:nrow(SS5))
{a<-n.wilcox.ord(power=0.8, alpha=0.05, .5, Hern[,i], Ordinal_Shift(Original_Prop=Hern[,i],0.4))$`total sample size`
  SS5[i,2]<-a}
SS5[,1]<-WFNS_1_Range
SS5[,3]<-"Included"

SS6<-matrix(nrow=ncol(No_Hern),ncol=3)
for(i in 1:nrow(SS6))
{a<-n.wilcox.ord(power=0.8, alpha=0.05, .5, No_Hern[,i], Ordinal_Shift(Original_Prop=No_Hern[,i],0.4))$`total sample size`
  SS6[i,2]<-a}
SS6[,1]<-Final_Probs_No_Hern[,1]
SS6[,3]<-"Excluded"

Long_WRS_SS<-as.data.frame(rbind(SS5,SS6))
colnames(Long_WRS_SS)<-c("Perc_WFNS1","SS","Herniated")
Long_WRS_SS$Perc_WFNS1<-as.numeric(Long_WRS_SS$Perc_WFNS1)
Long_WRS_SS$SS<-as.numeric(Long_WRS_SS$SS)
Long_WRS_SS$Herniated<-as.factor(Long_WRS_SS$Herniated)
Long_WRS_SS$Dichot<-NA
Long_WRS_SS$Test<-"Wilcoxon Rank-Sum"
############################################################################################################
#Final dataset
# Long_Final$Dichot="44444" is  mRS cut at 0-3 vs. 4-6 
PlotData<-Long_Final[which(Long_Final$Dichot=="33333" | Long_Final$Dichot == "44444" | Long_Final$Dichot=="11114" |Long_Final$Dichot=="25555"),]
FinalPlot<-rbind(PlotData[,c("Perc_WFNS1","Herniated","Dichot","Test","SS")],Long_WRS_SS[,c("Perc_WFNS1","Herniated","Dichot","Test","SS")])
FinalPlot$PlotGroup<-as.factor(paste(FinalPlot$Dichot,FinalPlot$Herniated))
FinalPlot$Herniated<-factor(FinalPlot$Herniated, levels = c("Included","Excluded"))

G<-ggplot(data = FinalPlot)+
  geom_line(aes(x = Perc_WFNS1 , y = SS, color = Dichot, lty = Herniated),size = 1.25)+
  scale_color_discrete(labels=c("(Slide) 0,0,0,0,3; 10%", "(Slide) 1,4,4,4,4; 10%","(Fixed) mRS 0-2; 10%","(Fixed) mRS 4-6; 10%","(Shift); Log Odds 0.4"))+
  xlab("WFNS Grade 1 Patients Enrolled (%)")+ggtitle("D. Sample size estimates by mRS analysis & WFNS eligibility")+
  ylab("")+ylim(c(0,800))+
  theme_pubr()+scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.grid.major = element_line(linetype = 2),plot.title = element_text(face = "bold"), legend.position = c(0.55,0.2),
        legend.direction = "vertical", legend.box = "horizontal")+
  guides(color=guide_legend(title="mRS Analysis; Effect size", ncol=1, order=1), 
         lty=guide_legend(title="Herniated WFNS\nGrade 5", order = 2))

G
#change caused by decreasing WFNS
max(Final_Hern[,c(6:89)]-Final_Hern[,89])
min(Final_Hern[,c(6:89)]-Final_Hern[,89])


(D+E)/(ff|G)
#1250 x 1150

((A+B+plot_layout(widths = c(1,2)))/C)/(D+E+f) + plot_layout(heights = c(4,1,8))
#2000x1100

minmax<-Long_Final %>%
  group_by(Dichot,Herniated) %>%
  summarize(minS = min(SS), minSS = Perc_WFNS1[which.min(SS)], 
            maxS = max(SS), maxSS = Perc_WFNS1[which.max(SS)])

plot(1, type = "n", xlab = "Control Proportion", ylab = "Sample Size (-10% effect size; 80% power; Type I error 5%)", xlim=c(0.2,0.9),ylim=c(300,800))
title(main = "Sample Size for a Two-Sample Test for Proportions")
for (i in seq(0.2,0.9,0.01)){
  ss<-round(2*power.prop.test(n=NULL, p1=i, p2=i-.1, sig.level = 0.05,power = 0.8)$n)
  points(i,ss)
}
grid()
#600x600
