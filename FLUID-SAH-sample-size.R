#########################################################################################
# PURPOSE: CONDUCT SIMULATION STUDIES FOR THE 3 ARM VERSION OF THE FLUID SAH TRIAL
#########################################################################################

#########################################################################################
# READING IN NECESSARY LIBRARIES
library(gsDesign)


#########################################################################################
# OBTAINING SAMPLE SIZE FOR THE SIMULATIONS GIVEN AN EFFECT SIZE
NewSS<-power.prop.test(n=NULL, p1 = .35, p2 = .25, sig.level = 0.05, power = 0.8)
SS_NoInflate<-ceiling(NewSS$n)*3
CrossOverInflate<-1/(.95**2) # assumes a 5% crossover rate 
LTFU<-1/.95
x <- gsDesign(k = 2,
              test.type = 4, 
              n.fix = SS_NoInflate, 
              timing = c(.5,1),
              alpha = 0.025, 
              beta=0.2)
FinalSS_NoInflate<-ceiling(x$n.I[2])
InterimSS<-ceiling(FinalSS_NoInflate/2)
FinalSS<-ceiling(FinalSS_NoInflate*CrossOverInflate*LTFU)


# BOUNDAIRES FOR INTERIM ANALYSES
P_Interim<-pnorm(x$lower$bound)[1]
P_Final<-0.05


#########################################################################################
# SIMULATIONS FOR PRIMARY ANALYSES
set.seed(6994)
Results_Final<-matrix(nrow = 0,ncol = 11)
NSims<-10000
BCNF_POut<-0.25
BCNL_POutl<-c(0.25,0.35)
SLNF_POutl<-c(0.25,0.35)

for(f in 1:length(BCNL_POutl))
{
  for(s in 1:length(SLNF_POutl))
  {
    BCNL_POut<-BCNL_POutl[f]
    SLNF_POut<-SLNF_POutl[s]
    Results<-matrix(nrow = NSims,ncol = 8)
    for(sim in 1:NSims)
    {
      
      Path<-NA
      Treatment_Pre<-sample(x = 1:3,size = InterimSS,replace = T)
      
      BCNF_Subs_Pre<-which(Treatment_Pre==1)
      BCNL_Subs_Pre<-which(Treatment_Pre==2)
      SLNF_Subs_Pre<-which(Treatment_Pre==3)
      
      OutcomeI<-c()
      OutcomeI[BCNF_Subs_Pre]<-rbinom(n=length(BCNF_Subs_Pre),size = 1,prob =BCNF_POut)
      OutcomeI[BCNL_Subs_Pre]<-rbinom(n=length(BCNL_Subs_Pre),size = 1,prob =BCNL_POut)
      OutcomeI[SLNF_Subs_Pre]<-rbinom(n=length(SLNF_Subs_Pre),size = 1,prob =SLNF_POut)
      
      ModelI<-summary(glm(OutcomeI~factor(Treatment_Pre),family = binomial))$coefficients
      BCNL_BCNF_TestI<-ModelI[2,4]
      SLNF_BCNF_TestI<-ModelI[3,4]
      
      # CASE WHERE NO ARM IS STOPPED AT THE INTERIM
      if(BCNL_BCNF_TestI<P_Interim & SLNF_BCNF_TestI<P_Interim)
      {
        Path = 1
        Treatment_Post<-sample(x = 1:3,size = (FinalSS_NoInflate-InterimSS),replace = T)
        
        BCNF_Subs_Post<-which(Treatment_Post==1)
        BCNL_Subs_Post<-which(Treatment_Post==2)
        SLNF_Subs_Post<-which(Treatment_Post==3)
        
        OutcomeP<-c()
        OutcomeP[BCNF_Subs_Post]<-rbinom(n=length(BCNF_Subs_Post),size = 1,prob =BCNF_POut)
        OutcomeP[BCNL_Subs_Post]<-rbinom(n=length(BCNL_Subs_Post),size = 1,prob =BCNL_POut)
        OutcomeP[SLNF_Subs_Post]<-rbinom(n=length(SLNF_Subs_Post),size = 1,prob =SLNF_POut)
        
        TreatmentFinal<-c(Treatment_Post,Treatment_Pre)
        OutcomeFinal<-c(OutcomeP,OutcomeI)
        
        ModelF<-summary(glm(OutcomeFinal~factor(TreatmentFinal),family = binomial))$coefficients
        BCNL_BCNF_TestF<-ModelF[2,4]
        SLNF_BCNF_TestF<-ModelF[3,4]
        
        
      }
      
      # CASE WHERE BCNL IS STOPPED AT THE INTERM
      if(BCNL_BCNF_TestI>=P_Interim & SLNF_BCNF_TestI<P_Interim)
      {
        Path = 2
        Treatment_Post<-sample(x = c(1,3),size = (FinalSS_NoInflate-InterimSS),replace = T)
        
        BCNF_Subs_Post<-which(Treatment_Post==1)
        BCNL_Subs_Post<-which(Treatment_Post==2)
        SLNF_Subs_Post<-which(Treatment_Post==3)
        
        OutcomeP<-c()
        OutcomeP[BCNF_Subs_Post]<-rbinom(n=length(BCNF_Subs_Post),size = 1,prob =BCNF_POut)
        OutcomeP[BCNL_Subs_Post]<-rbinom(n=length(BCNL_Subs_Post),size = 1,prob =BCNL_POut)
        OutcomeP[SLNF_Subs_Post]<-rbinom(n=length(SLNF_Subs_Post),size = 1,prob =SLNF_POut)
        
        TreatmentFinal<-c(Treatment_Post,Treatment_Pre)
        OutcomeFinal<-c(OutcomeP,OutcomeI)
        ModelF<-summary(glm(OutcomeFinal~factor(TreatmentFinal),family = binomial))$coefficients
        BCNL_BCNF_TestF<-ModelF[2,4]
        SLNF_BCNF_TestF<-ModelF[3,4]
      }
      
      # CASE WHERE SNLF IS STOPPED AT THE INTERM
      if(BCNL_BCNF_TestI<P_Interim & SLNF_BCNF_TestI>=P_Interim)
      {
        Path = 3
        Treatment_Post<-sample(x = c(1,2),size = (FinalSS_NoInflate-InterimSS),replace = T)
        
        BCNF_Subs_Post<-which(Treatment_Post==1)
        BCNL_Subs_Post<-which(Treatment_Post==2)
        SLNF_Subs_Post<-which(Treatment_Post==3)
        
        OutcomeP<-c()
        OutcomeP[BCNF_Subs_Post]<-rbinom(n=length(BCNF_Subs_Post),size = 1,prob =BCNF_POut)
        OutcomeP[BCNL_Subs_Post]<-rbinom(n=length(BCNL_Subs_Post),size = 1,prob =BCNL_POut)
        OutcomeP[SLNF_Subs_Post]<-rbinom(n=length(SLNF_Subs_Post),size = 1,prob =SLNF_POut)
        
        TreatmentFinal<-c(Treatment_Post,Treatment_Pre)
        OutcomeFinal<-c(OutcomeP,OutcomeI)
        ModelF<-summary(glm(OutcomeFinal~factor(TreatmentFinal),family = binomial))$coefficients
        BCNL_BCNF_TestF<-ModelF[2,4]
        SLNF_BCNF_TestF<-ModelF[3,4]
      }
      
      # CASE WHERE BOTH STOP AT INTERIM
      if(BCNL_BCNF_TestI>=P_Interim & SLNF_BCNF_TestI>=P_Interim)
      {
        Path = 4
        BCNL_BCNF_TestF<-BCNL_BCNF_TestI
        SLNF_BCNF_TestF<-SLNF_BCNF_TestI
        
      }
      
      
      
      if(sim%%100==0){print(sim)}
      Results[sim,]<-c(Path,BCNL_BCNF_TestI,SLNF_BCNF_TestI,BCNL_BCNF_TestF,SLNF_BCNF_TestF,table(TreatmentFinal))
    }
    Results_Final<-rbind(Results_Final,
                         c(BCNL_POut,SLNF_POut,table( factor(Results[,1], levels = 1:4))/NSims,colMeans(Results[,4:5]<P_Final), 
                           colMeans(Results[,6:8])))
  }
}



colnames(Results_Final)<-c("BCNL","SLNF","1","2","3","4","PFludro","PSaline","AvgSSTRT1","AvgSSTRT2","AvgSSTRT3")