library(pscl)
library(wnominate)
library(foreign)

set.seed(100)

#Setting SIGNAL-NOISE RATIO and WEIGHTS
b<-15
w<-0.5

#Creating unique legislator IDs
leg_id<-c(1:101)

#Generating legislator ideal points
ideal_leg<-seq(-1,1,length.out=101)

#Liberal ideological scores are negative by construction
leg_dem<-as.numeric(ideal_leg<=0)

#Note: Actual legislative polarization is one by construction

#Creating empty Status Quo Matrix
sim.length<-20
num.rollcalls<-400
sq.matrix<-as.matrix(rep(NA,sim.length^2*num.rollcalls))
dim(sq.matrix)<-c(num.rollcalls,sim.length^2)

#Establishing the parameters of beta distributions from which status quos will be drawn
alpha.start<-0.5
alpha.end<-2
alpha.vec<-seq(0.5,2,length.out=sim.length)
beta.vec<-alpha.vec


#Stores random draws from different beta distributions
for (i in 1:sim.length){
  for (j in 1:sim.length){
    overall.iteration<- sim.length*(i-1)+j
    alpha<-alpha.vec[i]
    beta<-beta.vec[j]
    sq.matrix[,overall.iteration]<-2*rbeta(num.rollcalls,shape1=alpha,shape2=beta)-1
  }
}

#Creates an empty matrix to store polarization estimates
pol_matrix_dem_normal<-matrix(nrow=sim.length^2,ncol=3)

#These fill the first 2 columns of pol_matrix_dem_normal with the 
#appropriate alpha and beta values
pol_matrix_dem_normal[,1]<-sort(rep(alpha.vec,sim.length))
pol_matrix_dem_normal[,2]<-rep(beta.vec, sim.length)


#Drawing Status quo values using the parameters from the beta distribution
for (i in 1:sim.length){
  for (j in 1:sim.length){
    overall.iteration<- sim.length*(i-1)+j
    alpha<-alpha.vec[i]
    beta<-beta.vec[j]
    sq.matrix[,overall.iteration]<-2*rbeta(num.rollcalls,shape1=alpha,shape2=beta)-1
  }
}

#Assume that Democrats control the legislature and propose the median members'
#preference
proposals.dem<-rep(-0.5,n=num.rollcalls)

#LEGISLATORS' UTILITY FUNCTION-NOTE THE SIGNAL-NOISE RATIO.
#This determines how the agents behave

leg_utility<-function(proposal)
{
  b*exp((-(w^2)*(proposal-ideal_leg)^2)/2)+rnorm(101,0,1)
}

#CREATING VOTE YES/NO FUNCTION - INCLUDING ERROR
#This generates the voting pattern in the legislature

vote_yes<-function(proposal,status_quo){
  ifelse(leg_utility(proposal)>leg_utility(status_quo),1,0)
}


#The following object will be used to record roll-calls
roll_call<-matrix(nrow=101)
roll_call<-as.data.frame(roll_call)
roll_call[,1]<-leg_id

#DW-NOMINATE requires the user to identify a conservative legislator to orient the
#ideological space
conservative<-101

#The following code draws status quos from a beta distribution, pairs them
#against the policy preferred by the median Democrat in the legislature, generates
#the expected roll-call matrix, estimates legislator ideal points using the matrix,
#and calculates legislative polarization

for (i in 1:dim(sq.matrix)[2]){
  for (j in 1:dim(sq.matrix)[1]){
    sq<-sq.matrix[j,i]
    proposal<-sample(proposals.dem,1)
    vote_yes_temp<-vote_yes(proposal,sq)
    roll_call<-cbind(roll_call, vote_yes_temp)
  }
  roll_call_binary<-roll_call[,-1]
  rc_temp<-rollcall(roll_call_binary, legis.names=leg_id)
  result<-wnominate(rc_temp, dims=1, minvotes=20,
                    lop=0.025,trials=3, polarity=conservative, verbose=TRUE)
  output<-summary(result, verbose=TRUE)
  output<-data.frame(output)
  pol_matrix_dem_normal[i,3]<-abs(median(output$coord1D[1:51])-median(output$coord1D[52:101]))
  roll_call<-matrix(nrow=101)
  roll_call<-as.data.frame(roll_call)
  roll_call[,1]<-leg_id
}

pol_matrix_dem_normal<-data.frame(pol_matrix_dem_normal)

#The following plot illustrates how polarization estimates vary based on the parameters
#of the beta distribution from which status quos are drawn. The results demonstrate
#that status quo policies disproportionately benefiting a single party may generate
#heightened polarization in a legislature

library(plotly)

dem_normal_plotly<-plot_ly(pol_matrix_dem_normal, x=~alpha, y = ~beta, z = ~polarization, type="contour",colorbar = list(title = 'Polarization'))
htmlwidgets::saveWidget(dem_normal_plotly, file = "dem_normal_plotly.html")


