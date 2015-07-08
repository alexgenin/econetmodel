# October 2012
# Sonia Kéfi

# Note: parallelise with snowfall
# write the equations in a matrix form

# cd /home/skefi/Dropbox/ProjectIanNTI

rm(list=ls())
#setwd("/Dropbox/ProjectIanNTI")
library("deSolve")
	
maxtime <- 200 #1000
steplenth <- 0.01
tsteps <- maxtime/steplenth+1
analyzetime <- maxtime*0.8

# Here is where the presence of NTI in the model can be activated/disactivated	
NTICrabonWhelk <- 1 # 1 if there is a neg NTI, 0 otherwise
NTIWhelkonGrazers <- 1 # 1 if there is a neg NTI, 0 otherwise
PosNTIMusselsOnAlgae <- 1 # 1 if mussels increase establishment of algae, 0 otherwise
CompetSpaceAmongAlgae <- 1 # 1 if algae compete for space among each other, 0 otherwise
NegNTIMusselsOnAlgae <- 1 # 1 if there is competition for space from mussels on algae, 0 otherwise

#-----------------------------------------------------------------------
{## Model parameters, all body mass-dependent
	times <-seq(0, maxtime, length = tsteps)
	
	## Body masses: consumers are supposed to be 10 times larger than their prey
	# This can be changed to put actual values
	mb <- 1 # body mass of basal species (the 4 algae groups)
	mi <- 10 # body mass of intermediate species (Grazers and mussels)
	mt <- 100 # body mass of top species (Whelks and crabs)

	## Parameters for basal species (algae)
	r1 <- 1 # maximum growth rate
	r2 <- 1 # maximum growth rate
	r3 <- 1 # maximum growth rate
	r4 <- 1 # maximum growth rate
	K1 <- 1 # half-saturation constant
	K2 <- 1 # half-saturation constant
	K3 <- 1 # half-saturation constant
	K4 <- 1 # half-saturation constant
	
	## Constants for consumers
	y <- 8 # maximum ingestion rate of consumer per unit metabolic rate of resource/prey
	x0 <- 0.2227 # constant
	e <- 0.85 # biomass conversion efficiency
	eps <- 0.01064
	
	## metabolism
	x3 <- x0*mi^(-0.25)
	x4 <- x0*mi^(-0.25)
	x1 <- x0*mt^(-0.25)
	x2 <- x0*mt^(-0.25)
	
	## handling time
	h1 <- 1/(y*x1)
	h2 <- 1/(y*x2)
	h3 <- 1/(y*x3)
	h4 <- 1/(y*x4)
	
	## attack rate
	a0 <- 27.23734
	a13 <- a0*x1*exp(-eps*mt/mi)
	a14 <- a0*x1*exp(-eps*mt/mi)
	a23 <- a0*x2*exp(-eps*mt/mi)
	a24 <- a0*x2*exp(-eps*mt/mi)
	a35 <- a0*x3*exp(-eps*mi/mb)
	a36 <- a0*x3*exp(-eps*mi/mb)
	a37 <- a0*x3*exp(-eps*mi/mb)
	a38 <- a0*x3*exp(-eps*mi/mb)
	a45 <- a0*x4*exp(-eps*mi/mb)
	a46 <- a0*x4*exp(-eps*mi/mb)
	a47 <- a0*x4*exp(-eps*mi/mb)
	a48 <- a0*x4*exp(-eps*mi/mb)
	
	# Relative consumption rates
	w13 <- 0.5
	w14 <- 0.5
	w23 <- 0.5
	w24 <- 0.5
	w35 <- 3/7
	w36 <- 2/7
	w37 <- 1/7
	w38 <- 1/7
	w45 <- 0.25
	w46 <- 0.25
	w47 <- 0.25
	w48 <- 0.25
	
	if(CompetSpaceAmongAlgae==1){
		## Adds competition coefficients between algae groups
		# It would be more elegant to define a 4x4 matrices to write down all the interactions between the 4 algae groups. To simplify, we define these terms as not directional, and we omit them in the equation of A1 to take into account the fact that it does not suffer from competition with the other algae groups. 
		c56 <- 0.1
		c57 <- 0.1
		c58 <- 0.1
		c67 <- 0.1
		c78 <- 0.1
		c68 <- 0.1}
	else{
		c56 <- 0
		c57 <- 0
		c58 <- 0
		c67 <- 0
		c78 <- 0
		c68 <- 0}
	
	if(NegNTIMusselsOnAlgae == 1){
		# Mussels compete for space with algae A2 and A3
		# If this is indeed the case, put everything to 0 otherwise
		c45 <- 0
		c46 <- 0.1 
		c47 <- 2*c46
		c48 <- 0}
	else{
		c45 <- 0
		c46 <- 0 
		c47 <- 0
		c48 <- 0}
		
	parms <-c(r1=r1, K1=K1, r2=r2, K2=K2, r3=r3, K3=K3, r4=r4, K4=K4, x1=x1, x2=x2, x3=x3, x4=x4, y=y, x0=x0, e=e, eps=eps, h1=h1, h2=h2, h3=h3, h4=h4, a0=a0, a13=a13, a14=a14, a23=a23, a24=a24, a35=a35, a36=a36, a37=a37, a38=a38, a45=a45, a46=a46, a47=a47, a48=a48, c56=c56, c57=c57, c58=c58, c67=c67, c78=c78, c68=c68, c45=c45, c46=c46, c47=c47, c48=c48, w13=w13, w14=w14, w23=w23, w24=w24, w35=w35, w36=w36, w37=w37, w38=w38, w45=w45, w46=w46, w47=w47, w48=w48)
	} # end definition of the model parameters

#-----------------------------------------------------------------------
# Equation definition
	
#print(system.time(
	phmodel <-function(t, x, parms){
				# Very small biomass values are put to zero
				x[1] <- ifelse(x[1] < 10^(-30), 0, x[1])
				x[2] <- ifelse(x[2] < 10^(-30), 0, x[2])
				x[3] <- ifelse(x[3] < 10^(-30), 0, x[3])
				x[4] <- ifelse(x[4] < 10^(-30), 0, x[4])
				x[5] <- ifelse(x[5] < 10^(-30), 0, x[5])
				x[6] <- ifelse(x[6] < 10^(-30), 0, x[6])
				x[7] <- ifelse(x[7] < 10^(-30), 0, x[7])
				x[8] <- ifelse(x[8] < 10^(-30), 0, x[8])
				
				# Renaming the variables
				W <- x[1]   # whelks
				Cr <- x[2]  # crabs
				G <- x[3]   # grazers
				M <- x[4]   # mussels
				A1 <- x[5]  # Algae, group 1
				A2 <- x[6]  # Algae, group 2
				A3 <- x[7] # Algae, group 3
				A4 <- x[8] # Algae, group 4		
				
				# Definition of the equations
				with (as.list(parms), {
				
						if(NTICrabonWhelk == 1){
						# Adds NT negative effect of crabs on whelks - let's say that the crabs can devide the attack rates and handling time of the whelks by dnt at max
						# The attack rates and the handling time of the whelks become functions of the crab biomass
						dnt <- 5
						a13o <- (a13*x[2]/dnt+a13)/(x[2]+1) 
						a14o <- (a14*x[2]/dnt+a14)/(x[2]+1)
						h1o <- (h1*x[2]/dnt+h1)/(x[2]+1)}
						else{
						a13o <- a13 
						a14o <- a14
						h1o <- h1}
						
						if(NTIWhelkonGrazers == 1){
						# Adds NT negative effect of whelks on grazers - same as above
						# The attack rates and the handling time of the grazers become functions of the whelk biomass
						dnt2 <- 5
						a35o <- (a35*x[1]/dnt2+a35)/(x[1]+1) 
						a36o <- (a36*x[1]/dnt2+a36)/(x[1]+1)
						a37o <- (a37*x[1]/dnt2+a37)/(x[1]+1)
						a38o <- (a38*x[1]/dnt2+a38)/(x[1]+1)
						h3o <- (h3*x[1]/dnt2+h3)/(x[1]+1)}
						else{
						a35o <- a35 
						a36o <- a36
						a37o <- a37
						a38o <- a38
						h3o <- h3}
						
						if(PosNTIMusselsOnAlgae == 1){
						# Mussels increase the carrying capacity of the algae A1 and A2
						dnt3 <- 5
						Ko1 <- (K1*x[4]*dnt3*3/2+K1)/(x[4]+1)
						Ko2 <- (K2*x[4]*dnt3+K2)/(x[4]+1)
						Ko3 <- K3
						Ko4 <- K4}
						else{
						Ko1 <- K1
						Ko2 <- K2
						Ko3 <- K3
						Ko4 <- K4}
						
						# Algae - Basal species
						dA1 <- r1*A1*(1-A1/Ko1) - w35*a35o*G*A1/((1+a35o*h3o*A1)*e) - w45*a45*M*A1/((1+a45*h4*A1)*e) # no compet for space with other algae
						dA2 <- r2*A2*(1-A2/Ko2) - w36*a36o*G*A2/((1+a36o*h3o*A2)*e) - w46*a46*M*A2/((1+a46*h4*A2)*e) - c56*A1 - c67*A3 - c68*A4 - c46*M # competition with other algae and competition with mussels
						dA3 <- r3*A3*(1-A3/Ko3) - w37*a37o*G*A3/((1+a37o*h3o*A3)*e) - w47*a47*M*A3/((1+a47*h4*A3)*e) - c57*A1 - c67*A2 - c78*A4 - c47*M # competition with other algae and competition with mussels
						dA4 <- r4*A4*(1-A4/Ko4) - w38*a38o*G*A4/((1+a38o*h3o*A4)*e) - w48*a48*M*A4/((1+a48*h4*A4)*e) - c58*A1 - c68*A2 - c78*A3 # competition with other algae
						
						# Intermediate species, grazers and mussels
						dG <- w35*a35o*G*A1/(1+a35o*h3o*A1) + w36*a36o*G*A2/(1+a36o*h3o*A2) + w37*a37o*G*A3/(1+a37o*h3o*A3) - x3*G - w13*a13o*W*G/((1+a13o*h1o*G)*e) - w23*a23*Cr*G/((1+a23*h2*G)*e)
						dM <- w45*a45*M*A1/(1+a45*h4*A1) + w46*a46*M*A2/(1+a46*h4*A2) + w47*a47*M*A3/(1+a47*h4*A3) - x4*M - w14*a14o*W*M/((1+a14o*h1o*M)*e) - w24*a24*Cr*M/((1+a24*h2*M)*e)
						# Top species, whelks and crabs
						dW <- w13*a13o*W*G/(1+a13o*h1o*G) + w14*a14o*W*M/(1+a14o*h1o*M) - x1*W  
						dCr <- w23*a23*Cr*G/(1+a23*h2*G) + w24*a24*Cr*M/(1+a24*h2*M) - x2*Cr  
					
						res<-c(dW,dCr,dG,dM,dA1,dA2,dA3,dA4)
						list(res)
						}
					)
				} # end of the function phmodel
		
		
		## Sets initial biomasses 
		y <- xstart <- c(W = 0.1, Cr = 0.1, G = 0.2, M = 0.2, A1 = 0.1, A2 = 0.1, A3 = 0.1, A4 = 0.1)
		#y <- xstart <- c(W = runif(1,0.1,0.9), Cr = runif(1,0.1,0.9), G = runif(1,0.1,0.9), M = runif(1,0.1,0.9), Ab = runif(1,0.1,0.9), Ag = runif(1,0.1,0.9), Are = runif(1,0.1,0.9))
		
		## Creates a time series
		output <-as.data.frame(ode(xstart, times, phmodel, parms,
										method = rkMethod("rk45dp7") #("ode45"); rkMethod("rk78f"); ("lsoda") 
										))
		output$W <- ifelse(output$W<10^(-30),0,output$W)
		output$Cr <- ifelse(output$Cr<10^(-30),0,output$Cr)
		output$G <- ifelse(output$G<10^(-30),0,output$G)
		output$M <- ifelse(output$M<10^(-30),0,output$M)
		output$A1 <- ifelse(output$A1<10^(-30),0,output$A1)
		output$A2 <- ifelse(output$A2<10^(-30),0,output$A2)
		output$A3 <- ifelse(output$A3<10^(-30),0,output$A3)
		output$A4 <- ifelse(output$A4<10^(-30),0,output$A4)
		
		{## Creates jpeg fig
		jpegname <- "Simu_WithNTI_1211"#paste("bmr_",j,"_series_",k,"single.jpeg",sep="")
		jpeg(file=jpegname, width = 980, height = 980, quality = 100)
		par(mfrow=c(3,3))
		plot(output$time, output$A1, type="n")
		lines(output$time, output$A1, col="green")
		mtext(side=3,line=0.5,"(a) A1", cex=1.5, adj = 0.1)
		
		plot(output$time, output$A2, type="n")
		lines(output$time, output$A2, col="green")
		mtext(side=3,line=0.5,"(b) A2", cex=1.5, adj = 0.1)
		
		plot(output$time, output$A3, type="n")
		lines(output$time, output$A3, col="green")
		mtext(side=3,line=0.5,"(c) A3", cex=1.5, adj = 0.1)
			
		plot(output$time, output$A4, type="n")
		lines(output$time, output$A4, col="green")
		mtext(side=3,line=0.5,"(c) A3", cex=1.5, adj = 0.1)
		
		plot(output$time, output$G, type="n")
		lines(output$time, output$G, col="#2D98EA")
		mtext(side=3,line=0.5,"(d) Grazers", cex=1.5, adj = 0.1)
		
		plot(output$time, output$M, type="n")
		lines(output$time, output$M, col="#2D98EA")
		mtext(side=3,line=0.5,"(e) Mussels", cex=1.5, adj = 0.1)
		
		plot(1,1)
		
		plot(output$time, output$W, type="n")
		lines(output$time, output$W, col="#E65732")
		mtext(side=3,line=0.5,"(f) Whelks", cex=1.5, adj = 0.1)
		
		plot(output$time, output$Cr, type="n")
		lines(output$time, output$Cr, col="#E65732")
		mtext(side=3,line=0.5,"(g) Crabs", cex=1.5, adj = 0.1)
		title(main="Trophic web", xlab = "time", ylab = "log10 biomass", outer=TRUE)
		
		dev.off()
		} # end part creating jpeg fig
		
#setwd("")
#write.table(x=mmb, file="minmax_single_basal.csv", sep = ",", dec=".", row.names = FALSE, col.names = TRUE)
	
#)) # end print time
	
