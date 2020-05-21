#' @export
cov_hybrid <- function(R0 = 2.0,
                       Rp = 1.5,
                       Rover = 0.9,
                       vecN=66000000*c(0.25,0.25,0.25,0.25),
                       vecI0=rep(5000/length(vecN),length(vecN)),
                       vecInfNess=rep(1,length(vecN)),
                       vecSusTy=rep(1,length(vecN)),
                       vecPS=rep(0.1,length(vecN)),
                       vecPM=rep(0.1,length(vecN)),
                       vecPA=rep(0.4,length(vecN)),
                       matCt=matrix(1/length(vecN),
                                     nrow=length(vecN),
                                     ncol=length(vecN)),
                       matCtClosure=matrix(1/length(vecN),
                                     nrow=length(vecN),
                                     ncol=length(vecN)),
                       scLim=c(99999,99999),
                       vecTcalc=seq(0,360,0.5),
                       vecRtrel=rep(1,length(vecTcalc)),
                       D_E=2,
                       D_I1=3,
                       D_I2=3,
                       D_HR=20,
                       D_HD=23,
                       D_ICU=rep(7,length(vecN)),
                       deterministic=FALSE,
                       nReals=2,
                       trig_pres=99999999,
                       icu_cap=0.9999,
                       plot=FALSE,
                       trickle=0,
                       sevBchange=FALSE,
                       trig_type = 1, 
                       trig_day=2000
                       ) { 
        
    ## Need to interpolate if using country R(t) profile
	
    ## Define housekeeping variables
    nAges <- length(vecN)
    nTimes <- length(vecTcalc)
    totalN <- sum(vecN)
    dt <- vecTcalc[2] - vecTcalc[1]
    vecOne <- rep(1,nAges)
    vecPF <- vecOne - vecPS - vecPM - vecPA
    vecPMcond <- vecPM / (vecOne - vecPS)
    vecPAcond <- vecPA / (vecOne - vecPS - vecPM)
    sevClassChars <- c("S","M","F","A")
    nSevClasses <- length(sevClassChars)
    matPsev <- data.frame(S=vecPS,M=vecPM,F=vecPF,A=vecPA)

    ## Define triggers 
    maxICU <- ceiling(icu_cap * totalN)

    ## Test for consistency in the input parameters
    if (min(vecPF) < 0) {stop("Age specific severity parameters are not consistent")}
    if (deterministic) {nReals <- 1}

    ## Declare items to be returned
    rtn_inf <- array(0, dim=c(nTimes,nAges,nReals))
    rtn_pop <- array(0, dim=c(nTimes,nAges,nReals))

	## Need to start from day -1 because cases start adding up already on day zero 
	## Initial number of cases is tehrefore pushed back to -1
	
	tps <- seq(from = 0, to = round(max(vecTcalc)))
	
	noTPts <- length(tps)
	rtn_daily <- array(0, dim=c(noTPts-1,nAges,nReals))
	rec_daily <- array(0, dim=c(noTPts-1,nAges,nReals))
	icu_daily <- array(0, dim=c(noTPts-1,nAges,nReals))
	dead_daily <- array(0, dim=c(noTPts-1,nAges,nReals))
	rt_daily <- rep(0, (noTPts-1))
	epsilon <- 1e-10
	  
    ## Create next generation matrix, assuming each severity level is an
    ## infectious type. Needs to be nested to 4 levels for infector and infectee
	# Calibrate Infectious Interval
	  # original approximation
	inf_int = D_I1 + D_I2
	# use all.equal(), so that extremely small differences will come back TRUE. For instance all.equal(1, 1+1e-8) returns TRUE, but all.equal(1, 1+1e-1) returns a string describing the difference.
	check_vec = all.equal(c(dt, D_I1, D_I2), c(1, 3, 3))
	if (is.logical(check_vec)) {
	  # using our original parameters, the effective numeric mean infectious interval is 7.06 (See Evernote 05-03-2020)
	  inf_int = 7.06
	} else {
	  check_vec = all.equal(c(dt, D_I1, D_I2), c(1, 1.71, 1.71))
	  if (is.logical(check_vec)) {
	    # using the proposed parameters, the effective numeric mean infectious interval is 4.52
	    inf_int = 4.52
	  }
	}
    ngm = matrix(ncol=nAges*nSevClasses,nrow=nAges*nSevClasses)
    for (i in 1:nSevClasses) {
        tor <- sevClassChars[i]
        for (j in 1:nAges) {
            for (k in 1:nSevClasses) {
                tee <- sevClassChars[k] 
                for (l in 1:nAges) {
                    ngm_col <- (i-1)*nAges + j
                    ngm_row <- (k-1)*nAges + l

                    ## Not entirely sure why this doesn't have a term for the
                    ## relative size of each population, but will worry about that
                    ## later! vecN[l] / totalN. Seems to pass the tests.
                    ngm[ngm_row,ngm_col] <-
                        matCt[j,l] * (inf_int) * matPsev[l,k] *
                        vecInfNess[j] * vecSusTy[l]

                    ## Close all the loops
                 }
            }
        }
    }

    ## Set the value of beta using the NGM matrix
    R0_ngm <- Re((eigen(ngm))$values[1])
    beta <- R0 / R0_ngm
    beta_check <- R0 / (D_I1 + D_I2)
    del <- beta - beta * Rp / R0
    ## Initiate the realisation loop
    for (i in 1:nReals) {
        #cat('Calculating Realization: ', i, '\n')
        ## Initiate the state variables
        S <- round(vecN - vecI0)
        E <- rep(0,nAges)
        I_1M <- vecI0
        I_2M <- rep(0,nAges)
        I_1F <- rep(0,nAges)
        I_2F <- rep(0,nAges)
        I_1A <- rep(0,nAges)
        I_2A <- rep(0,nAges)
        I_1S <- rep(0,nAges)
        I_2S <- rep(0,nAges)
        R <- rep(0,nAges)
        ICU <- rep(0,nAges)

        ## Initiate constant hazards
        vecHazExE = rep(1/D_E,nAges)
        vecHazExI_1M = rep(1/D_I1,nAges)
        vecHazExI_2M = rep(1/D_I2,nAges)
        vecHazExI_1F = rep(1/D_I1,nAges)
        vecHazExI_2F = rep(1/D_I2,nAges)
        vecHazExI_1A = rep(1/D_I1,nAges)
        vecHazExI_2A = rep(1/D_I2,nAges)
        vecHazExI_1S = rep(1/D_I1,nAges)
        vecHazExI_2S = rep(1/D_I2,nAges)
        vecHazExICU = 1/D_ICU

        ## Initiate constant probs
        vecProbExE <- 1 - exp(-dt * vecHazExE)
        vecProbExI_1M <- 1 - exp(-dt * vecHazExI_1M)
        vecProbExI_2M <- 1 - exp(-dt * vecHazExI_2M)
        vecProbExI_1F <- 1 - exp(-dt * vecHazExI_1F)
        vecProbExI_2F <- 1 - exp(-dt * vecHazExI_2F)
        vecProbExI_1S <- 1 - exp(-dt * vecHazExI_1S)
        vecProbExI_2S <- 1 - exp(-dt * vecHazExI_2S)
        vecProbExI_1A <- 1 - exp(-dt * vecHazExI_1A)
        vecProbExI_2A <- 1 - exp(-dt * vecHazExI_2A)
        vecProbExICU <- 1 - exp(-dt * vecHazExICU)
        
        ## Initiate the return data structures
        
        rtn_pop[1,,i] <- sum(
            S,E,
            I_1M,I_2M,I_1F,I_2F,
            I_1S,I_2S,I_1A,I_2A,
            ICU,R)

        ## Initiate some non-state variables that are needed
        cumICU <- 0
        sevInICU <- rep(0,nAges)
        sevOutICU <- rep(0,nAges)
        
        ## initiate the output arrays 
	          
        ## Initiate the time loop
        j <- 2
		## initiate for daily output
        ind_t <- 2
        t_next <- tps[ind_t]
		ts <- vecTcalc[nTimes]
		
		
        while (j <= nTimes) {
        	t_cur <- vecTcalc[j]
        	
            ## Adjust for triggers
            if (cumICU > trig_pres && trig_type == 1) {
            	ts <- min(ts, t_cur)
                if ((sum(ICU) > maxICU) && sevBchange) {
                    beta_tmp <- beta * Rover / R0
                } else {
                    #beta_tmp <- beta * Rp / R0   ## Commented this and introduced the smooth change
					beta_tmp <- beta - del*(tanh((t_cur-ts)/5))
                }
            } else if (t_cur > trig_day && trig_type == 2){
            		ts <- min(ts, t_cur)	
            		beta_tmp <- beta - del*(tanh((t_cur-ts)/5))            		
            } else if (trig_type == 3) {
            	beta_tmp <- vecRtrel[j] / R0_ngm
            }
            else {
                beta_tmp <- beta
            }

            ## Set current beta
            beta_cur <-  beta_tmp
            

            ## Set mixing matrix
            
            if ((t_cur < scLim[1]) || (t_cur >= scLim[2])) {
                matCtTmp <- matCt
            } else {
                matCtTmp <- matCtClosure
            }
            
            ## Calculate variable hazards
            vecFoi <-  beta_cur * vecSusTy * (
                ((I_1M * vecInfNess) %*% matCtTmp) / vecN +
                ((I_2M * vecInfNess) %*% matCtTmp) / vecN +
                ((I_1F * vecInfNess) %*% matCtTmp) / vecN +
                ((I_2F * vecInfNess) %*% matCtTmp) / vecN +
                ((I_1S * vecInfNess) %*% matCtTmp) / vecN +
                ((I_2S * vecInfNess) %*% matCtTmp) / vecN +
                ((I_1A * vecInfNess) %*% matCtTmp) / vecN +
                ((I_2A * vecInfNess) %*% matCtTmp) / vecN
            ) + trickle / (totalN * 7 * dt * nAges)
            
            ## Calculate variable probabilites
            pVecFoi <- 1 - exp(-dt*vecFoi)

            # Either draw random numbers or calculate averages
            # Currently just below here
            # if (deterministic) {
            	
                # noInf <- S * pVecFoi
                # noExE <- E * vecProbExE
                # noEntI1S <- noExE * vecPS
                # noEntI1M <- noExE * vecPM
                # noEntI1A <- noExE * vecPA
                # noEntI1F <- noExE * (vecOne - vecPM - vecPS - vecPA)
                # noExI_1M <- I_1M * vecProbExI_1M
                # noExI_2M <- I_2M * vecProbExI_2M
                # noExI_1F <- I_1F * vecProbExI_1F
                # noExI_2F <- I_2F * vecProbExI_2F
                # noExI_1S <- I_1S * vecProbExI_1S
                # noExI_2S <- I_2S * vecProbExI_2S
                # noExI_1A <- I_1A * vecProbExI_1A
                # noExI_2A <- I_2A * vecProbExI_2A
                # noExICU <- ICU * vecProbExICU
            # } else {
                noInf <- rbinom(nAges,S,pVecFoi)
                noExE <- rbinom(nAges,E,vecProbExE)
                noEntI1S <- rbinom(nAges, noExE, vecPS)
                noEntI1M <- rbinom(nAges, noExE - noEntI1S, vecPMcond)
                noEntI1A <- rbinom(nAges, noExE - noEntI1S - noEntI1M, vecPAcond)
                noEntI1F <- noExE - noEntI1S - noEntI1M - noEntI1A                        
                noExI_1M <- rbinom(nAges,I_1M,vecProbExI_1M)
                noExI_2M <- rbinom(nAges,I_2M,vecProbExI_2M)
                noExI_1F <- rbinom(nAges,I_1F,vecProbExI_1F)
                noExI_2F <- rbinom(nAges,I_2F,vecProbExI_2F)
                noExI_1S <- rbinom(nAges,I_1S,vecProbExI_1S)
                noExI_2S <- rbinom(nAges,I_2S,vecProbExI_2S)
                noExI_1A <- rbinom(nAges,I_1A,vecProbExI_1A)
                noExI_2A <- rbinom(nAges,I_2A,vecProbExI_2A)
                noExICU <- rbinom(nAges,ICU,vecProbExICU)
            # }

            # zero before updating properly
			noDead <- 0
            ## Count severe people making it into ICU and those not making
            ## it into ICU
            capICU <- maxICU - sum(ICU)
            if (capICU > sum(ICU)) {
                    sevInICU <- sevInICU + noExI_2S
                    noDead <- noExI_2S * 0.5
                } else if (capICU <= 0) {
                    sevOutICU <- sevOutICU + noExI_2S
                    noDead <- noExI_2S 
                } else {
                    ## TODO age prioritization here
                    sevOutICU <- sevOutICU + noExI_2S
                    noDead <- noExI_2S 
                }

             #<- sevOutICU + 0.5 * sevInICU # 
            
            ## Update the state variables
            ## Problems are probably here
            S <- S - noInf
            E <- E + noInf - noExE
            I_1M <- I_1M + noEntI1M - noExI_1M
            I_2M <- I_2M + noExI_1M - noExI_2M
            I_1F <- I_1F + noEntI1F - noExI_1F
            I_2F <- I_2F + noExI_1F - noExI_2F
            I_1S <- I_1S + noEntI1S - noExI_1S
            I_2S <- I_2S + noExI_1S - noExI_2S
            I_1A <- I_1A + noEntI1A - noExI_1A
            I_2A <- I_2A + noExI_1A - noExI_2A
            ICU <- ICU + noExI_2S - noExICU
            R <- R + noExI_2M + noExI_2F + noExICU + noExI_2A

            ## Record the other output variables
            rtn_inf[j,,i] <- noInf
            rtn_pop[j,,i] <- sum(S,E,
                                 I_1M,I_2M,I_1F,I_2F,
                                 I_1S,I_2S,I_1A,I_2A,
                                 ICU,R)
            cumICU <- cumICU + sum(noExI_2S)

            rtn_daily[ind_t-1,,i] <- rtn_daily[ind_t-1,,i] + noInf
            rec_daily[ind_t-1,,i] <- rec_daily[ind_t-1,,i] + noExI_2M + noExI_2F + noExICU + noExI_2A
            icu_daily[ind_t-1,,i] <- icu_daily[ind_t-1,,i] + noExI_2S
           	rt_daily[ind_t-1]     <- beta_cur * R0_ngm 
			dead_daily[ind_t-1,,i] <-  dead_daily[ind_t-1,,i] + noDead  # See calculated above lines 257, 260, 264
            ## Close off the loop
            j <- j+1

            if (t_cur > (t_next - epsilon)) {
            	rt_daily[ind_t-1] <- round(rt_daily[ind_t-1], digits = 2)
            	ind_t <- ind_t + 1
            	t_next <- tps[ind_t]
            	         		        	
            }
        }

        for (k in 1:nAges) dead_daily[,k,i] <- cumsum(dead_daily[,k,i])

    }

    ## Make derived outputs
    rtn_inf_cum <- array(dim=c(nTimes,nAges,nReals))
    rtn_inf_cum[1,,] <- rtn_inf[1,,]
    for (i in 2:nTimes) {
        rtn_inf_cum[i,,] <- rtn_inf_cum[i-1,,] + rtn_inf[i,,]
    }

 	## smooth for plotting
 	
 	#rt_daily <- mav(rt_daily, n = 7)
	#rt_daily <- round(rt_daily, digits = 2)

    ## Return the outputs, expand as necessary
    list(inf=rtn_inf,
         cinf=rtn_inf_cum,
         sevIn=sum(sevInICU),
         sevOut=sum(sevOutICU),
         t=vecTcalc,
         pop=vecN,
         rtn_daily=rtn_daily,
         rec_daily=rec_daily,
         icu_daily=icu_daily,
         dead_daily=dead_daily,
         rt_daily=rt_daily,
         tps=tps)

}
