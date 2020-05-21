
trim.data.in <- function(longvec, change = 10) {
	noobs <- length(longvec)
	first <- 1
	last <- noobs
	
	while (abs(longvec[last] - longvec[(last - 1)]) < change) {
		last <- last - 1
	}

	last <- last + 7
	last <- min(last, noobs)
	
	last
}

generate_rt <- function(list_rt, nrt = 3, xvals = 0:365, vecTcalc = 0:365) {
	list_vecRtrel = list()
	for (ii in 1:nrt) {
		myrt <- list_rt[[ii]]
		n = length(myrt)
		mydays = 1:length(n)
		rt_last = myrt[n]
		vecRtrel = rep(rt_last, length(xvals)) 
		vecRtrel[1:n] = myrt
		new <- approx(x = xvals, y = vecRtrel, xout = vecTcalc)$y
		#new <- rep(1.4, length(vecTcalc))
		list_vecRtrel[[ii]] = new
	}
	list_vecRtrel
}

unformat <- function(x) as.numeric(gsub(",", "", x))

mav <- function(x, n=3) {
	stats::filter(x,rep(1/n,n), sides=2)}

process_cov_hybrid_output <- function(nAges = 9, day1 = NULL, y = NULL) {
	inf = y$inf # infected
	tps = y$tps
	tps = tps[2:length(tps)]


	rtn <- y$rtn_daily
	#rec <- y$rec_daily
	icu <- y$icu_daily
	ded <- y$dead_daily

	dayf <- day1 + length(tps) - 1
	dates <- seq(from = as.Date(day1), to = as.Date(dayf), by = "1 month")
	ddates <- seq(from = as.Date(day1), to = as.Date(dayf), by = "1 day")


	mrtn <- 0 * rtn[, , 1]
	for (j in 1:length(tps)) {
		for (k in 1:nAges) mrtn[j, k] <- round(median(rtn[j, k, ]))
	}

	lowrtn <- 0 * rtn[, , 1]
	upprtn <- 0 * rtn[, , 1]
	
	for (j in 1:length(tps)) {
		for (k in 1:nAges) lowrtn[j, k] <- round(quantile(rtn[j, k, ], probs = c(0.025)))
		for (k in 1:nAges) upprtn[j, k] <- round(quantile(rtn[j, k, ], probs = c(0.975)))
	}	
	
	micu <- 0 * icu[, , 1]
	for (j in 1:length(tps)) {
		for (k in 1:nAges) micu[j, k] <- round(median(icu[j, k, ]))
	}

	lowicu <- 0 * icu[, , 1]
	uppicu <- 0 * icu[, , 1]
	
	for (j in 1:length(tps)) {
		for (k in 1:nAges) lowicu[j, k] <- round(quantile(icu[j, k, ], probs = c(0.025)))
		for (k in 1:nAges) uppicu[j, k] <- round(quantile(icu[j, k, ], probs = c(0.975)))
	}	
	mded <- 0 * ded[, , 1]

	for (j in 1:length(tps)) {
		for (k in 1:nAges) mded[j, k] <- round(median(ded[j, k, ]))
	}
	lowded <- 0 * ded[, , 1]
	uppded <- 0 * ded[, , 1]
	
	for (j in 1:length(tps)) {
		for (k in 1:nAges) lowded[j, k] <- round(quantile(ded[j, k, ], probs = c(0.025)))
		for (k in 1:nAges) uppded[j, k] <- round(quantile(ded[j, k, ], probs = c(0.975)))
	}	
		
	sdrtn <- 0 * rtn[, , 1]
	for (j in 1:length(tps)) {
		for (k in 1:nAges) sdrtn[j, k] <- round(sd(rtn[j, k, ]))
	}

	# sdrec <- 0 * rec[, , 1]
	# for (j in 1:length(tps)) {
		# for (k in 1:nAges) sdrec[j, k] <- round(sd(rec[j, k, ]))
	# }

	sdicu <- 0 * icu[, , 1]
	for (j in 1:length(tps)) {
		for (k in 1:nAges) sdicu[j, k] <- round(sd(icu[j, k, ]))
	}

	sdded <- 0 * ded[, , 1]
	for (j in 1:length(tps)) {
		for (k in 1:nAges) sdded[j, k] <- round(sd(ded[j, k, ]))
	}
	
	out = list(mrtn=mrtn, lowrtn = lowrtn, upprtn = upprtn, micu=micu, lowicu = lowicu, uppicu = uppicu, lowded = lowded, uppded = uppded, mded = mded, ddates=ddates, sdrtn=sdrtn, sdicu=sdicu, sdded = sdded)
	
	return(out)
}

process_cov_hybrid_output_total <- function(nAges = 4, day1 = Sys.Date(), y = NULL, cum_conf_death_df = NULL) {
	inf = y$inf # infected
	tps = y$tps
	tps = tps[2:length(tps)]


	rtn <- y$rtn_daily
	icu <- y$icu_daily
	ded <- y$dead_daily

	dayf <- day1 + length(tps) - 1
	dates <- seq(from = as.Date(day1), to = as.Date(dayf), by = "1 month")
	ddates <- seq(from = as.Date(day1), to = as.Date(dayf), by = "1 day")

	nReals <- dim(rtn)[3]
	all.rtn <- 0 * rtn[, 1, ]
	all.icu <- 0 * icu[, 1, ]
	all.ded <- 0 * ded[, 1, ]
	
	for (j in 1:length(tps)) {
		for (k in 1:nReals) {
			all.rtn[j,k] <- sum(rtn[j, 1:nAges, k])
			all.icu[j,k] <- sum(icu[j, 1:nAges, k])
			all.ded[j,k] <- sum(ded[j, 1:nAges, k])
		}
	}
	
	#cumulative infected 
	cinf <- 0 * all.rtn
	cinf[1, ] <- all.rtn[1, ]
	mcinf <- rep(0, length(tps))
	
	for (j in 2:length(tps)) {
		for (k in 1:nReals) {
			cinf[j, k] <- cinf[j-1, k] + all.rtn[j,k]
		}
		mcinf[j] <- round(median(cinf[j, ]))
	}	
	
	mrtn   <- 0 * rtn[, 1, 1]
	lowrtn <- 0 * rtn[, 1, 1]
	upprtn <- 0 * rtn[, 1, 1]
	
	for (j in 1:length(tps)) {
		   mrtn[j] <- round(median(all.rtn[j,]))
		 lowrtn[j] <- round(quantile(all.rtn[j, ], probs = c(0.025)))
		 upprtn[j] <- round(quantile(all.rtn[j, ], probs = c(0.975)))
	}	
	
	micu   <- 0 * icu[, 1, 1]
	lowicu <- 0 * icu[, 1, 1]
	uppicu <- 0 * icu[, 1, 1]
	
	for (j in 1:length(tps)) {
		   micu[j] <- round(median(all.icu[j,]))
		 lowicu[j] <- round(quantile(all.icu[j, ], probs = c(0.025)))
		 uppicu[j] <- round(quantile(all.icu[j, ], probs = c(0.975)))
	}	
	
	
	mded   <- 0 * ded[, 1, 1]
	lowded <- 0 * ded[, 1, 1]
	uppded <- 0 * ded[, 1, 1]
	
	for (j in 1:length(tps)) {
		   mded[j] <- round(median(all.ded[j,]))
		 lowded[j] <- round(quantile(all.ded[j, ], probs = c(0.025)))
		 uppded[j] <- round(quantile(all.ded[j, ], probs = c(0.975)))
	}	
		
	sdrtn <- 0 * rtn[, 1, 1]
	for (j in 1:length(tps)) {
		sdrtn[j] <- round(sd(all.rtn[j,]))
	}

	sdicu <- 0 * icu[, 1, 1]
	for (j in 1:length(tps)) {
		sdicu[j] <- round(sd(all.icu[j, ]))
	}

	sdded <- 0 * ded[, 1, 1]
	for (j in 1:length(tps)) {
		sdded[j] <- round(sd(all.ded[j,]))
	}

	out = list(mrtn=mrtn, lowrtn = lowrtn, upprtn = upprtn, micu=micu, lowicu = lowicu, uppicu = uppicu, lowded = lowded, uppded = uppded, mded = mded, ddates=ddates, sdrtn=sdrtn, sdicu=sdicu, sdded = sdded, mcinf = mcinf)

	return(out)
}

make_table <- function(df1 = NULL, out1 = NULL, all.out1 = NULL, df2 = NULL, out2 = NULL, all.out2 = NULL, nmax = NULL, ageNames = " ", obs_death = NULL) {
	
	nAges1 = length(ageNames)
	nAges  = nAges1 - 1
	
	data = data.frame(Date = character(), med = integer(), upp = integer(), low = integer(), Age = character(), Observable = character(), model = character(), Rt = numeric(), stringsAsFactors = FALSE)

	model_names = c('Fixed', 'Modulated')
	
	icount = 0
	
	## Start with observed data 
	nd = dim(obs_death)[1]
	for (ii in 1:nd) {
		icount = icount + 1
		data[icount, 'Date'] = format(as.Date(obs_death$dates[ii], origin = "1970-01-01")) 
		data[icount, c("med", 'upp', "low")] = obs_death$cum_death[ii] 
		data[icount, "Observable"] = 'Repored Cum. Death'
		data[icount, "model"] = 'Observed'
		data[icount, "Age"]   = ageNames[nAges1]
		data[icount, "Rt"] = 'NA'
	}
	
	## Fixed runs 
		
	for (ii in 1:nmax) {

		for (jj in 1:nAges) {

			icount = icount + 1
			data[icount, "Date"] = format(as.Date(all.out1$ddates[ii], origin = "1970-01-01"))
			data[icount, "med"] = out1$mrtn[ii, jj]
			data[icount, "upp"] = out1$upprtn[ii, jj]
			data[icount, "low"] = out1$lowrtn[ii, jj]
			data[icount, "Age"] = ageNames[jj]
			data[icount, "Observable"] = "Daily Incidence"
			data[icount, "model"] = model_names[1]
			data[icount, "Rt"] = df1$Rt[ii]
		}
		icount = icount + 1
		data[icount, "Date"] = format(as.Date(all.out1$ddates[ii], origin = "1970-01-01"))
		data[icount, "med"] = df1$rtn[ii]
		data[icount, "upp"] = sum(out1$upprtn[ii, ])
		data[icount, "low"] = sum(out1$lowrtn[ii, ])
		data[icount, "Age"] = ageNames[nAges1]
		data[icount, "Observable"] = "Daily Incidence"
		data[icount, "model"] = model_names[1]
		data[icount, "Rt"] = df1$Rt[ii]
		for (jj in 1:nAges) {
			icount = icount + 1
			data[icount, "Date"] = format(as.Date(all.out1$ddates[ii], origin = "1970-01-01"))
			data[icount, "med"] = out1$micu[ii, jj]
			data[icount, "upp"] = out1$uppicu[ii, jj]
			data[icount, "low"] = out1$lowicu[ii, jj]
			data[icount, "Age"] = ageNames[jj]
			data[icount, "Observable"] = "Daily ICU"
			data[icount, "model"] = model_names[1]
			data[icount, "Rt"] = df1$Rt[ii]
		}
		icount = icount + 1
		data[icount, "Date"] = format(as.Date(all.out1$ddates[ii], origin = "1970-01-01"))
		data[icount, "med"] = df1$icu[ii]
		data[icount, "upp"] = sum(out1$uppicu[ii, ])
		data[icount, "low"] = sum(out1$lowicu[ii, ])
		data[icount, "Age"] = ageNames[nAges1]
		data[icount, "Observable"] = "Daily ICU"
		data[icount, "model"] = model_names[1]
		data[icount, "Rt"] = df1$Rt[ii]
		
		for (jj in 1:nAges) {
			icount = icount + 1
			data[icount, "Date"] = format(as.Date(all.out1$ddates[ii], origin = "1970-01-01"))
			data[icount, "med"] = out1$mded[ii, jj]
			data[icount, "upp"] = out1$uppded[ii, jj]
			data[icount, "low"] = out1$lowded[ii, jj]
			data[icount, "Age"] = ageNames[jj]
			data[icount, "Observable"] = "Cum. Deaths"
			data[icount, "model"] = model_names[1]
			data[icount, "Rt"] = df1$Rt[ii]
		}
		icount = icount + 1
		data[icount, "Date"] = format(as.Date(all.out1$ddates[ii], origin = "1970-01-01"))
		data[icount, "med"] = df1$ded[ii]
		data[icount, "upp"] = sum(out1$uppded[ii, ])
		data[icount, "low"] = sum(out1$lowded[ii, ])
		data[icount, "Age"] = ageNames[nAges1]
		data[icount, "Observable"] = "Cum. Deaths"
		data[icount, "model"] = model_names[1]
		data[icount, "Rt"] = df1$Rt[ii]
	}

	## Repeat for the run with modulated R(t) 
	
	for (ii in 1:nmax) {

		for (jj in 1:nAges) {

			icount = icount + 1
			data[icount, "Date"] = format(as.Date(all.out2$ddates[ii], origin = "1970-01-01"))
			data[icount, "med"] = out2$mrtn[ii, jj]
			data[icount, "upp"] = out2$upprtn[ii, jj]
			data[icount, "low"] = out2$lowrtn[ii, jj]
			data[icount, "Age"] = ageNames[jj]
			data[icount, "Observable"] = "Daily Incidence"
			data[icount, "model"] = model_names[2]
			data[icount, "Rt"] = df2$Rt[ii]
		}
		icount = icount + 1
		data[icount, "Date"] = format(as.Date(all.out2$ddates[ii], origin = "1970-01-01"))
		data[icount, "med"] = df2$rtn[ii]
		data[icount, "upp"] = sum(out2$upprtn[ii, ])
		data[icount, "low"] = sum(out2$lowrtn[ii, ])
		data[icount, "Age"] = ageNames[nAges1]
		data[icount, "Observable"] = "Daily Incidence"
		data[icount, "model"] = model_names[2]
		data[icount, "Rt"] = df2$Rt[ii]
		for (jj in 1:nAges) {
			icount = icount + 1
			data[icount, "Date"] = format(as.Date(all.out2$ddates[ii], origin = "1970-01-01"))
			data[icount, "med"] = out2$micu[ii, jj]
			data[icount, "upp"] = out2$uppicu[ii, jj]
			data[icount, "low"] = out2$lowicu[ii, jj]
			data[icount, "Age"] = ageNames[jj]
			data[icount, "Observable"] = "Daily ICU"
			data[icount, "model"] = model_names[2]
			data[icount, "Rt"] = df2$Rt[ii]
		}
		icount = icount + 1
		data[icount, "Date"] = format(as.Date(all.out2$ddates[ii], origin = "1970-01-01"))
		data[icount, "med"] = df2$icu[ii]
		data[icount, "upp"] = sum(out2$uppicu[ii, ])
		data[icount, "low"] = sum(out2$lowicu[ii, ])
		data[icount, "Age"] = ageNames[nAges1]
		data[icount, "Observable"] = "Daily ICU"
		data[icount, "model"] = model_names[2]
		data[icount, "Rt"] = df2$Rt[ii]
		for (jj in 1:nAges) {
			icount = icount + 1
			data[icount, "Date"] = format(as.Date(all.out2$ddates[ii], origin = "1970-01-01"))
			data[icount, "med"] = out2$mded[ii, jj]
			data[icount, "upp"] = out2$uppded[ii, jj]
			data[icount, "low"] = out2$lowded[ii, jj]
			data[icount, "Age"] = ageNames[jj]
			data[icount, "Observable"] = "Cum. Deaths"
			data[icount, "model"] = model_names[2]
			data[icount, "Rt"] = df2$Rt[ii]
		}
		icount = icount + 1
		data[icount, "Date"] = format(as.Date(all.out2$ddates[ii], origin = "1970-01-01"))
		data[icount, "med"] = df2$ded[ii]
		data[icount, "upp"] = sum(out2$uppded[ii, ])
		data[icount, "low"] = sum(out2$lowded[ii, ])
		data[icount, "Age"] = ageNames[nAges1]
		data[icount, "Observable"] = "Cum. Deaths"
		data[icount, "model"] = model_names[2]
		data[icount, "Rt"] = df2$Rt[ii]
	}
	
	
	return(data)

}
head_df <- function( df= NULL) {
	print(head(df))
}


# return n_profiles of R(t) with individual intervention parameters drawn from their gaussian distributions
build_sample_Rt_profiles <- function(start_date=as.Date("2020-03-01"), end_date=as.Date("2020-12-31"), radio_control=3, intervs_df=NULL, n_profiles=1, IC_dists=NULL) {
  # generate date vector
  date_vec = seq.Date(from=start_date, to=end_date, by=1)
  
  Rt = matrix(data=1., nrow=length(date_vec), ncol=n_profiles)
  if (radio_control %in% c(1,2)) {
    # country profile or trigger modulation, do nothing.
  } else {
    if (nrow(intervs_df)>0) {
      # order interventions dataframe by start date
      intervs_df = intervs_df[order(intervs_df$start_date, intervs_df$interv_name), ]
      ii = 1
      interv = intervs_df$interv_name[ii]
      IC_row = IC_dists$intervention==interv
      value_mean = IC_dists[IC_row, paste0("first_mean")]/100
      value_sd = IC_dists[IC_row, paste0("first_sd")]/100
      vals = rnorm(n=n_profiles, mean=value_mean, sd=value_sd)
      vals[vals<0] = 0
      if (any(is.na(vals))) browser()
      # index into date_vec
      dates_effected = date_vec<=intervs_df$end_date[ii] & 
        date_vec>=intervs_df$start_date[ii]
      for (jj in 1:n_profiles) {
        Rt[dates_effected, jj] = Rt[dates_effected, jj]*(1.-vals[jj])
      }
      
      for (ii in 1:nrow(intervs_df)) {
        interv = intervs_df$interv_name[ii]
        IC_row = IC_dists$intervention==interv
        value_mean = IC_dists[IC_row, paste0("after_mean")]/100
        value_sd = IC_dists[IC_row, paste0("after_sd")]/100
        vals = rnorm(n=n_profiles, mean=value_mean, sd=value_sd)
        vals[vals<0] = 0
        if (any(is.na(vals))) browser()
        # index into date_vec
        dates_effected = date_vec<=intervs_df$end_date[ii] & 
          date_vec>=intervs_df$start_date[ii]
        for (jj in 1:n_profiles) {
          Rt[dates_effected, jj] = Rt[dates_effected, jj]*(1.-vals[jj])
        }
      }
    }
  }
  
  output = list(date=date_vec, Rt=Rt)
  return(output)
}


# return the mean R(t) profile
build_mean_Rt <- function(start_date=as.Date("2020-03-01"), end_date=as.Date("2020-12-31"), radio_control=3, intervs_df=NULL, IC_dists=NULL) {
  # generate date vector
  date_vec = seq.Date(from=start_date, to=end_date, by=1)
  
  Rt = matrix(data=1., nrow=length(date_vec), ncol=1)
  if (radio_control %in% c(1,2)) {
    # country profile or trigger modulation, do nothing.
  } else {
    if (nrow(intervs_df)>0) {
      # order interventions dataframe by start date
      intervs_df = intervs_df[order(intervs_df$start_date, intervs_df$interv_name), ]
      ii = 1
      interv = intervs_df$interv_name[ii]
      IC_row = IC_dists$intervention==interv
      vals = IC_dists[IC_row, paste0("first_mean")]/100
      # index into date_vec
      dates_effected = date_vec<=intervs_df$end_date[ii] & 
        date_vec>=intervs_df$start_date[ii]
      for (jj in 1:1) {
        Rt[dates_effected, jj] = Rt[dates_effected, jj]*(1.-vals[jj])
      }
      
      for (ii in 1:nrow(intervs_df)) {
        interv = intervs_df$interv_name[ii]
        IC_row = IC_dists$intervention==interv
        vals = IC_dists[IC_row, paste0("after_mean")]/100
        # index into date_vec
        dates_effected = date_vec<=intervs_df$end_date[ii] & 
          date_vec>=intervs_df$start_date[ii]
        for (jj in 1:1) {
          Rt[dates_effected, jj] = Rt[dates_effected, jj]*(1.-vals[jj])
        }
      }
    }
  }
  
  output = list(date=date_vec, Rt=Rt)
  return(output)
}


gen_country_map <- function(iso3="AUS", data_date=NULL, map_date=NULL, ggscale=NULL, raster_map=NULL) {
  
  p = ggplot() +
    layer_spatial(raster_map) +
    ggscale
  
  return(p)
}





