rankhospital <- function(state, outcome, num="best"){

	data <- read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings = "Not Available", stringsAsFactors = FALSE)

	outcome <- tolower(as.character(outcome))
    	state <- as.character(state)
	
	if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
	else if(!(state %in% dimnames(table(data[,7]))[[1]])) stop("invalid state")

	if(outcome=="heart attack"){
		df <- data.frame(HospitalName=data[,2],TheState=data[,7],Rate=data[,11])
		d <- na.omit(df)
		s <- split(d, d$TheState)
		nd <- s[[state]]
		nnd <- data.frame(HospitalName=nd$HospitalName,TheState=nd$TheState,Rate=as.numeric(as.character(nd$Rate)))
		dfvf <- nnd[order(nnd$Rate, nnd$HospitalName),]
		dfvf <- data.frame(dfvf, Rank=c(1:nrow(nd)))
		if (num=="best") num<-1
		else if (num=="worst") num<-nrow(nd)
		r <- dfvf$HospitalName[dfvf$Rank==num]}
	else if(outcome=="heart failure"){
		df <- data.frame(HospitalName=data[,2],TheState=data[,7],Rate=data[,17])
		d <- na.omit(df)
		s <- split(d, d$TheState)
		nd <- s[[state]]
		nnd <- data.frame(HospitalName=nd$HospitalName,TheState=nd$TheState,Rate=as.numeric(as.character(nd$Rate)))
		dfvf <- nnd[order(nnd$Rate, nnd$HospitalName),]
		dfvf <- data.frame(dfvf, Rank=c(1:nrow(nd)))
		if (num=="best") num<-1
		else if (num=="worst") num<-nrow(nd)
		r <- dfvf$HospitalName[dfvf$Rank==num]}
	else if (outcome=="pneumonia"){
		df <- data.frame(HospitalName=data[,2],TheState=data[,7],Rate=data[,23])
		d <- na.omit(df)
		s <- split(d, d$TheState)
		nd <- s[[state]]
		nnd <- data.frame(HospitalName=nd$HospitalName,TheState=nd$TheState,Rate=as.numeric(as.character(nd$Rate)))
		dfvf <- nnd[order(nnd$Rate, nnd$HospitalName),]
		dfvf <- data.frame(dfvf, Rank=c(1:nrow(nd)))
		if (num=="best") num<-1
		else if (num=="worst") num<-nrow(nd)
		r <- dfvf$HospitalName[dfvf$Rank==num]}

	r
}

		