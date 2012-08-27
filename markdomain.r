#AA sequence
#Domain coordinates start, stop
markdomain<-function(sequence, domcrd){
	substr(sequence, domcrd[1], domcrd[2])<-paste(rep(".", domcrd[2] - domcrd[1] + 1), sep = "", collapse = "")
	return(sequence)
}
