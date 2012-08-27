#marked AA sequence
extractnondom <- function(markedseq){
	tmppos   <- gregexpr("[[:alnum:]]+", markedseq)
	posbegin <- unlist(tmppos)
	posend   <- posbegin + attr(tmppos[[1]], "match.length") - 1
	res<-lapply(1:length(posbegin), function(i,markedseq, posbegin, posend){c(posbegin[i],posend[i],substr(markedseq, posbegin[i], posend[i]))}, markedseq, posbegin, posend)	
	return(res)
}
