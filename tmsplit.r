tmsplit<-function(sequence,seqID=NULL, removefolder=TRUE){
	tmres<-grep(system('tmhmm', input=sequence, intern=TRUE), pattern="^#", invert=TRUE, value=TRUE)

	tmres<-reduceblanks(tmres)
	coord<-as.data.frame(do.call(rbind, lapply(strsplit(tmres, split="\t"), function(x){return(x[3:5])})),stringsAsFactors=FALSE)

	coord$V2<-as.integer(coord$V2)
	coord$V3<-as.integer(coord$V3)


	res<-do.call(rbind,(apply(coord,1, function(coord, sequence,seqID){
		tmp<-list()

		tmp$type<-coord[1]
		if(!is.null(seqID)){
			tmp$seqID<-seqID			
		}
		tmp$start<-as.integer(coord[2])
		tmp$stop<-as.integer(coord[3])
		tmp$subseq<-substr(sequence, coord[2],coord[3])
		tmp<-as.data.frame(tmp,stringsAsFactors=FALSE,row.names=NULL)
		return(tmp)
	},sequence,seqID
	)))		

	rownames(res)<-1:nrow(res)
	result<-list()
	if(sum(res$type=="outside")!=0)
		result[["outside"]]<-res[res$type=="outside",2:ncol(res)]
	if(sum(res$type=="TMhelix")!=0)
		result[["TMhelix"]]<-res[res$type=="TMhelix",2:ncol(res)]
	if(sum(res$type=="inside")!=0)
		result[["inside"]]<-res[res$type=="inside",2:ncol(res)]

	if(removefolder==TRUE)
		unlink(grep(dir(),pattern="^TMHMM_",value=TRUE, perl=TRUE),recursive=TRUE, force=TRUE)
	return(result)
}

sequence<-'MTRPTPRGTPEPVAVPRPRAPATPPARQCPAPYAAARAALRTAVARVTGSTLGARQTAAVRIGVAGTWGVYLLREWPHRAELYGPDSAWGWDLARRLVSGNGAFTALLWSRSEAWFTCVYVLALLVSFLLVLGWRTRTMSVLFALTLLSLQNRSVFVGDGGDNLLHLLALYLCLTRCGQVWSLDARRRALGRPDRAGPWLWGLLGTALLAARLSDRLTPGWTLTLAATWAALGAWWFLERHPLGETRALADVLGNLLHHAGVLIIMAETCLVYATAGWYKIQGSRWQDGTAVYYPLELDSFSPWPALSHALAAHGTLIALLTYGTVFAQVAFPFSLLNRRAKNVLIIVLVGEHLGIAFLLGLPFFSLAMLAADAVFLPTGFLRAVERGARRVTLRRTRPTTRRGPRRTVRTADAEAPVRPGTGSVGAAP'
