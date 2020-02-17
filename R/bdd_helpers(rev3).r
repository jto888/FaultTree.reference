## rev3 code uses list objects in place of dataframes for Ite's
## the dot will also be removed from X.1 and X.0 labels

ite.form<-function(tag) {						
	list(node=tag,X1="1",X0="0")					
}						
						
tx<-function(ite)  {						
	paste0("<",ite$node,",", ite$X1, ",", ite$X0, ">")					
}						
					
tx2ite<-function(txt)  {						
	commas<-unlist(gregexpr(pattern =",",txt))					
	node<-substr(txt, start=2, stop=commas[1]-1)					
	last2<-substr(txt, start=nchar(txt)-1, stop=nchar(txt))					
	if(last2=="0>")  {					
## X0 is just "0", so we simply identify X1 in the middle						
		X1end<-nchar(txt)-3				
		X1<-substr(txt,start=commas[1]+1, stop=X1end)				
		X0<-"0"				
	}else{					
## X0 is an ite, but is X1 could be just "1"						
		if(commas[2]-commas[1]==2) {				
## simpler code than searching for ite_opens, because X1 must be "1"						
			X1<-"1"			
			X0start<-commas[2]+1			
			X0end<-nchar(txt)-1			
			X0<-substr(txt,start=X0start, stop=X0end)			
		}else{				
## both X1 and X0 must be ite's  so we can find the ite closure before X0 starts
			opens<-unlist(gregexpr(pattern ="<",txt))	
			closes<-unlist(gregexpr(pattern =">",txt))						
			ite_closes<-unlist(gregexpr(pattern =">,<",txt))
			for(icls in ite_closes)  {	
				this_ite_close<-which(closes==icls)
				opens2close<-length(which(opens<icls))-1
				if(this_ite_close==opens2close) break
			}			
			X1end<-icls			
			X1<-substr(txt,start=commas[1]+1, stop=X1end)			
			X0start<-X1end+2			
			X0end<-nchar(txt)-1			
			X0<-substr(txt,start=X0start, stop=X0end)			
		}				
	}					
	ite_obj<-list(node=node,X1=X1,X0=X0)					
	ite_obj 					
}						
