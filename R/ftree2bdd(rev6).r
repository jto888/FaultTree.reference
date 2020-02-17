ftree2bdd<-function(ftree,ft_node=1)  {	
## Rev6 changes ite objects (Gline, Fline, Rline, etc) to lists
## Note: Code to error trap poorly formed ftree objects is not used here.
## Such protective code exists in ftree2cpp.r
				
# set the ftree object in global environment					
	FT<<-ftree				
					
# initialize the table to be used to store operations that need not be repeated					
	T1<<-c(operation=" ", result=" ")				
					
	BDD<-tx(FT2BDD( ft_node))				
	BDD				
}					
					
					
					
FT2BDD<-function(ft_node)  {					
if(!exists("FT")) stop("FT and a null T1 object are required in global environment")					
ResultFound<-FALSE					
ChildrenFound<-FALSE					
while(!ResultFound && !ChildrenFound)  {					
					
	if(FT$Type[which(FT$ID==ft_node)]<10) {				
		tag<-FT$Tag[which(FT$ID==ft_node)]			
		ResultList<-ite.form(tag)			
		ResultFound<-TRUE			
	}else{				
		if(FT$Type[which(FT$ID==ft_node)]==10) op<-"or" else op<-"and"			
## Should throw an exception if length(childFT)==0 signifies an empty gate here.					
## However, this case should have been trapped in R code preparing the FT object					
		childFT<-FT[which(FT$CParent==ft_node),]			
		if(length(childFT)==1)  {			
			ft_node<-childFT		
		}else{			
			ChildrenFound<-TRUE		
		}			
	}				
}					
	if(ChildrenFound)  {				
					
		gateFT<-childFT[which(childFT$Type>9),]			
		basicFT<-childFT[which(childFT$Type<10),]			
					
		if(nrow(gateFT)>0)  {			
			NDX<-sapply(gateFT$ID, function(X) {		
					
				if(gateFT$MOE[which(gateFT$ID==X)] > 0)  {	
					gateFT$MOE[which(gateFT$ID==X)] 
				}else{ 	
					X
				}	
			})		
					
			gateFT<-cbind(gateFT, NDX)		
					
			Gline_mat<-c(node=NULL, X1=NULL, X0=NULL)		
					
			while(nrow(gateFT)>0)  {		
				thisGft_node<-gateFT$ID[which(gateFT$NDX==max(gateFT$NDX))]	
				Rlist<-FT2BDD(thisGft_node)	
				Gline_mat<-rbind(Gline_mat, unlist(Rlist))	
				gateFT<-gateFT[-which(gateFT$NDX==max(gateFT$NDX)),]	
			}		
					
			Glist<-as.list(Gline_mat[1,])		
			if(nrow(Gline_mat)>1)  {		
				for(line in 2:nrow(Gline_mat))  {	
					Glist<-BDD.apply(as.list(Gline_mat[line,]), Glist, op)
				}	
			}		
					
			ResultList<-Glist		
		}			
					
			Bline_mat<-c(node=NULL, X1=NULL, X0=NULL)		
					
		if(nrow(basicFT)>0)  {			
			NDX<-sapply(basicFT$ID, function(X) {		
					
				if(basicFT$MOE[which(basicFT$ID==X)] > 0)  {	
					basicFT$MOE[which(basicFT$ID==X)] 
				}else{ 	
					X
				}	
			})		
					
			basicFT<-cbind(basicFT, NDX)		
					
			while(nrow(basicFT)>0)  {		
				thisBft_node<-basicFT$ID[which(basicFT$NDX==max(basicFT$NDX))]	
				Rlist<-FT2BDD(thisBft_node)	
				Bline_mat<-rbind(Bline_mat, unlist(Rlist))	
				basicFT<-basicFT[-which(basicFT$NDX==max(basicFT$NDX)),]	
			}		
			Blist<-as.list(Bline_mat[1,])		
					
			if( exists("ResultList") && length(ResultList)>0 )  {		
				## ResultLine<-BDD.apply(Bline, ResultLine, op) 	
					
				ResultList<-BDD.apply(Blist, ResultList, op) 	
			}else{		
				 ResultList<-Blist	
			}		
					
			if(nrow(Bline_mat)>1)  {		
				for(line in 2:nrow(Bline_mat))  {	
					ResultList<-BDD.apply(as.list(Bline_mat[line,]), ResultList, op)
				}	
			}		
		}			
					
	}				
					
return(ResultList)					
}					
				
					
					
BDD.apply<-function(F,G,op)  {					
if(!exists("FT")) stop("FT and a null T1 object are required in global environment")					
if(tx(F)==tx(G)) {					
	Rlist=F				
}else{					
## test for inclusion in table here, then nest one more else block					
# the return off of the table needs to be parsed back to an ite list object					
	this_operation<-paste0(tx(F),":",tx(G),":",op)				
	pos<-which(T1[1]==this_operation) 				
	if(length(pos)>0)  {				
		Rlist<-tx2ite(T1$result[pos])			
	## The closure for this else block must by-pass the T1 entry code				
	}else{				
#browser()					
		Findex<-min(FT$ID[which(FT$Tag==F$node)])			
		Gindex<-min(FT$ID[which(FT$Tag==G$node)])			
		if(Gindex<Findex) {			
			storeF<-F		
			F<-G		
			G<-storeF		
		}			
	## test for two terminal nodes				
		if(F$X1=="1" && F$X0=="0" && G$X1=="1" && G$X0=="0") {			
			if(op=="or") {		
				Rlist<-list(node=F$node,X1="1", X0=tx(G))	
			}else{		
				Rlist<-list(node=F$node,X1=tx(G), X0="0")	
			}		
		}else{			
		## this code is for non terminal nodes and non-tabled ite pair as ite data frames			
			if(Findex == Gindex) {		
				Rlist<-list(node=F$node, 	
					X1=BDD.txapply2(F$X1, G$X1,op), 
					X0=BDD.txapply2(F$X0, G$X0,op)
					)
			}else{		
				Rlist<-list(node=F$node, 	
					X1=BDD.txapply1(F$X1, G, op), 
					X0=BDD.txapply1( F$X0, G, op)
				)	
					
			}		
		}			
		## store F, G and tx(Rlist) in a table (matrix) so not repeated			
		operation<-paste0(tx(F),":",tx(G),":",op)			
		T1line<-c(operation=operation, result=tx(Rlist))			
		T1<<-rbind(T1, T1line)			
	}				
}					
	return(Rlist)				
}					
				
					
					
BDD.txapply1<-function( txt, H, op)  {					
if(!exists("FT")) stop("FT and a null T1 object are required in global environment")
if(txt == tx(H))  {	
	Rtxt <- txt
}else{	
					
	if(nchar(txt)==1)  {				
		if(op=="or")  {			
			if(txt=="1") Rtxt<-"1" else Rtxt<- tx(H)		
		}else{			
			if(txt=="0") Rtxt<-"0" else Rtxt<- tx(H)		
		}			
	}else{				
## parse txt as an ite data frame object					
		G<-tx2ite(txt)			
		Gindex<-min(FT$ID[which(FT$Tag==G$node)])			
		Hindex<-min(FT$ID[which(FT$Tag==H$node)])			
## assure that INDX(G$node) < INDX(H$node)					
		if(Gindex < Hindex)  {			
			Rtxt<-tx(BDD.apply(G,H,op))		
		}else{			
			Rtxt<-tx(BDD.apply( H,G,op))		
		}			
	}
}	
	return(Rtxt)				
}					
					
					
BDD.txapply2<-function( txt1, txt2, op)  {					
if(!exists("FT")) stop("FT and a null T1 object are required in global environment")

if(txt1 == txt2) {
	Rtxt=txt1
}else{
					
	F<-NULL				
	G<-NULL				
## parse any BDD text to ite data frame					
	if(nchar(txt1)>1) F<-tx2ite(txt1)				
	if(nchar(txt2)>1) G<-tx2ite(txt2)				
					
	if(nchar(txt1)==1 && nchar(txt2)>1) {				
		if(op=="or")  {			
			if(txt1=="1") Rtxt<-"1" else Rtxt<- tx(G)		
		}else{			
			if(txt1=="0") Rtxt<-"0" else Rtxt<- tx(G)		
		}			
	}				
					
	if(nchar(txt2)==1 && nchar(txt1)>1) {				
		if(op=="or")  {			
			if(txt2=="1") Rtxt<-"1" else Rtxt<- tx(F)		
		}else{			
			if(txt2=="0") Rtxt<-"0" else Rtxt<- tx(F)		
		}			
	}				
					
	if(nchar(txt1)==1 && nchar(txt2)==1) {				
		if(op=="or")  {			
			if(txt1=="1" || txt2=="1") Rtxt<-"1" else Rtxt<-"0"		
		}else{			
			if(txt1=="1" && txt2=="1") Rtxt<-"1" else Rtxt<-"0"		
		}			
	}				
					
	if(class(F)=="list" && class(G)=="list")  {		
			Rlist<- BDD.apply(F, G, op)
		Rtxt<-tx(Rlist)	
	}		
}			
	return(Rtxt)		
}			
