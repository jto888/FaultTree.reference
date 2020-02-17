extract_minimals<-function(cs_lists, max_len=NULL)  {

	max_len<-length(cs_lists)
## this is the generalized brute force algorithm  with 4 nested loops									
	for(smat in 1:(max_len-1))  {								
		if(!is.null(cs_lists[[smat]]) && length(cs_lists[[smat]])>0)  {							
		for( tmat in (smat+1):max_len)  {							
			if(!is.null(cs_lists[[tmat]]) && length(cs_lists[[tmat]])>0)  {						
				if(is.vector(cs_lists[[smat]]))  {					
					smat_rows<-1				
				}else{					
					smat_rows<-dim(cs_lists[[smat]])[1]				
				}					
									
				for(source in 1:smat_rows)  {	
				if(is.null(cs_lists[[tmat]]) || length(cs_lists[[tmat]])==0)  break
				elim_rows<-NULL					
## **************** edge cases error here if cs_lists[[tmat]] or cs_lists[[smat]] is a vector (dim will return NULL)									
					if(is.vector(cs_lists[[tmat]])) {				
						if(is.vector(cs_lists[[smat]]))  {			
							if(length(intersect(cs_lists[[smat]][source], cs_lists[[tmat]][1]))==smat)  {		
								cs_lists[[tmat]]<-c(0)	
								break	
								#elim_rows<-NULL
							}		
						}else{			
							if(length(intersect(cs_lists[[smat]][source,], cs_lists[[tmat]][1]))==smat)  {		
								cs_lists[[tmat]]<-c(0)	
								break	
								#elim_rows<-NULL
							}		
						}			
					}else{
## the previous test for null tmat is by-passed within this loop
					##if(!is.null(cs_lists[[tmat]]) && length(cs_lists[[tmat]])>0)  {	
				
							for(target in 1:dim(cs_lists[[tmat]])[1])  {
								if(is.vector(cs_lists[[smat]]))  {		
									if(length(intersect(cs_lists[[smat]][source], cs_lists[[tmat]][target,]))==smat)  {	
										elim_rows<-c(elim_rows, target)
									}	
								}else{		
									if(length(intersect(cs_lists[[smat]][source,], cs_lists[[tmat]][target,]))==smat)  {	
										elim_rows<-c(elim_rows, target)
									}	
								}		
							}
						}		
					##}	
								
## the tmat can now be reduced for future iteration through source(s)									
					if(length(elim_rows)>0)  {			
## test here was probably intended to address cs_lists[[tmat]][,1] but now as matrix not needed									
						##if(length(elim_rows)==length(cs_lists[[tmat]][1,]))  {		
						##		 cs_lists[[tmat]]<-c(0)
						##}else{		
							elim_rows<- (-1)*elim_rows	
							cs_lists[[tmat]]<-cs_lists[[tmat]][elim_rows,]	
						##}		
					}			
									
									
## ready for next source item									
				}					
## Can't figure out why this only impacted last matrix									
##				row.names(cs_lists[[tmat]])<-as.character(1:dim(cs_lists[[tmat]])[1])					
			}
			if(!is.null(cs_lists[[tmat]]) && length(cs_lists[[tmat]])>0)  {	
				if(!is.vector(cs_lists[[tmat]]))  {			
					row.names(cs_lists[[tmat]])<-as.character(1:dim(cs_lists[[tmat]])[1])
				}
			}
		}							
		}							
	}								
## clean up excess order list items									
	for(li in length(cs_lists):1){
		if(length(cs_lists[[li]])>0) {
			break
		}else{
			cs_lists[[li]]<-NULL
		}
	}
									
									
									
cs_lists									
}									
									
