## get_solutions(rev2) continues to utilize the list object for ite
## The solutions function is stream-lined to only build the an Imp vector 
## in the global environment. The max_order is identified during Imp generation.
## The follow up code requires that the ftree object be provided as FT
## so the implicant path_list can be converted directly into index integer values.
## The output list is ready for extract_minimals processing.


get_solutions<-function(FT, bdd)  {				
				
	solutions<-function(F_bdd,sigma=NULL)  {
		if(F_bdd=="1") {		
			Imp<<-unique(c(Imp,sigma))		
			path_order<-length(unlist(gregexpr(pattern =":",sigma)))
			if(path_order > max_order) max_order <<- path_order
		}else{	
			if(F_bdd != "0")  {		
				F_obj<-tx2ite(F_bdd)
				x<-F_obj$node
				F1_bdd<-F_obj$X1
				F2_bdd<-F_obj$X0
				S<-solutions(F1_bdd, paste0(sigma, x, ":") )
				T<-solutions(F2_bdd, sigma)		
			}
		}		
	}	
## note, solutions() populates the external Imp vector and establishes the max_order value	
	Imp<<-NULL	
	max_order<<-1	
	solutions(bdd)		
				
## build the empty nested list structure for implicant processing				
		#max_order<-max(ImpDF$order)		
		imp_list<-list(NULL)		
		if(max_order>1)  {		
			for(len in 2:max_order)  {	
				imp_list<-c(imp_list, list(NULL))
			}	
		}		
				
	for(imp_str in 1:length(Imp))  {				
		sep_v<-unlist(gregexpr(pattern =":",Imp[imp_str]))
		imp_order<-length(sep_v)
		imp_vec<-NULL
		begin_pos<-1
						
		for(imp in 1:imp_order)  {		
			this_node<-substr(Imp[imp_str], begin_pos, sep_v[imp]-1)
			this_index<-min(FT$ID[which(FT$Tag==this_node)])
			imp_vec<-c(imp_vec, this_index)
			begin_pos<-sep_v[imp]+1
		}		
				
		if(is.null(imp_list[[imp_order]])) {		
		        imp_list[[imp_order]]<-sort(imp_vec)		
		}else{		
		         imp_list[[imp_order]]<-rbind(imp_list[[imp_order]], sort(imp_vec))		
		}		
	}			
				
	imp_list			
}				
