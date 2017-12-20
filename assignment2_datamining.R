#------#------#------#------#------#------#------#------#
#			 Assignment 2
# CMPT 741: Data Mining 
# Author: Raquel Aoki
# Date: November 17
#------#------#------#------#------#------#------#------#

rm(list=ls(all=TRUE))


#------# Part 1
# Dataset and hyperparameters
setwd("C:/Users/raoki/Documents/Data Mining")
#setwd("C:\\Users\\Raquel Aoki\\Google Drive\\SFU\\Data Mining")
bd = read.table('data-Assignment2.txt', sep=',')
n = dim(bd)[1]
m = dim(bd)[2]
t = 0.3

# jaccard similarity - list similar elements of objects 
#obj.i = c()
#obj.j = c()
#sim  = c()
#for(i in 1:(m-1)){
#	for(j in (i+1):m){
#		aux = bd[,c(i,j)]
#		aux = aux[aux[,1]==1 | aux[,2]==1,]
#		if (dim(aux)[1]>=1){
#			obj.i = c(obj.i,i)
#			obj.j = c(obj.j,j)
#			sim = c(sim,dim(aux[aux[,1]==1 & aux[,2]==1,])[1]/dim(aux)[1])
#		}
#	}
#}
#obj.sim = data.frame(obj.i, obj.j, sim)
#obj.sim = subset(obj.sim , sim>t)
#write.table(obj.sim ,'datamining_a2_sim_obj.csv',sep=',')
obj.sim  = read.table('datamining_a2_sim_obj.csv', header=T, sep=',')
obj.sim = subset(obj.sim, sim>=t)

#------# Part 2
#Signature matrix p random permutations
#Hash function: position first 1
hash_first1 <-function(data, p){
	m = dim(data)[2]
	sign = matrix(0,p,m)
	for(j in 1:p){
		data = data[sample(1:m,m,replace = F),]
		for (i in 1:m){
			sign[j,i]=min(which(data[,i]==1))
		}
	}
	return (sign)
}

p1 = 100
set.seed(1117) #fix seed
#m_sign = hash_first1(bd,p) #signature matrix

#------# Part 3
# Determine the proper b and r for t=0.3
par(mfcol = c(2,2))
p1 = 100
curve(1-(1-t^(p1/x))^x, from=2,to=p1,n=100,lwd=2,xlab='band size',
	main="p=100")
curve(1-(1-t^(p1/x))^x, from=20,to=60,n=100,lwd=2,xlab='band size',
	main='zoom')
#best combination is on the place where the curve change from 0 to 1 
#b = 25
#r = 4

p1 = 500
# Determine the proper b and r for t=0.3
curve(1-(1-t^(p1/x))^x, from=2,to=p1,n=500,lwd=2,xlab='band size',
	main="p=500")
curve(1-(1-t^(p1/x))^x, from=60,to=150,n=100,lwd=2,xlab='band size',
	main='zoom')
#best combination is on the place where the curve change from 0 to 1 
#b = 25
#r = 4
savePlot(filename = "q3",type = "png")


#------#------#------#------#------#------#------#------#
# Part 4, 5, 6 and 6 are in a function to test more b,r and p 
require(digest) #package digest has implemented md5

assignment2 <-function(p,b,r,bd,true.obj){
	m_sign = hash_first1(bd,p) #signature matrix
	output = list(p = p, b = b, r = r)
	o = length(output)
	#------# Part 4
	# Candidate pairs + LHS + hash function h with k=10,000 buckets
	buckets<- rep(list(c()),10000)
	m_buck = matrix(0,b,m)
	#hash function one element for each r elements
	#if two sets of r elements are equal they have the same md5 code
	for(i in 0:(b-1)){
		for(j in 1:m){
			row = m_sign[c((i*r+1):(i*r+r)),j]
			m_buck[i+1,j] = digest(paste(row,collapse="-"),algo = 'md5')
		}
	}

	#creating 10000 buckets from md5 codes
	#there are more than 10000 md5 codes, thus some colisions may occurr
	m_buck = as.numeric(as.factor(m_buck))
	m_buck = matrix(m_buck ,ncol = m)
	for(i in 1:b){
		for(j in 1:m){
			buckets[[m_buck[i,j]%%10000+1]]=c(buckets[[m_buck[i,j]%%10000+1]],j)		
		}
	}

	cont = 1
	cand.numb = 0
	cand.pair.i = c()
	cand.pair.j = c()

	#check if two objects are in the same bucket
	for(b in 1:length(buckets)){
		if(length(buckets[[b]])>1){
			candidatos = buckets[[b]]
			cont = cont+1
			cand.numb = cand.numb+choose(length(buckets[[b]]),2)	
			candidatos = candidatos[order(candidatos)]
			for(i in 1:(length(candidatos )-1)){
				for(j in (i+1):length(candidatos )){
					cand.pair.i = c(cand.pair.i,candidatos [i])
					cand.pair.j = c(cand.pair.j,candidatos [j])
				}
			}	
		}
	}

	cand.pair= data.frame(cand.pair.i,cand.pair.j)
	cand.pair= cand.pair[order(cand.pair.i,cand.pair.j),]
	cand.pair= unique(cand.pair)
	o = o + 1
	output[[o]] = c(cand.pair = dim(cand.pair)[1] )

	#------# Part 5
	# Determine FP and FN from signature 

	# jaccard similarity - list similar elements on signature matrix
	cand = 0
	pair.i= c()
	pair.j = c()
	sim = c()
	for(i in 1:(m-1)){
		for(j in (i+1):m){
			#jaccard  =  sum(m_sign[,i]== m_sign[,j])/p
			cand = cand + 1
			if ((sum(m_sign[,i]== m_sign[,j])/p)>t){
				pair.i = c(pair.i,i)
				pair.j = c(pair.j,j)
				sim = c(sim,(sum(m_sign[,i]== m_sign[,j])/p))
			}
		}
	}

	cand #number total of pairs 
	#length(pair.i) # pairs using set.seed 100 10679
	pair = data.frame(pair.i, pair.j,sim)
	#cand.pair$cand.pair.j[cand.pair$cand.pair.i==1]

	#compare signature pairs with LHS candidate pairs 
	#FP: dissimilar pair candidates
	#FN: similar pair not candidates
	#TP: similar pair candidates
	cand.pair$ind = ''
	pair$ind = ''
	for(i in 1:1000){	
		if(dim(cand.pair[cand.pair$cand.pair.i==i,])[1]>=1){
			cand.pair[cand.pair$cand.pair.i==i,]$ind = 'FP'
			if(dim(pair[pair$pair.i==i,])[1]>=1){
				jj = pair$pair.j[pair$pair.i==i] #true sim
				for(j in 1:length(jj)){
					if(dim(cand.pair[cand.pair$cand.pair.i==i & cand.pair$cand.pair.j==jj[j],])[1]==1){
						cand.pair[cand.pair$cand.pair.i==i & cand.pair$cand.pair.j==jj[j],]$ind = 'TP'
						pair[pair$pair.i==i & pair$pair.j==jj[j],]$ind = 'TP'
					}else{
						pair[pair$pair.i==i & pair$pair.j==jj[j],]$ind = 'FN'
					}
				}
			}
		}else{
			pair$ind[pair$pair.i==i]='FN'
		}
	}
	o = o + 1
	output[[o]]= c(table(cand.pair$ind))
	o = o + 1
	output[[o]]= c(table(pair$ind))

	#------# Part 6
	#Find similar pairs of signatures by removing FP
	cand.pair = subset(cand.pair, ind != 'FP')
	o = o + 1
	output[[o]] = c('n.pair'=dim(cand.pair)[1])

	#------# Part 7 
	#Find similar pairs of objects from the remaining candidate pairs in 6
	cand.pair$final = ''
	true.obj$final = ''
	cand.i = unique(cand.pair$cand.pair.i)
	for(i in 1:length(cand.i)){	
		cand.pair[cand.pair$cand.pair.i==cand.i[i],]$final= 'FP'
		if(dim(true.obj[true.obj$obj.i==cand.i[i],])[1]>=1){
			jj = true.obj$obj.j[true.obj$obj.i==cand.i[i]] #true sim
			for(j in 1:length(jj)){
				if(dim(cand.pair[cand.pair$cand.pair.i==cand.i[i] & cand.pair$cand.pair.j==jj[j],])[1]==1){
					 cand.pair[cand.pair$cand.pair.i==cand.i[i] & cand.pair$cand.pair.j==jj[j],]$final = 'TP'
					true.obj[true.obj$obj.i==cand.i[i] & true.obj$obj.j==jj[j],]$final = 'TP'
				}else{
					true.obj[true.obj$obj.i==cand.i[i] & true.obj$obj.j==jj[j],]$final= 'FN'
				}
			}
		}
	}
	true.obj$final[true.obj$final=='']='FN'
	o = o + 1
	output[[o]]= c(table(cand.pair$final))
	o = o + 1
	output[[o]]= c(table(true.obj$final))
	return(output)
}



#Other (b,r) values
o2 = assignment2(100,20,5,bd,obj.sim)
o3 = assignment2(100,25,4,bd,obj.sim)
o1 = assignment2(100,33,3,bd,obj.sim)
output = rbind(unlist(o1),unlist(o2),unlist(o3))

#Other b and r values
o6 = assignment2(500,100,5,bd,obj.sim)
output = rbind(output,unlist(o6))
o7 = assignment2(500,50,10,bd,obj.sim)
output = rbind(output,unlist(o7))
o8 = assignment2(500,25,20,bd,obj.sim)
output = rbind(output,unlist(o8))

output  = as.data.frame(output )
names(output) = c('p','part3_b','part3_r','part4_numberpairs',
	'part5_FP','part5_TP','part5_FN','part5_TP','part6_numberpairs',
	'part7_FP','part7_TP','part7_FN','part7_TP')
write.table(output,'datamining_a2_output.csv', sep=',')


#Q5
output = output[order(output[,2]),]
par(mfrow=c(2,2))
plot(output[,2],output[,5],type="b",lwd=2,main="Question 2 - FP",xlab="b",ylab="pairs")
plot(output[,2],output[,7],type="b",lwd=2,main="Question 2 - FN",xlab="b",ylab="pairs")
plot(output[,2],output[,10],type="b",lwd=2,main="Question 3 - FP",xlab="b",ylab="pairs")
plot(output[,2],output[,12],type="b",lwd=2,main="Question 3 - FN",xlab="b",ylab="pairs")

savePlot(filename="questao23",type="png")











