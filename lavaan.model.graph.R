#05/25/2017 11.25pm Updated to use readJpg{jpeg} 
#07/20/2017 11:01pm Updated to work with categorial variables and latent variables

lavaan.model.graph<-function(model,outfile="outfile",out.format=NULL,dpi=300,dot.code=NULL,categorical=NULL){
  plot_jpeg = function(path, add=FALSE)
  {
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[1:2] # get the resolution
    if (!add) # initialize an empty plot area if add==FALSE
      plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=res[1]/res[2],type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
  }
#trim any leading line breaks
if(substr(model, 1,1)=="\n")model<-sub("\n","",x= model)
#create function to remove white space in node names
trim <- function (x) gsub("\\s", "", x)
#separate into equations
mod<-strsplit(model,"\n")[[1]]
#trim the white spaces
new.mod<-rep(NA,length(mod))
for(i in 1:length(mod))new.mod[i]<-trim(mod[i])
#set up couters for number of obseved nodes, correations and latent vars
neqn=0
ncor=0
nlat=0
#set identifier of position in index of each type (eqn, corr, lat)
pos<-rep(NA,length(new.mod))

for(i in 1:length(new.mod))
if(length(grep("~~",new.mod[i]))==1) {ncor=ncor+1;pos[i]<-"cor"} else
  if(length(grep("=~",new.mod[i]))==1) {nlat=nlat+1;pos[i]<-"lat"} else
    if(length(grep("~",new.mod[i]))==1) {neqn=neqn+1;pos[i]<-"eqn"}

eqns=matrix(NA,neqn,2)
corrs=matrix(NA,ncor,2)
lats<-matrix(NA,nlat,2)
eqns1=as.data.frame(matrix(NA,neqn,2))
lats1=as.data.frame(matrix(NA,nlat,2))
#reorder equations
new.mod<-c(new.mod[pos=="eqn"],new.mod[pos=="cor"],new.mod[pos=="lat"])
#split each rhs up into components
if(neqn>0)for(i in 1:neqn)eqns[i,]<-strsplit(new.mod[i],"~")[[1]]
if(ncor>0)for(i in 1:ncor)corrs[i,]<-strsplit(new.mod[i+neqn],"~~")[[1]]
if(nlat>0)for(i in 1:nlat)lats[i,]<-strsplit(new.mod[i+neqn+ncor],"=~")[[1]]



if(neqn!=0)for(i in 1:neqn){
      eqns1[i,1]<-eqns[i,1]
      #remove all pre-multiplied stuff
      foo<-strsplit(lats[i,2],"\\+")[[1]]
      foo1<-strsplit(foo,"\\*")
      len<-sapply(foo1,length)
      eqn.new<-NULL
      for(j in 1:length(len))eqn.new[j]<-foo1[[j]][len[j]]
      lats[i,2]<-paste(eqn.new,collapse = "+")
      if(!is.null(categorical)){       
                 rhs<-strsplit(eqns[i,2],"\\+")[[1]]
                 for(j in 1:length(categorical)){
                   rhs[rhs%in%categorical[[j]]]<-names(categorical)[j]
                   rhs<-unique(rhs)
                 }
                 eqns[i,2]<-paste(rhs,collapse="+")
                 eqns1[i,2]<-length(rhs)} else eqns1[i,2]<-length(strsplit(eqns[i,2],"\\+")[[1]])}

if(nlat!=0)for(i in 1:nlat){
      lats1[i,1]<-lats[i,1]
      #remove all pre-multiplied stuff
      foo<-strsplit(lats[i,2],"\\+")[[1]]
      foo1<-strsplit(foo,"\\*")
      len<-sapply(foo1,length)
      eqn.new<-NULL
      for(j in 1:length(len))eqn.new[j]<-foo1[[j]][len[j]]
      lats[i,2]<-paste(eqn.new,collapse = "+")
      #combine categoricals
      if(!is.null(categorical)){       
      rhs<-strsplit(lats[i,2],"\\+")[[1]]
      for(j in 1:length(categorical)){
      rhs[rhs%in%categorical[[j]]]<-names(categorical)[j]
      rhs<-unique(rhs)
    }
    lats[i,2]<-paste(rhs,collapse="+")
    lats1[i,2]<-length(rhs)} else lats1[i,2]<-length(strsplit(lats[i,2],"\\+")[[1]])
}
  
  if(ncor!=0)for(i in 1:ncor){
  #remove all pre-multiplied stuff
  foo<-strsplit(corrs[i,2],"\\+")[[1]]
  foo1<-strsplit(foo,"\\*")
  len<-sapply(foo1,length)
  eqn.new<-NULL
  for(j in 1:length(len))eqn.new[j]<-foo1[[j]][len[j]]
  corrs[i,2]<-paste(eqn.new,collapse = "+")
  }


norow=ncor+sum(eqns1[,2])+ifelse(nlat==0,0,sum(lats1[,2]))
mat=matrix(NA,norow,3)
colnames(mat)<-c("lhs","op","rhs")
mat[,"op"]<-c(rep("~",sum(eqns1[,2])),rep("~~",ncor),rep("=~",sum(lats1[,2])))

x<-NULL
if(neqn>0)for(i in 1:neqn)x<-append(x,rep(eqns1[i,1],eqns1[i,2]))
x<-append(x,corrs[,1])
if(nlat>0)for(i in 1:nlat)x<-append(x,rep(lats1[i,1],lats1[i,2]))
mat[,"lhs"]<-x

x<-NULL
if(neqn>0)for(i in 1:neqn)if(eqns1[i,2]>1)x<-append(x,strsplit(eqns[i,2],"\\+")[[1]]) else x<-append(x,eqns[i,2])
x<-append(x,corrs[,2])
if(nlat>0)for(i in 1:nlat)if(lats1[i,2]>1)x<-append(x,strsplit(lats[i,2],"\\+")[[1]]) else x<-append(x,lats[i,2])
mat[,"rhs"]<-x
vars=c(mat[,1],mat[,3])
node.names<-unique(vars)
ests<-data.frame(mat)
ests1<-ests[which(ests$op=="~"),1:3]
exo<-ests1[!ests1[,"rhs"]%in%ests1[,"lhs"],"rhs"]
exo<-as.vector(exo[!duplicated(exo)])

sink(paste(outfile,".dot",sep=""))
cat("digraph LavaanSEM{",sep="\n")
cat(paste("graph [ dpi = ",dpi,"]",sep=""),sep="\n")
if(!is.null(dot.code))cat(dot.code,sep="\n") else
 
if(length(exo)<0)cat(paste("{rank=min;","\"",exo,"\"","}",sep=""),sep="\n")

for(i in 1:length(node.names)){
  if(node.names[i]%in%unique(ests[ests$op=="=~",1]))
  cat(paste("\"",node.names[i],"\"","[shape=oval]",sep=""),sep="\n" ) else
    cat(paste("\"",node.names[i],"\"","[shape=box]",sep=""),sep="\n" )}

for(i in 1:nrow(ests)){
     if(ests[,"op"][i]=="~")cat(paste("\"",ests[,"rhs"][i],"\"","->","\"",ests[,"lhs"][i],"\"",sep=""),sep="\n")
     if(ests[,"op"][i]=="~~")cat(paste("\"",ests[,"rhs"][i],"\"","->","\"",ests[,"lhs"][i],"\"","[dir=both]",sep=""),sep="\n")
     if(ests[,"op"][i]=="=~")cat(paste("\"",ests[,"rhs"][i],"\"","->","\"",ests[,"lhs"][i],"\"","[dir=back]",sep=""),sep="\n")
     }

cat("}")
sink()
out<-paste("dot -Tjpg ",outfile,".dot ", "-o ",outfile,".jpg",sep="")
system(out)
if(!is.null(out.format))system(paste("dot -T",out.format," ",outfile,".dot -o ",outfile,".",out.format,sep=""))
plot_jpeg(paste(outfile,".jpg",sep=""))
}
