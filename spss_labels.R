setwd("E:/QD/tools/labels-frame")
source("E:/QD/tools/asvar.R")#调用自定义操作符%!%，作用:视为变量而非字符
file<-read.csv("test.csv",header = T)
file[,1]<- as.character(file[,1])
file[,2]<-as.character(file[,2])
#预设数据
value<-data.frame()
variable<-data.frame()
make<-data.frame()
stub<-data.frame()
tt<-data.frame()
title<-data.frame()
#初步处理
for(i in 1:nrow(a)){
  if(substr(file[i,1],1,1) %in% c(seq(1:9))){
    value[i,1]<-paste(' ',file[i,1],' "',file[i,2],'"',sep="")
    stub[i,1]<-paste(file[i,1],';',file[i,2],';',sep="")
    
  }
  else{
    value[i,1]<-paste("/",file[i,1],sep="")
    variable[i,1]<-paste(file[i,1],' "',file[i,2],'"',sep="")
    stub[i,1]<-paste("[*data ",file[i,1],"R(;)=",sep="")
    tt[i,1]<-paste("[*set q='",file[i,1],"']","\n","  [*for b=1]","\n","  [*for h=1]","\n","    if $bb[b]/nb, then,","\n","      t#[q][&b][h]=$t[q]*$h[h],","\n","      bt='value[b].[Base[b]]',","\n","    endif,","\n","  [*end h]","\n","  [*end b]","\n\n",sep="")
    title[i,1]<-paste("VT $T",file[i,1],"='","(",file[i,1],")",file[i,2],"',",sep="")#输出title.stp的
    
  }
}

xx<-c(grep("^[A-Z]",file[,1]))#获取变量名所在位置
yy<-c(xx[2:length(xx)],nrow(a)+1)#插值
gap<-yy-xx-1#选项数
#针对多选题处理
if(sum(file[,3])==0){
  make<-0
}else{
  for(i in xx){
    if(file[i,3]==1&file[i,4]==0){
      value[i,1]<-paste("/",file[i,1],"_1 to ",file[i,1],"_",gap[which(xx==i)],sep="")
      assign(paste("z",i,sep=""),paste(rep(file[i,1],gap[which(xx==i)]),'_',seq(1,gap[which(xx==i)]),' "',file[i,2],'"',sep=""))
      variable[i,1]<-do.call(paste,c(as.list(("z"%!%i)[1:gap[which(xx==i)]]),sep="\n"))
      make[i,1]<-paste(file[i,1],";",gap[which(xx==i)],";",sep="")
    }
    if(file[i,3]==1&file[i,4]!=0){
      value[i,1]<-paste("/",file[i,1],"_1 to ",file[i,1],"_",file[i,4],sep="")
      assign(paste("z",i,sep=""),paste(rep(file[i,1],file[i,4]),'_',seq(1,file[i,4]),' "',file[i,2],'"',sep=""))
      variable[i,1]<-do.call(paste,c(as.list(("z"%!%i)[1:file[i,4]]),sep="\n"))
      make[i,1]<-paste(file[i,1],";",file[i,4],";",sep="")
    }
  }
}


value<-rbind("VALUE LABELS",value,".","EXECUTE.")
variable<-subset(variable,!is.na(variable))
variable<-rbind("VARIABLE LABELS",variable,".","EXECUTE.")
labels<-rbind(variable,"\n\n",value)

#########################################MR模板#############################################
#1:table
table<-paste("start control,",	"\n* Study  Sheet1",	"\noldnames,tidy,oldwq,charset=double,nolsum,",	"\nc= xx.dat,",	"\no=..\\output\\xxxxx.tab,",	"\nCEP,",	"\nCSV2,",	"\nfinish control,",	"\n",	"\nstart data,",	"\norder 1234567890xv,",	"\nserial number in columns 1-6,card characters binary 39999,",	"\nTEMPORARY/99999/,",	"\n",	"\n",	"\nF=ADF,!DW读小数",	"\n[*insert define.stp]",	"\n[*insert make.stp]",	"\n[*insert stub.stp]",	"\n[*insert base.stp]",	"\n[*insert title.stp]",	"\n",	"\n",	"\nfinish data,",	"\n",	"\nstart tables,",	"\n%JH='Project Sheet1 * * *',",	"\n%TCI='PAGE <&PAG><JR><J+>*TABLE <&TABLEIDENTIFICATION><JL><J+>',",	"\n%PTR='总体', %AVG='Mean<d2>', %MED='Median', %RTV='<d1>',  GLOBAL BT#='All Respondents',",	"\nF=NWFS,",	"\nF=NUPC/NPTC/NPGN/LBR0/UCH/RBL/CLW12/CHG1/RLW25/BAL/PBL/DBL/RHW25/IAT0/IBJ0/IAJ0/IBT12/SHG1/SIG2/PHG1,",	"\nF=NBTS/NBCS/NBRS/SPL1000/NFSL,     !!F=PCS/NPCB/LBR1/UCL,     !!Just for e-Tabs",	"\n!f = PCB / CAZ '0'/CZP'0'/SLA101/SLB101/NPCS/LPP-1/NRBL/NPRC, !F=DPR2,   ! Percentage",	"\n!f = PCB / CAZ '0'/CZP'0'/SLA101/SLB101/NPCS/LPP-1/NRBL/NRTV, !F=DPT2,   ! Frequency",	"\nf=NPCS/PRC/RTV/SLA95/SLB101,                                       ! For HK CSV-Transformer",	"\n!f=SLA95, !Sig Test;",	"\n",	"\n",	"\n",	"\n[*insert tt.stp]",	"\n",	"\n",	"\n",	"\nfinish tables,",sep="")
#2:define
define<-data.frame()
#3:make

make<-subset(make,!is.na(make))
if(make[1,1]==0){
  make<-"No multiple choice,please check your input file."
}else{
  make<-rbind(c("[*data ttl(;)="),make,"]","[*do i=1:[ttl.#]/2]","   [*do a=1:[ttl.i*2]]","      om $[ttl.i*2-1]=$[ttl.i*2-1]0[a]/1-999,","   [*end a]","[*end i]",sep="")
}

#4:stub
for(i in 1:length(xx)){
  stub[yy[i]-1,1]<-paste(stub[yy[i]-1,1],"\n","]","\n\n","[*set Q='",file[xx[i],1],"']","\n","dm $t[Q]=$[Q]/[*do i=1:[[Q]R.#]/2][[Q]R.i*2-1],[*end i]","\n","   ","r,f,","\n","v $t[Q]='[*do i=1:[[Q]R.#]/2]","\n          ","[[Q]R.i*2] [*sle]**[*end i]","\n          ","**Missing<z>","\n          ","**mmmmm<c><S>',","\n\n",sep="")
}
#5:tt
tt<-subset(tt,!is.na(tt))
#6:base
base<-paste("[*data 999(m=p,t=;)=",	"\n1;t;All Respondents",	"\n        ",	"\n]    ",	"\n[*do i=1:[999.#]:3]","\n   [*set BaseNo=999.i]",	"\n      ds $bb[BaseNo]=[999.i+1],",	"\n      x  $bb[BaseNo]='Base [BaseNo] - [999.i+2]',",	"\n      xt $bb[BaseNo]='Base [BaseNo] - [999.i+2]',","\n   [*set Base[BaseNo]='[999.i+2]']",	"\n[*end i]",sep="")
#7:title
title<-subset(title,!is.na(title))
header<-paste("\n\n!!HEADER1","\ndm x $h1=",'\n\nt:     "Total***([~1])",','\n\nxt $h1="Header A ",',sep="")
title<-rbind(title,header)

#输出标签
write.table(labels,"./label/labels.sps",row.names = F,col.names = F,quote = F)
#输出框架
write.table(table,"./table/table.stp",row.names = F,col.names = F,quote = F)
write.table(define,"./table/define.stp",row.names = F,col.names = F,quote = F)
write.table(stub,"./table/stub.stp",row.names = F,col.names = F,quote = F)
write.table(make,"./table/make.stp",row.names = F,col.names = F,quote = F)
write.table(base,"./table/base.stp",row.names = F,col.names = F,quote = F)
write.table(tt,"./table/tt.stp",row.names = F,col.names = F,quote = F)
write.table(title,"./table/title.stp",row.names = F,col.names = F,quote = F)    

