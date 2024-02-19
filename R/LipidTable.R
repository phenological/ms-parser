
#' conn = read_sdf("~/Downloads/structures.sdf",coord = F)
#' # read.table("~/Downloads/structures.sdf",header = F,sep = "\\t")
#' test<-data.frame(tab1 = read.csv("~/Downloads/structures.sdf",header = F,sep = "\t"))
#' 
#' 
#' id_row<-grep("<LM_ID>",test[,1])
#' end_row<-grep("$$$$",test[,1],fixed = T)
#' 
#' LMID<-list()
#' for(i in 1:length(id_row)){
#'   ID<-test[c(id_row[i]+1),1]
#'   tdf<-data.frame(t(matrix(test[id_row[i]:(end_row[i]-1),1],ncol = 2,byrow = T)))
#'   colnames(tdf)<-gsub("> <","<",tdf[1,])
#'   tdf<-tdf[2,]
#'   LMID[[ID]]<-tdf
#'   rm(ID,tdf)
#'   
#' }
#' 
#' library(data.table)
#' test1<-rbindlist(LMID,fill = T)
#' 
#' write.csv(test1,file = "~/Desktop/LMID.csv")
#' 
#' LMID<-read.csv("~/Desktop/LMID.csv")

test<-read.csv("~/Downloads/Results_P01-P10/2021-11-09_Busselton P01-10_SE_lipidexploreRv2_individual_lipid_data.csv")
test<-test[,-c(1:2)]

# lipid_names<-colnames(test)
# lipid_class_name<-sapply(strsplit(lipid_names,".",fixed = T),"[",1)

# lipid_class_name
# CE  CER  DAG DCER  FFA HCER LCER  LPC  LPE  LPG  LPI  MAG   PC   PE   PG   PI   PS   SM  TAG 
# 21   11   48    9   20   19   13   15    3    2    5   10   73  120   32   42   23   11  439 
library(dplyr)
Lipid<-data.frame(id = colnames(test))%>%
  mutate(class = sapply(strsplit(id,".",fixed = T),"[",1),
         family = ifelse(class=="CE","Cholesteryl esters",class),
         family = ifelse(class=="CER","Ceramides",family),
         family = ifelse(class=="DAG","Diacylglycerol",family),
         family = ifelse(class=="DCER","Dihydroceramides",family),
         family = ifelse(class=="FFA","Free fatty acids",family),
         family = ifelse(class=="HCER","Hexosylceramides",family),
         family = ifelse(class=="LCER","Lactosylceramides",family),
         family = ifelse(class=="LPC","Lysophosphatidylcholine",family),
         family = ifelse(class=="LPE","Lysophosphatidylethanolamine",family),
         family = ifelse(class=="LPG","Lysyl-phosphatidylglycerol
",family),
         family = ifelse(class=="LPI","Lysophosphatidylinositol
",family),
         family = ifelse(class=="MAG","Monoacylglycerols",family),
         family = ifelse(class=="PC","Phosphatidylcholine",family),
         family = ifelse(class=="PE","Phosphatidylethanolamine",family),
         family = ifelse(class=="PG","Phosphatidylglycerol",family),
         family = ifelse(class=="PI","Phosphoinositides",family),
         family = ifelse(class=="PS","Phosphatidylserine",family),
         family = ifelse(class=="SM","Sphingomyelin",family),
         family = ifelse(class=="TAG","Triacylglycerols",family),
         linkage = ifelse(grepl(".P.",id,fixed = T),"alkyl ether",NA),
         linkage = ifelse(grepl(".O.",id,fixed = T),"(1Z)-alkyl ether",linkage),
         linkage = ifelse(grepl(".d",id,fixed = T),"1,3 dihydroxy",linkage),
         # linkage = ifelse(grepl(".O.",id,fixed = T),"(1Z)-alkyl ether",linkage),
         sidechain = gsub(".$*","",gsub("d","",gsub("P.","",gsub("O.","",gsub("^[^.]*.","",id))))),
         s1 = sapply(strsplit(sidechain,"_"),"[",1),
         s2 = sapply(strsplit(sidechain,"_"),"[",2),
         unit = "ug/ml"
         )




# save(test,Lipid,file = "~/git/phenological/ms-parser/data/Lipids.rda")

