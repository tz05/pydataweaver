# a) a tree-level datafile
# - one row for each measurement (i.e. if a tree was remeasured once, the file contains two lines for this tree)
# - each row contains a unique tree ID, which is the same for all measurements of a single tree (currently not the case in raw FIA data)
# - each row contains a unique plot ID, which is the same for all the trees in a single plot and for all the measurements of those trees, matching the plot ID in the plot-level datafile (see below)
# - if possible, data from plot-level datafile (important columns as mentioned below) already merged in this file 
# b) a plot-level datafile
# - in the best case one file containing merged data from XX_PLOT.csv and XX_COND.csv
# - important columns for me: FORTYPCD, STDAGE, SITECLCD from XX_COND.csv; INVYR, MEASYEAR, MEASMON, MEASDAY from XX_PLOT.csv; but it could be all the columns from both XX_PLOT.csv and XX_COND.csv as well
# - one row for each census
# - each row contains a unique plot ID (see above)
# I need this for data from WA and OR. For training purposes, the data could be filtered for FORTYPCD == 201 already, but I'd rather have the full dataset and do that myself later on. Also you could filter for remeasured plots only, but the same applies here. 

state <- 'OR'   # or 'WA'
setwd('.')
plt <- read.csv(sprintf('%s_PLOT.csv',state),stringsAsFactors=F)
for(fld in c('CN','PREV_PLT_CN')) plt[,fld] <- as.character(plt[,fld])
plt <- plt[,c('CN','PREV_PLT_CN','INVYR','MEASYEAR','MEASMON','MEASDAY')]   # CN, PREV_PLT_CN + others

cond <- read.csv(sprintf('%s_COND.csv',state),stringsAsFactors=F)
for(fld in c('CN','PLT_CN')) cond[,fld] <- as.character(cond[,fld])
cond <- cond[,c('CN','PLT_CN','FORTYPCD','STDAGE','SITECLCD')]              # CN, PLT_CN + others
plt_multi_cond <- names(which(table(cond$PLT_CN)>1))
cond1 <- subset(cond,!PLT_CN%in%plt_multi_cond)     # remove plots with multiple conditions

plt1 <- subset(plt,!CN%in%plt_multi_cond)           # remove plots with multiple conditions
plt_cond <- merge(plt1,cond1,by.x='CN',by.y='PLT_CN')
names(plt_cond)[which(names(plt_cond)=='CN.y')] <- 'COND_CN'
names(plt_cond)[which(names(plt_cond)=='CN')] <- 'PLT_CN'

tree <- read.csv(sprintf('%s_TREE.csv',state),stringsAsFactors=F)
for(fld in c('CN','PLT_CN','PREV_TRE_CN')) tree[,fld] <- as.character(tree[,fld])

tree_CN <- tree[,c('CN','PREV_TRE_CN')]
tree_CN$ID0 <- 1:nrow(tree_CN)
cp <- tree_CN
cp$ID1 <- cp$ID0
while(with(cp,length(intersect(CN,PREV_TRE_CN))>0)) {
    cp <- merge(cp,tree_CN,by.x='PREV_TRE_CN',by.y='CN',all.x=T,suffixes=c('_pre','_post'))
    i_row <- which(is.na(cp$ID0_post))
    if(length(i_row)>0) cp$ID0_post[i_row] <- cp$ID1[i_row]
    i_row <- which(is.na(cp$PREV_TRE_CN))
    if(length(i_row)>0) cp$PREV_TRE_CN[i_row] <- cp$CN[i_row]
    cp <- cp[,c('ID0_pre','PREV_TRE_CN','PREV_TRE_CN_post','ID0_post')]
    names(cp) <- c('ID0','CN','PREV_TRE_CN','ID1')
    print(head(cp[order(cp$ID0),]))
}
cp <- cp[order(cp$ID0),c('ID0','ID1')]
tree <- cbind(ID=tree_CN$CN[cp$ID1],tree)           # use the first-survey CN as tree's ID
tree1 <- subset(tree,!PLT_CN%in%plt_multi_cond)     # remove plots with multiple conditions

library(plyr)
tree_plot <- join(tree1,plt_cond,type='inner',by='PLT_CN')  # "inner" is to remove plots with no condition record
write.csv(tree_plot,row.names=F,file=sprintf('%s_TREE_PLOT.csv',state))
