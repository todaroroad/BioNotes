
library(ggh4x)
Sys.setenv(LANGUAGE = "en") #显示英文报错信息
options(stringsAsFactors = FALSE) #禁止chr转成factor
library(extrafont)
col_surv = c("#BC3C29FF", "#0072B5FF")
col_pal<-c("#78B7C5",  "#EBCC2A", "#FF0000", "#EABE94", 
         "#3B9AB2", "#B40F20", "#0B775E", "#F2300F", 
         "#5BBCD6", "#F98400", "#ab0213", "#E2D200", 
         "#ff7700", "#46ACC8", "#00A08A", "#78B7C5",
         "#DD8D29")
colorvector=  c("#4577FF","#C2151A","#6F8B35","#303B7F","#EECA1F",
                "#2B7AB5","#339E2B", "#FDAE61","#BDAED4", "#7DC77D",
                "#ABD9E8", "#FDAE61","#339E2B")
col_col = c("#00468BFF", "#00A087FF", "#0099B4FF", "#AD002AFF","#EECA1F","#925E9FFF", 
           "#FDAF91FF", "#ADB6B6FF")
col_pair  =c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", 
            "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"
)
col_set1=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
           "#A65628","#EFC000FF", "#F781BF", "#999999")
col_long=c(col_set1,col_pair)
col_long1=c( "#4DAF4A", "#984EA3", "#FF7F00",
            "#A65628", "#F781BF","#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", 
            "#FDBF6F", "#CAB2D6",  "#B15928")
col_gp = c("#0A1FAB","#EB4D27","#5B1A60","#377D42","#B1823D")
lancet = c("#00468BFF", "#ED0000FF", "#42B540FF", "#0099B4FF", "#925E9FFF", 
  "#FDAF91FF", "#AD002AFF", "#ADB6B6FF")
jco  =c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF", 
        "#003C67FF", "#8F7700FF", "#3B3B3BFF", "#A73030FF", "#4A6990FF")
npg = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", 
        "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF", "#B09C85FF"
)
aaas  = c("#3B4992FF", "#EE0000FF", "#008B45FF", "#631879FF", "#008280FF", 
          "#BB0021FF", "#5F559BFF", "#A20056FF", "#808180FF", "#1B1919FF"
)
nejm = c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "#7876B1FF", 
         "#6F99ADFF", "#FFDC91FF", "#EE4C97FF")
jama = c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", 
         "#6A6599FF", "#80796BFF")
ucscgb = c("#FF0000FF", "#FF9900FF", "#FFCC00FF", "#00FF00FF", "#6699FFFF", 
           "#CC33FFFF", "#99991EFF")
d3 = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", "#9467BDFF", 
       "#8C564BFF", "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"
)
locuszoom = c("#D43F3AFF", "#EEA236FF", "#5CB85CFF", "#46B8DAFF", "#357EBDFF", 
              "#9632B8FF", "#B8B8B8FF")
igv = c("#5050FFFF", "#CE3D32FF", "#749B58FF", "#F0E685FF", "#466983FF", 
        "#BA6338FF", "#5DB1DDFF", "#802268FF", "#6BD76BFF", "#D595A7FF", 
        "#924822FF", "#837B8DFF", "#C75127FF", "#D58F5CFF", "#7A65A5FF", 
        "#E4AF69FF", "#3B1B53FF", "#CDDEB7FF", "#612A79FF", "#AE1F63FF", 
        "#E7C76FFF", "#5A655EFF", "#CC9900FF", "#99CC00FF", "#A9A9A9FF", 
        "#CC9900FF", "#99CC00FF", "#33CC00FF", "#00CC33FF", "#00CC99FF", 
        "#0099CCFF", "#0A47FFFF", "#4775FFFF", "#FFC20AFF", "#FFD147FF", 
        "#990033FF", "#991A00FF", "#996600FF", "#809900FF", "#339900FF", 
        "#00991AFF", "#009966FF", "#008099FF", "#003399FF", "#1A0099FF", 
        "#660099FF", "#990080FF", "#D60047FF", "#FF1463FF", "#00D68FFF", 
        "#14FFB1FF")
scicol = c(lancet,jco,npg,aaas)
col_nm = c("#3B99D4", "#8ED14B", "#F06B49","#ECC2F1", "#82C7C3", "#19413E","#1776EB", 
           "#F5B2AC", "#533085","#89363A","#19413E", "#D92B45", "#60C9FF", "#1B9F2E", 
           "#BA217D", "#635019", "#E3698A", "#076B82", "#A86A16")
col_nm_5=c("#4A86B6","#B4D5A7","#F6F8C3","#F0B06E","#C44A53")
plot_col=c(col_nm,col_nm_5)
library(ggsci)
library(RColorBrewer)
Blues = brewer.pal(9, "Blues")
y_b = brewer.pal(9, "YlGnBu")
purple <- "#3A236B";lightblue <- "#A7DCF8";seagreen <- "#7B9C4A";nake <- "#BDA597";cherry <- "#7F1D47"
  orange <- "#EB736B";blue <- "#57B5AB";blue <- "#4577FF";red <- "#C2151A";orange <- "#E45737"
    blue <- "#4577FF";red <- "#C2151A";orange <- "#E45737";green <- "#6F8B35";darkblue <- "#303B7F"
      darkred <- "#D51113";yellow <- "#EECA1F";blue <- "#4577FF";red <- "#C2151A";orange <- "#E45737"
        green <- "#6F8B35";darkblue <- "#303B7F";darkred <- "#D51113";yellow <- "#EECA1F";purple <- "#3A236B"
          lightblue <- "#A7DCF8";seagreen <- "#7B9C4A";nake <- "#BDA597";cherry <- "#7F1D47";orange <- "#EB736B"
            blue <- "#57B5AB"
            

# 画图的美化规范####################################
## 基本的theme################
self_base_theme = theme(
  text=element_text(face="plain",colour = "black",size = 10),
  axis.title.x = element_text(margin = margin(t= 2, r = 0, b = 0, l = 0),angle = 0,vjust = 0.5, hjust = 0.5,color = "black",size = 10.1),
  axis.title.y = element_text(margin = margin(t= 0, r = 2, b = 0, l = 0),angle = 90,vjust = 0.5, hjust = 0.5,color="black",size = 9.9),
  axis.text.x = element_text(margin = margin(t = 2),color = "black",size = 10.1),
  axis.text.y = element_text(margin = margin(r = 2),color = "black",size = 9.9),
  axis.ticks.x=element_line(color="black",linewidth=0.8,linetype = "solid",lineend = "butt"),
  axis.ticks.y =element_line(color="black",linewidth=0.8,linetype = "solid",lineend = "butt"),
  axis.ticks.length=unit(2,"pt"),
    panel.border = element_blank(),
  axis.line.x=element_line(linetype=1,color="black",linewidth=0.2,lineend="butt"),
  axis.line.y=element_line(linetype=1,color="black",linewidth=0.2,lineend="butt"),
  panel.background = element_blank(),
  plot.title = element_text(margin = margin(b = 2),color = "#2B7AB5"), 
  legend.box.background = element_rect(colour = "grey", linewidth = 0.1), 
  legend.box.margin = margin(0, 0, 0, 0), 
  legend.box.spacing = unit(0.2, "lines"), 
  legend.box = "vertical", 
  legend.position = "right",  
  legend.justification = c(1, 1),         
  legend.text = element_text(size = 9.8), 
  legend.title = element_text(color = "black",hjust=0), 
  legend.background = element_rect(colour = "grey", linewidth = 0.1), 
  legend.direction = "vertical",  
    legend.key = element_blank(), 
  legend.key.height = unit(1, "lines"), 
  legend.key.width = unit(1, "lines")
)
# 设置主题
theme_set(self_base_theme)
## 四个框类似bw的主题######################
self_box_theme = theme(
  axis.line = element_blank(), 
  panel.border = element_rect(colour = "black",  fill = NA,size = 0.9), 
)
## 类似classic的主题######################
self_classic_theme = theme(
  panel.border = element_blank(),
  axis.line.x=element_line(linetype=1,color="black",linewidth=0.4,lineend="square"),
  axis.line.y=element_line(linetype=1,color="black",linewidth=0.4,lineend="square"),
  )



library(easypackages)
libraries("survival","limma","tidyverse","ggalluvial","pheatmap","janitor",
          "survivalROC","stringi","glmnet","ggsignif","gdata","data.table",
          "dplyr","ggpubr","DT","rms","forestplot","showtext","survminer",
          "ggsci","gridExtra","reticulate","here","patchwork","foreach"

)
hh = function(x){
  return(paste0(res,x))
}
# 替代head######
vv= function(x,y=10,z=5){x[1:y,1:z]}

# 使用view的函#######
ve= function(df,row=10){view(head(df,row))}

# rbind########
myrbind <- function(...,method=c("fill","common"),value=NA){
  if("fill"==method[1]) {
    fun1 <- function(x,y,value=NA){
      x[setdiff(colnames(y),colnames(x))] <- value
      
      y[setdiff(colnames(x),colnames(y))] <- value
      
      return(rbind(x,y))
    }
  }
  
  if("common"==method[1]) {
    fun1 <- function(x,y,value=NULL){
      common_cols <- intersect(colnames(x), colnames(y))
      return(rbind(x[, common_cols,drop=F],y[, common_cols,drop=F]))
    }
  }
  return(Reduce(function(x,y){fun1(x=x,y=y,value=value)},list(...)))
}
# 删除特定行名的行#######
delrow <- function(df,del_row){
  df[!(row.names(df) %in% del_row), ,drop = F]
}
# 检测两个向量是否完全一样##########
di = function(v1,v2){
  x =sort(v1) %>% as.character()
  y = sort(v2) %>% as.character()
  z = identical(x,y)
  if(z){
    print("相同")
  }else{
    print("不同")
  }
}
# 检测是否有重复值######
ds= function(x){
  x = x%>%duplicated%>%sum
  if(x>0){
    print("有重复值")
  }else{
    print("没有重复值")
  }
}

# 去重########
rmdup = function(df, ...){
  distinct(df,...,.keep_all = T)
}
# save rda#########
msave= function(file,filenames){
  if(inherits(file, "character")&length(file)==1){
    save(list = file,file = filenames)
  }else{
  save(list = deparse(substitute(file)),file = filenames)
}}
# load函数，一次性load多个文件########
mload=function(filenames){
  lapply(filenames,load,.GlobalEnv)
}
# 自定义函数用于归一化表达谱#########
standarize.fun <- function(indata=NULL, halfwidth=NULL, centerFlag=T, scaleFlag=T) {  
  outdata=t(scale(t(indata), center=centerFlag, scale=scaleFlag))
  if (!is.null(halfwidth)) {
    outdata[outdata>halfwidth]=halfwidth
    outdata[outdata<(-halfwidth)]= -halfwidth
  }
  return(outdata)
}
# 列转行名#######
col2rowname <- function(df){
  rownames(df) <- df[,1]
  df <- df[,-1,drop = F]
  return(df)
}
c2r <- function(df){
  rownames(df) <- df[,1]
  df <- df[,-1,drop = F]
  return(df)
}
my_merge <- function(df1, df2){        # Create own merging function
  merge(df1, df2, by = 0)
}
mergelist = function(data_list){
  Reduce(my_merge, data_list) %>% c2r    # Apply Reduce to own function
}
# 一个取交集的函数,输入为向量#######
mcom <- function(VectorLists)
{
  Reduce(intersect,VectorLists)
}

# matrix 切片使用#########
mslice <- function(matrix,rowname){
  com = intersect(rownames(matrix),rowname)
  matrix[com,,drop = F]
}
mselect <- function(matrix,colname){
  com = intersect(colnames(matrix),colname)
  matrix[,com,drop = F]
}
# 读取文件替代fread########
read=function(input="",file=NULL,c2r = T,data.table = F,header=T,...){
  if(c2r==T){
    fread(input=input,file=file,data.table = data.table,header=header,...) %>% c2r
  }else{
    fread(input=input,file=file,data.table = data.table,header=header,...)
  }
}

# 替代merge，不会产生新的列
mm = function(x,y,by=0,...){
  merge(x=x,y=y,by=by,...) %>% c2r
}


setGeneric("diffset", function(x, y) standardGeneric("diffset"))
setMethod("diffset", signature(x = "character", y = "character"), function(x, y) {
  unique(c(setdiff(x, y), setdiff(y, x)))
})

# 一长串的转换为单个的c()
getvector  =function(str,split = ","){
  str %>% strsplit(split) %>% unlist() %>% dput()
}

#替代paste0的file.path
fp = function(...){
  file.path(...)
}


