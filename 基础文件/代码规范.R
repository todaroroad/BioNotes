# 创建一个新项目所需的代码规范###################################
# 初始化设置 ####################################################
# 加载通用数据预览函数
source("/Users/hanxiangming/Library/CloudStorage/Dropbox/documents/GitHubLocal/BioNotes/基础文件/预览数据.R")
# 创建新的文件夹
dir.create(fp(A_proj, "改动"), recursive = TRUE, showWarnings = T) # 创建完成后删掉
# 设置当前项目工作目录
setwd(fp(A_proj, "改动"))
# 定义结果文件夹路径（版本控制建议使用v01/v02等）
res1 = "results/v01/"
res2 = "results/v02/"
res3 = "results/v03/"
# 快捷路径拼接函数，用于生成结果文件路径
hh1 = function(x) {
  return(paste0(res1, x))
}
hh2 = function(x) {
  return(paste0(res2, x))
}
hh3 = function(x) {
  return(paste0(res3, x))
}

# 如果没有 input 文件夹，则创建（用于存放输入数据）
input = "input/"
if (!file.exists(input)) {
  dir.create(input)
}
# 如果没有 scripts 文件夹，则创建（用于存放脚本文件）
scripts = "scripts/"
if (!file.exists(scripts)) {
  dir.create(scripts)
}

# 如果没有 support 文件夹，则创建（用于存放参考信息或函数文件）
support = "support/"
if (!file.exists(support)) {
  dir.create(support)
}
# 如果没有 results 文件夹，则创建（存放分析输出）
res = "results/"
if (!file.exists(res)) {
  dir.create(res)
}
# 创建results下面的子文件夹res1
if (!file.exists(res1)) {
  dir.create(res1)
}
# 创建results下面的子文件夹res2
if (!file.exists(res2)) {
  dir.create(res2)
}
# 创建results下面的子文件夹res3
if (!file.exists(res3)) {
  dir.create(res3)
}
theme_set(self_base_theme)





# 画图的美化规范,见MyRFunction文件####################################
## 画图例子#############
library(ggplot2)
library(ggpubr)

# 准备数据
data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# 比较组
my_comparisons <- list(c("0.5", "1"), c("0.5", "2"), c("1", "2"))

# 加一个 shape 映射来制造第二个图例
ggplot(ToothGrowth, aes(x = dose, y = len, fill = dose, shape = supp)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8, aes(color = dose)) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif",bracket.size=0.5,tip.length=0.01) +
  stat_compare_means(label.y = 40) +
  labs(title = "Tooth Length by Vitamin C Dose",
       x = "Dose (mg)", y = "Tooth Length")+
  self_box_theme 

## 画多个基因的图，主要是小提琴图，box图等##############

###box图##########
(load("results/re_cutoff_diff_analysis.rda"))


# 箱图
# 首先要把输入的文件搞成这个格式
dfselectexpr= read(paste0(res,"selgene_expr_sel.txt"))
# 把condition列移动到第一列
dfselectexpr = dfselectexpr %>% 
  select(condition, everything())
#   行名    condition   Pgk1     Pkm       Tpi1                   
# 10Gy1_FPKM  10Gy     10.69896 10.43811  9.940802     
# 10Gy2_FPKM  10Gy     10.34942 10.10707  9.571522      
# 2Gy1_FPKM   2Gy      10.60362 10.23368  9.666131       

plist = list()
for(i in names(dfselectexpr)[-1]){
  message("当前：", i)
  plotdata = dfselectexpr[c("condition",i)] %>% setNames(c("condition","value"))
  # 这一步为解耦合
  pdata = data.frame( x =plotdata$condition,y=plotdata$value,group = plotdata$condition)
  # 分类因素
  group_vector =c("10Gy", "8Gy", "6Gy", "2Gy", "Ctrl") %>% rev()
  pdata$x=factor(pdata$x,levels = group_vector)
  pdata$group = factor(pdata$group,levels = group_vector)
  # class(pdata$y)
  
  # 获得感兴趣基因的p值
  pvalue_list = map(difflist,~.x[i,"pvalue"])
  # 把每个list的内容按照列合并为一个dataframe
  pvalue_df= do.call(cbind,pvalue_list) %>% as.data.frame() %>% t
  
  # [,1]
  # 10Gy_8Gy_all_diff  0.9402577868
  # 10Gy_6Gy_all_diff  0.0960937809

  pvalue = pvalue_df[c(10,9,7,4,8,6,3,5,2,1),,drop=F]
  
  
  # 为了添加p值我们这里借助了生成t_test函数，然后替换掉p值
  library(ggpubr)
  library(rstatix)
  plabel <- pdata %>%
    # group_by(group) %>%
    t_test(y ~ x) %>%
    # mutate(y.position = y.position*0.9)%>%
    mutate(pvalue = pvalue[,1]) %>% 
    add_significance("pvalue") %>% 
    filter(pvalue.signif != "ns") %>%   # ✅ 去除不显著的
    add_xy_position()
  
  
  
  # 显著性括号
  y_max <- max(pdata$y)
  n_brackets <- nrow(plabel)
  if (nrow(plabel) > 0) {
  plabel <- plabel %>%
    mutate(
      y.position = y_max + 0 + 0.05 * (1:n_brackets)  # 累加高度
    )
  }
  # 开始画图
  library(ggh4x)
  library(RColorBrewer)
  color_v = brewer.pal(9, "Set1")[1:5]
p0 =  ggplot(pdata,aes(x=x, y=y,
                   #  fill = x, #按类填充颜色
                   color = x),#按类给边框着色
  ) +
    # ggh4x::facet_grid2(. ~ group, scales = "free_y", independent = "y")+
    stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3)+
    geom_boxplot(aes(),notch = F)+
    geom_jitter(position = position_jitter(0.2),size=2,alpha=0.3)+
    ggtitle(paste0("gene_",i,": ",paste0(group_vector,collapse = " vs. ")))+
    labs(x="Groups", y="Log2(FPKM+1)", color = "")+ 
    scale_color_manual(values=color_v)+
    scale_fill_manual(values=color_v)+
    stat_pvalue_manual(plabel, label = "pvalue.signif",bracket.size=0.4,tip.length=0.01,hide.ns = F)+
    self_box_theme+
    theme(
          legend.position = "none"
    )
  # ggsave(file = paste0(res,i,"_diff_selgenes.pdf"), width = length(unique(pdata$group))*0.8, height = 4)
  plist[[i]] = p0
  message("完成：", i)
}

# 单个图片合并输出的方法
plots = plist
pncol = 4
sizemg = 3
panlistarr= ggarrange(
  plotlist = plots,
  ncol = pncol,
  nrow = ceiling(length(plots)/pncol),
  labels = "AUTO"
)

panlistarr =annotate_figure(panlistarr, top = text_grob(paste("EXPR"), 
                                                        color = red, face = "bold", size = 14))

ggsave(panlistarr,filename = paste0("plots.pdf"),width = pncol*sizemg,height = ceiling(length(plots)/pncol)*sizemg)


### 画小提琴图#############
(load("results/re_cutoff_diff_analysis.rda"))


# 箱图
# 首先要把输入的文件搞成这个格式
dfselectexpr= read(paste0(res,"selgene_expr_sel.txt"))
# 把condition列移动到第一列
dfselectexpr = dfselectexpr %>% 
  select(condition, everything())
#   行名    condition   Pgk1     Pkm       Tpi1                   
# 10Gy1_FPKM  10Gy     10.69896 10.43811  9.940802     
# 10Gy2_FPKM  10Gy     10.34942 10.10707  9.571522      
# 2Gy1_FPKM   2Gy      10.60362 10.23368  9.666131       

plist = list()
for(i in names(dfselectexpr)[-1]){
  message("当前：", i)
  plotdata = dfselectexpr[c("condition",i)] %>% setNames(c("condition","value"))
  # 这一步为解耦合
  pdata = data.frame( x =plotdata$condition,y=plotdata$value,group = plotdata$condition)
  # 分类因素
  group_vector =c("10Gy", "8Gy", "6Gy", "2Gy", "Ctrl") %>% rev()
  pdata$x=factor(pdata$x,levels = group_vector)
  pdata$group = factor(pdata$group,levels = group_vector)
  # class(pdata$y)
  
  # 获得感兴趣基因的p值
  pvalue_list = map(difflist,~.x[i,"pvalue"])
  # 把每个list的内容按照列合并为一个dataframe
  pvalue_df= do.call(cbind,pvalue_list) %>% as.data.frame() %>% t
  
  # [,1]
  # 10Gy_8Gy_all_diff  0.9402577868
  # 10Gy_6Gy_all_diff  0.0960937809
  
  pvalue = pvalue_df[c(10,9,7,4,8,6,3,5,2,1),,drop=F]
  
  
  # 为了添加p值我们这里借助了生成t_test函数，然后替换掉p值
  library(ggpubr)
  library(rstatix)
  plabel <- pdata %>%
    # group_by(group) %>%
    t_test(y ~ x) %>%
    # mutate(y.position = y.position*0.9)%>%
    mutate(pvalue = pvalue[,1]) %>% 
    add_significance("pvalue") %>% 
    filter(pvalue.signif != "ns") %>%   # ✅ 去除不显著的
    add_xy_position()
  
  
  
  # 显著性括号
  y_max <- max(pdata$y)
  n_brackets <- nrow(plabel)
  if (nrow(plabel) > 0) {
    plabel <- plabel %>%
      mutate(
        y.position = y_max + 0 + 0.05 * (1:n_brackets)  # 累加高度
      )
  }
  # 开始画图
  library(ggh4x)
  library(RColorBrewer)
  
  
  
  color_v = brewer.pal(9, "Set1")[1:5]
  p0 = ggplot(pdata,aes(x=x, y=y,
                        #  fill = x, #按类填充颜色
                        color = x),#按类给边框着色
  )+ 
    geom_violin(aes(colour = x, fill = x),trim = F,alpha = 0.65) + 
    # geom_boxplot(aes(fill = x,colour = x), width = 0.2,alpha = 0.65)+
    # geom_jitter(position = position_jitter(0.2),size=2,alpha=0.3)+
    ggtitle(paste0("gene_",i,": ",paste0(group_vector,collapse = " vs. ")))+
    labs(x="Groups", y="Log2(FPKM+1)", color = "")+ 
    scale_color_manual(values=color_v)+
    scale_fill_manual(values=color_v)+
    stat_pvalue_manual(plabel, label = "pvalue.signif",bracket.size=0.4,tip.length=0.01,hide.ns = F)+
    self_box_theme+
    theme(
      legend.position = "none"
    )
  # ggsave(file = paste0(res,i,"_diff_selgenes.pdf"), width = length(unique(pdata$group))*0.8, height = 4)
  plist[[i]] = p0
  message("完成：", i)
}

# 单个图片合并输出的方法
plots = plist
pncol = 4
sizemg = 3
panlistarr= ggarrange(
  plotlist = plots,
  ncol = pncol,
  nrow = ceiling(length(plots)/pncol),
  labels = "AUTO"
)

panlistarr =annotate_figure(panlistarr, top = text_grob(paste("EXPR"), 
                                                        color = red, face = "bold", size = 14))

ggsave(panlistarr,filename = paste0("vilo_plots.pdf"),width = pncol*sizemg,height = ceiling(length(plots)/pncol)*sizemg)



## 气泡图，其实只用ggplot就能做到#################
# 加载必要的包
library(ggplot2)
library(tidyr)
library(dplyr)

# 创建示例数据框
exp_mat <- data.frame(
  Gene = c("A", "B", "C"),
  CD40 = c(2.461538, 1.738806, 1.303030),
  CD80_CD86 = c(2.625000, 2.192308, 1.904762),
  MHC_I = c(0.05405405, 0.05833333, 0.01136364),
  MHC_II = c(3.333333, 3.280000, 1.466667)
)

percent_mat <- data.frame(
  Gene = c("A", "B", "C"),
  CD40 = c(0.6923077, 0.6940299, 0.4242424),
  CD80_CD86 = c(0.8750000, 0.7435897, 0.5714286),
  MHC_I = c(0.05405405, 0.04166667, 0.01136364),
  MHC_II = c(0.9166667, 0.7200000, 0.3333333)
)

# 将数据转换为长格式
exp_long <- exp_mat %>%
  pivot_longer(cols = -Gene, names_to = "Sample", values_to = "Expression")

percent_long <- percent_mat %>%
  pivot_longer(cols = -Gene, names_to = "Sample", values_to = "Proportion")

# 合并表达量和占比数据
data_long <- exp_long %>%
  left_join(percent_long, by = c("Gene", "Sample"))

# 绘制气泡图
theme_set(self_base_theme)
ggplot(data_long, aes(x = Sample, y = Gene, size = Proportion,fill = Expression)) +
  geom_point(alpha = 0.7,shape = 21, color = "black") +
  scale_size_continuous(range = c(3, 10), name = "prop") +
  scale_fill_viridis_c(option = "viridis", name = "expr") +
  # scale_fill_viridis_c(option = "turbo", name = "expr") +
  # scale_fill_viridis_c(option = "rocket", name = "expr") +
  # scale_color_gradient(low = "blue", high = "red", name = "expr") +
  # scale_fill_gradient(low = "blue", high = "red", name = "expr") +
  labs(title = "", x = "", y = "") + self_box_theme+theme(axis.text.x = element_text(angle = 45))











