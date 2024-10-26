# https://blog.csdn.net/dege857/article/details/127859291
library(grid)
library(forestploter)
library(ggplot2)
library(dplyr)
setwd("E:\\work\\wrn\\孟德尔随机化\\敏感性处理后\\")
# rm(list=ls())

AAs_merge<-read.csv("AAs_merge1.csv")
# 正向####
# 提取所需变量
dt2 <- AAs_merge
dt2[, 2:6] <- round(dt2[, 2:6], 4)
dt2[, 2:6] <- lapply(dt2[, 2:6], function(x) as.numeric(formatC(x, format = "f", digits = 3)))

# 将 dt2 中的所有 NA 转为空字符串
dt2 <- dt2 %>% mutate_all(~ ifelse(is.na(.), '', .))
dt2[, 4:6] <- lapply(dt2[, 4:6], function(x) as.numeric(formatC(x, format = "f", digits = 3)))

# 先把数据格式整理一下，先让它缩进一格
dt2$Method <- ifelse(is.na(dt2$N_of_SNP), 
                     dt2$Method,
                     paste0("   ", dt2$Method))


# 生成一个变量se，它在绘图的时候表示正方形的大小
dt2$se <- 6

# 生成一个绘图区间，等下用来绘图
dt2$` ` <- paste(rep(" ", 17), collapse = " ")

# 生成HR和可信区间
dt2$`OR (95% CI)` <- dt2$P_value_CI
dt2$OR <- as.numeric(substr(dt2$P_value_CI, 1, 5))

dt3<-dt2[-c(2,9,15,20,27,33,38,44,51),]
# orBeta_theme函数可以对森林图图片的细节进行调整，我们可以在forBeta_theme函数上设定预定的森林图样式模块，到时直接绘图就好了
tm <- forest_theme(base_size = 10,  #文本的大小
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,   #可信区间点的形状
                   ci_col = "#762a83",    #CI的颜色
                   ci_fill = "#762a83",     #ci颜色填充
                   ci_alpha = 0.8,        #ci透明度
                   ci_lty = 1,            #CI的线型
                   ci_lwd = 1.5,          #CI的线宽
                   ci_Theight = 0.2, # Set an T end at the end of CI  ci的高度，默认是NULL
                   # Reference line width/type/color   参考线默认的参数，中间的竖的虚线
                   refline_lwd = 1,       #中间的竖的虚线
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color  垂直线宽/类型/颜色   可以添加一条额外的垂直线，如果没有就不显示
                   vertline_lwd = 1,              #可以添加一条额外的垂直线，如果没有就不显示
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders   更改填充和边框的摘要颜色
                   summary_fill = "yellow",       #汇总部分大菱形的颜色
                   summary_col = "#4575b4",
                   # Footnote font size/face/color  脚注字体大小/字体/颜色
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "red",
                   # legend
                   legend_name = "Group",   #设置标题名字
                   legend_value = c("All participants"," "))   #设置分组名字


# 设定好模块后直接绘图就可以了
# 关闭所有未关闭的图形设备
while (!is.null(dev.list())) dev.off()

# 设置PDF图形设备参数
pdf("forest_plot_front3.pdf", width = 12, height = 16)  # 调整页面高度以适应更多的行

# 生成森林图
pt<-forest(dt3[,c(1,8,14,15,9:11)],
           est = dt3$OR,
           lower = dt3$CI_lower, 
           upper = dt3$CI_upper,
           sizes = 0.3,
           ci_column = 3,
           ref_line = 1,
           xlim = c(0,2),
           ticks_at = c(0,1,2),
           fontsize = 4,
           theme = tm)

row.names(dt3) <- 1:nrow(dt3)
# 把第五行的背景改成绿色
g <- edit_plot(pt, row = c(seq(1, 45, by = 5)),
               which = "background",
               gp = gpar(fill = "#98f5ff"))

# 把2, 5, 10, 13, 17, 20行的文本变成粗体
g <- edit_plot(g,
               row = c(seq(1, 45, by = 5)),
               gp = gpar(fontface = "bold"))

# 我们还可以对图片细节进行修改，比如我们想把第三行变成红色


g <- edit_plot(g, row = c(2:5,7:10,12:14,17:20,22),col =  c(4),

                gp = gpar(col = "red"))
g <- edit_plot(g, row = c(seq(1, 45, by = 5)),col =  c(1),
               gp = gpar(col = "#104e8b"))

g <- edit_plot(g, row = c(1,16,31,36),col = c(5),
               gp = gpar(col = "red"))
g
dev.off()

