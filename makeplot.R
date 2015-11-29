library(ggplot2)
library(extrafont)
loadfonts()

setwd("diag")
filenames <- list.files()[grep(paste("^ROC_Bondora(?=.*\\.Rds)",sep=''), list.files(), perl=TRUE)]
datAUC = data.frame(Xval=numeric(0), Yval=numeric(0), checktype=character(0))
for (i in 1:length(filenames)) {
  foo = readRDS(filenames[i])
  check = unlist(strsplit(filenames[i], split='_', fixed = F))[3]
  xx = attr(foo, "x.values")[[1]]
  yy = attr(foo, "y.values")[[1]]
  dat = cbind(xx,yy)
  datAUC = rbind(datAUC,cbind(dat,check))
}
datAUC$xx = as.numeric(as.character(datAUC$xx))
datAUC$yy = as.numeric(as.character(datAUC$yy))
datAUC$check = as.character(datAUC$check)

for (i in seq(0,1,0.01)) {
  datAUC = rbind(datAUC, c(as.numeric(i),as.numeric(i),"baseline"))
}
datAUC$xx = as.numeric(as.character(datAUC$xx))
datAUC$yy = as.numeric(as.character(datAUC$yy))
datAUC$check = as.factor(datAUC$check)

require(grid)
pdf(file="AUCBondora.pdf",family="CM Roman",width=6,height=6)
ggplot(data=datAUC, aes(x=100*xx, y=100*yy, colour=check)) +
  geom_line(aes(linetype=check),size=0.6) +
  scale_linetype_manual(values=c("dotted","dashed","dotdash","solid"))+
  scale_color_manual(values=c("black","red","blue","darkgreen"))+
  theme(text = element_text(size=15))+
  xlab("False positive rate") + ylab("True positive rate") + 
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = seq(0,100,10), labels =as.character(seq(0,100,10))) +
  scale_x_continuous(breaks = seq(0,100,10), labels =as.character(seq(0,100,10))) +
  theme(legend.title=element_blank()) + theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black',size=0.5)) + theme(panel.grid.major = element_line(colour = 'lightgrey', size = 0.3))+
  theme(legend.key.width = unit(2.5, "line"))+
  theme(legend.background = element_rect(colour = 'white',size = 0.1, linetype='solid'))
dev.off()
embed_fonts("AUCBondora.pdf",outfile="AUCBondora.pdf")

### Env permit ###

filenames <- list.files()[grep(paste("^ROC_Envpermit(?=.*\\.Rds)",sep=''), list.files(), perl=TRUE)]
datAUC = data.frame(Xval=numeric(0), Yval=numeric(0), checktype=character(0))
for (i in 1:length(filenames)) {
  foo = readRDS(filenames[i])
  check = unlist(strsplit(filenames[i], split='_', fixed = F))[3]
  xx = attr(foo, "x.values")[[1]]
  yy = attr(foo, "y.values")[[1]]
  dat = cbind(xx,yy)
  datAUC = rbind(datAUC,cbind(dat,check))
}
datAUC$xx = as.numeric(as.character(datAUC$xx))
datAUC$yy = as.numeric(as.character(datAUC$yy))
datAUC$check = as.character(datAUC$check)

for (i in seq(0,1,0.01)) {
  datAUC = rbind(datAUC, c(as.numeric(i),as.numeric(i),"baseline"))
}
datAUC$xx = as.numeric(as.character(datAUC$xx))
datAUC$yy = as.numeric(as.character(datAUC$yy))
datAUC$check = as.factor(datAUC$check)

require(grid)
pdf(file="AUCEnvPermit.pdf",family="CM Roman",width=6,height=6)
ggplot(data=datAUC, aes(x=100*xx, y=100*yy, colour=check)) +
  geom_line(aes(linetype=check),size=0.6) +
  scale_linetype_manual(values=c("dotted","dashed","dotdash","solid"))+
  scale_color_manual(values=c("black","red","blue","darkgreen"))+
  theme(text = element_text(size=15))+
  xlab("False positive rate") + ylab("True positive rate") + 
  theme(legend.position="bottom") +
  scale_y_continuous(breaks = seq(0,100,10), labels =as.character(seq(0,100,10))) +
  scale_x_continuous(breaks = seq(0,100,10), labels =as.character(seq(0,100,10))) +
  theme(legend.title=element_blank()) + theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black',size=0.5)) + theme(panel.grid.major = element_line(colour = 'lightgrey', size = 0.3))+
  theme(legend.key.width = unit(2.5, "line"))+
  theme(legend.background = element_rect(colour = 'white',size = 0.1, linetype='solid'))
dev.off()
embed_fonts("AUCEnvPermit.pdf",outfile="AUCEnvPermit.pdf")