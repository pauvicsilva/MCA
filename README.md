# MCA
# link !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/

##################################################
######### Multiple Correspondence Analysis #######
##################################################

library("FactoMineR")
library("factoextra")

data(poison)
head(poison[, 1:7], 3)

poison.active <- poison[1:55, 5:15]
head(poison.active[, 1:6], 3)

# Summary of the 4 first variables
summary(poison.active)[, 1:4]

for (i in 1:4) {
  plot(poison.active[,i], main=colnames(poison.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}

res.mca <- MCA(poison.active, graph = FALSE)

print(res.mca)


#  library("factoextra")

eig.val <- get_eigenvalue(res.mca)
head(eig.val)

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))


# fviz_mca_biplot(res.mca, 
#                repel = TRUE, # evitar nomes lagos (com muitos pontos pode dá erro)
#                ggtheme = theme_minimal())
#---------------
# var <- get_mca_var(res.mca)
# var

# PLOT !!!!!!!!!!!!!!!!!!!!
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# PLOT !!!!!!!!!!!!!!!!!!!!
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# PLOT !!!!!!!!!!!!!!!
#col.var = "cos2",
#gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),# ("white", "blue", "red")
# Color by cos2 values: quality on the factor map

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

##########################################
#### Residuos par AC bidimensional #######
##########################################

a=c(rep("a",10),rep("b",15),rep("c",20))
b=c(rep(c("ZZ","XX","YY"),15) )

nu=as.integer(rnorm(45,20,4))
table(a,b)
resid.PAD=function(a,b){
  a=as.factor(a)
  b=as.factor(b)
  t=table(a,b)
  s=sum(t)
  an=length(levels(a))
  bn=length(levels(b))
  m=matrix(t,an,bn)
  res=c()
  z=1
  for (j in c(1:bn) ){
    for (i in c(1:an) ){
      
      res[z]=(t[i,j]- sum(t[,j])*sum(t[i,])/s )/sqrt( sum(t[,j])*sum(t[i,])/s )
      z=z+1
    }
  }
  res=matrix(res,an,bn,  dimnames = list(levels(a),levels(b)) )
  return(res)
}
resid.PAD(a,b)
