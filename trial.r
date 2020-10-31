library(DoE.base)

# L4 2-3
oa.design(ID = get("L4.2.3"), nlevels = 2, nfactors = 2)

# L4 2-3
oa.design(nruns=18, nlevels = c(2,rep(3,7)))
oa.design(nruns=36, nlevels = c(2,2,rep(3,12)))
oa.design(nruns=27, nlevels = c(rep(3,2)))

# L8 4 1 2 1
oa.design(nruns = 8, nlevels=c(4,2))
# L8 4 1 2 2
oa.design(nruns = 8, nlevels=c(4,2,2))

# L32 2 4
oa.design(nruns = 32, nlevels = rep(2,4))

# L36 2 3 3 1
oa.design(nruns = 36, nlevels = c(2,2,2,3))
2*2*2*3 < 36
2*2*3 # 隨機抽4筆*3次

oa.design(nruns = 36, nlevels = c(2,3,3))
2*2*3 < 36
36 - 12
12*2 # 直接重複三次

rbind.data.frame("A" = c(1,2,3), "B" = c(1,2))


x <- as.data.frame(matrix(c('signal', 1:5),ncol = 6))
colnames(x)[1] <- "Factor"
colnames(x)[-1] <- paste("Level", 1:5)
x <- as.data.frame(x)
x$Factor <- "Factor"
x$Factor %>% class
as.vector(x[1,2:6])

dim(iris)[1]
sad = 'b'

x <- data.frame()

x <- cbind.data.frame("a" = c(1,2,3), sad = c(4,5,6))
colnames(x)[2] <- sad

as.character(as.matrix(x[1,2:6]))

as.vector()library(zoo)

addsig <- function(a = NULL, sig_level = NULL){
  sig <- rep(sig_level, each=nrow(a))
  #data <- as.data.frame(data)
  a <- coredata(a)[rep(seq(nrow(a)),length(sig_level))]
  data <- cbind.data.frame(a, signal = sig)
  return(data)
}
a <- coredata(a)[rep(seq(nrow(a)),length(c(1,2)))]
a <- oa.design(nruns = 8, nlevels = c(2,2,2))
a <- as.data.frame(addsig(a,sig_level=c("A","B")))
colnames(a)
# 去掉信號因子
data.frame("Factor"=colnames(a)[-ncol(a)]
           ,"Low"=rep(1,length(colnames(a)[-1]))
           ,"High"=rep(2,length(colnames(a)[-1]))
           , stringsAsFactors = FALSE)
t$Factor %>% class
MAT = matrix(rnorm(50), nrow = 10, dimnames = list(LETTERS[1:10],
                                                   letters[1:5]))

rhandsontable(MAT, width = 300, height = 150) %>%
  hot_cols(colWidths = 100, fixedColumnsLeft = 1) %>%
  hot_rows(rowHeights = 50, fixedRowsTop = 1)




addsig <- function(data = NULL, sig_level = vector()){
  sig <- rep(sig_level, each=nrow(data))
  data <- coredata(data)[rep(seq(nrow(data)),length(sig_level))]
  data <- cbind.data.frame(data, signal = sig)
  return(data)
}

a <- addsig(data = a, sig_level = c(1,2,3))
a <- oa.design(nruns=8, nlevels = rep(2,4))
sig <- rep(c(1,2), each=nrow(a))
a <- coredata(a)[rep(seq(nrow(a)),2)]
cbind.data.frame(a,signal = sig) 

# rep(levels of signal, each = nrow(a))
a <- oa.design(nruns=8, nlevels = rep(2,4))
# 判斷組合數是否符合run數
n = log2(8)
a <- oa.design(nruns = 16, nlevels= c(2,2,2))
a <- coredata(a)[rep(seq(nrow(a)),2)]
a <- as.data.frame(a)
a
as.data.frame(matrix(c("2-3","2-7","","2-11","2-15",rep("",3),"2-31",
                       rep("",2),"2-4",rep("",4),"2-13","",
                       rep("",5),"2-5",rep("",3),
                       rep("",6),"2-6",rep("",2)),
                     ncol=4,nrow=9,byrow = F),row.names = 
                paste("L",c(4,8,9,12,14,16,25,27,32)), col.names = paste0(2:5,"level"))



as.data.frame(as.table(matrix(c("2-3","2-7","","2-11","2-15",rep("",3),"2-31",
                                rep("",2),"2-4",rep("",4),"2-13","",
                                rep("",5),"2-5",rep("",3),
                                rep("",6),"2-6",rep("",2)),
                              ncol=4,nrow=9,byrow = F)))

htmlTable(as.data.frame(matrix(c("2-3","2-7","","2-11","2-15",rep("",3),"2-31",
                                 rep("",2),"2-4",rep("",4),"2-13","",
                                 rep("",5),"2-5",rep("",3),
                                 rep("",6),"2-6",rep("",2)),
                               ncol=4,nrow=9,byrow = F)),
          header =  paste0(2:5,"level "),
          rnames =  paste0("L", c(4,8,9,12,16,16,25,27,32)),
          cgroup = "Single-level designs",
          css.cell = "padding-left: .5em; padding-right: .2em;",
          col.rgroup = c("none", "#F7F7F7"),
          n.cgroup = c(4)
)

Erroresult<- tryCatch({
  a<-'abc'
  b<-1
  c<-b/a
}, warning = function(war) {
  print(paste("MY_WARNING:  ",war))
}, error = function(err) {
  print(paste("MY_ERROR:  ",err))
}, finally = {
  print(paste("End Try&Catch"))
})

y <- list("a"=c(1,2,3),b=c(3,4,5))
names(y)[1]

htmlTable(matrix(c(13,14,rep("",8)
                   ,20,20,rep("",6),15,""
                   ,31,30,rep("",6),27,27
                   ,52,54,32,33,rep("",4),46,46
                   ,90,90,53,54,rep("",4),54,54
                   ,152,160,88,90,rep("",4),62,62
                   ,"","",154,160,90,90,rep("",4)
                   ,rep("",4),156,160,"","",130,130
                   ,rep("",6),158,160,170,170)
                 ,ncol=9,nrow=10, byrow = F),
          header =  2:10,
          rnames = rep(c("unblocked","blocked"),5),
          rgroup = c("Central composite full",
                     "Central composite half",
                     "Central composite quarter",
                     "Central composite eighth",
                     "Box-Behnken"
          ),
          n.rgroup = c(2,2,2,2,2),
          cgroup = "Continuous Factors",
          n.cgroup = c(9)
)
L18_21_31 <- read.csv("E:/Users/Ross/Downloads/R軟體計畫/Taguchi/www/L18_21_31.csv", header = T, stringsAsFactors = F, row.names = NULL)
L18_21_31 <- as.matrix(L18_21_31)
coredata(x)[rep(seq(nrow(x)),50),]
DA <- as.data.frame(coredata(L18_21_31)[rep(seq(nrow(L18_21_31)),2),],row.names = 1:36)

output <- matrix(ncol = 6,
                 nrow = 0)

htmlTable(output, align="r",
          header =  paste(c("1st", "2nd",
                            "3rd", "4th",
                            "5th", "6th"),
                          "hdr"),
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2&dagger;")),
          n.cgroup = rbind(c(1,2,NA),
                           c(2,2,2)),
          caption="Basic empty table with column spanners (groups) and ignored row colors",
          tfoot="&dagger; A table footer commment",
          cspan.rgroup = 2,
          col.columns = c(rep("none", 2),
                          rep("#F5FBFF", 4)),
          col.rgroup = c("none", "#F7F7F7"),
          css.cell = "padding-left: .5em; padding-right: .2em;")

attach(x)
factors_a
