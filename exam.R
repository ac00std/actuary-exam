#データの読み込み
exam=read.csv("exam_data.csv",row.names=1)
rate=exam[,11:15]

#定常過程かの確認
library(tseries)
adf.test(rate)

#時系列プロット
ts.plot(rate,gpars=list(xlab="年", ylab="合格率",lty=c(1:5),col=c(1:5)))
legend(locator(1),c("math","life","gen","ann","fin"),lty=c(1:5),col=c(1:5))

#正規性の検証
shapiro.test(x=rate[,1])
shapiro.test(x=rate[,2])
shapiro.test(x=rate[,3])
shapiro.test(x=rate[,4])
shapiro.test(x=rate[,5])

#合格率の自己相関係数
par(mfrow=c(2,1)) 
pacf(rate[,1],lag.max=10,ylim=c(-1,1.0)) 
pacf(rate[,2],lag.max=10,ylim=c(-1,1.0))
windows() 
par(mfrow=c(2,1)) 
pacf(rate[,3],lag.max=10,ylim=c(-1,1.0)) 
pacf(rate[,4],lag.max=10,ylim=c(-1,1.0)) 
windows() 
par(mfrow=c(2,1)) 
pacf(rate[,5],lag.max=10,ylim=c(-1,1.0)) 


###ロジット変換を行う
rate_ln=log(rate/(1-rate))
ts.plot(rate_ln,gpars=list(xlab="年", ylab="合格率",lty=c(1:5),col=c(1:5)))

#定常過程かの確認
library(tseries)
adf.test(rate_ln)

#偏自己相関係数
windows()
par(mfrow=c(3,1)) 
pacf(rate_ln[,1],lag.max=10,ylim=c(-1,1.0),main="math") 
pacf(rate_ln[,2],lag.max=10,ylim=c(-1,1.0),main="life")
pacf(rate_ln[,3],lag.max=10,ylim=c(-1,1.0),main="gen") 

windows() 
par(mfrow=c(3,1)) 
pacf(rate_ln[,4],lag.max=10,ylim=c(-1,1.0),main="ann")
pacf(rate_ln[,5],lag.max=10,ylim=c(-1,1.0),main="fin") 

#正規性の検証
ks.test(x=rate_ln[,1],y="pnorm",mean=mean(rate_ln[,1]),sd=sd(rate_ln[,1]))
ks.test(x=rate_ln[,2],y="pnorm",mean=mean(rate_ln[,2]),sd=sd(rate_ln[,2]))
ks.test(x=rate_ln[,3],y="pnorm",mean=mean(rate_ln[,3]),sd=sd(rate_ln[,3]))
ks.test(x=rate_ln[,4],y="pnorm",mean=mean(rate_ln[,4]),sd=sd(rate_ln[,4]))
ks.test(x=rate_ln[,5],y="pnorm",mean=mean(rate_ln[,5]),sd=sd(rate_ln[,5]))

shapiro.test(x=rate_ln[,1])
shapiro.test(x=rate_ln[,2])
shapiro.test(x=rate_ln[,3])
shapiro.test(x=rate_ln[,4])
shapiro.test(x=rate_ln[,5])


##科目間の相関係数の確認
cor.test(rate_ln[,1],rate_ln[,2])
cor.test(rate_ln[,1],rate_ln[,4])
cor.test(rate_ln[,1],rate_ln[,5])
cor.test(rate_ln[,3],rate_ln[,4])
#ケンドールのτ
cor.test(rate_ln[,1],rate_ln[,5], method = "kendall")

#散布図
 plot(rate,main="合格率の散布図（ロジット変換前）")
 plot(rate_ln,main="合格率の散布図（ロジット変換後）")

#MT法による異常値検出
n=nrow(rate_ln)	# 単位空間のサンプル数を計算
Ave= colMeans(rate_ln) # 単位空間の各変数の平均値を計算
Var=var(rate_ln)*(n-1)/n # 単位空間の共分散行列を計算
k=ncol(rate_ln)	# 変数の数を計算
MD=mahalanobis(rate_ln, Ave, Var)/k	# 単位空間のMDの２乗を計算
plot(MD)

windows()
par(mfrow=c(2,1)) 
#マハラビノス距離=3
pairs(rate_ln, pch=21, bg=c("red","blue")[(MD>3)+1],main="マハラノビス距離を用いた異常値検出")
#マハラビノス距離=1.5
pairs(rate_ln, pch=21, bg=c("red","blue")[(MD>1.5)+1],main="マハラノビス距離を用いた異常値検出")

##VAR
library(vars)
VARselect(exam[,1:10],lag.max=1)
exam.var=VAR(exam[,1:10],p=VARselect(exam[,1:10],lag.max=1)$selection[1])
summary(exam.var)

#モデルの詳細
plot(exam.var)

#モデルの予測
predict(exam.var)
