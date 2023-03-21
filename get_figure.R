setwd("C:/Users/LORENA.LAPTOP-LGLJM15L/Documents/Cours ENS/M1/stage/figures")

load("cv.Rdata")
load("data_param.Rdata")
load("donnees_simul_taille.Rdata")
load("infs_reel.Rdata")
load("lignees_reel.Rdata")
load("mean_best_sim.Rdata")
load("N_select.Rdata")
load("nb_lignees_sup_25_reel.Rdata")
load("p_frechet_post_select.Rdata")
load("p_frechet_pre_select.Rdata")
load("R_post_select.Rdata")
load("R_pre_select.Rdata")
load("var_lignees_dirac.Rdata")
load("var_lignees_exp.Rdata")
load("var_post_select.Rdata")
load("var_pre_select.Rdata")
load("var_simul_dirac.Rdata")
load("var_simul_exp.Rdata")

#fig1
plot(infs_reel~seq(1,178), col="black",type='l',ylim=c(0,300000),ylab="daily incidence", xlab="day", lwd=2, axes=F )
par(new=TRUE)
plot(mean_best_sim[1:178]~seq(1,178), col='pink',  type="l", ylim=c(0,300000),ylab="daily incidence", xlab="day" , lwd=2, axes=F)
axis(side=2)
axis(side=1, at=c(1,60,84,108,178),label=c("1st January", "29th February", "24th March","17th April","26th June"))

barplot(c(as.numeric(lignees_reel[1:24]),nb_lignees_sup_25_reel), col=rgb(0, 0, 0, alpha=0.5),ylim=c(1,1650), ylab="number of lineages", log="y", names.arg=append(seq(1,24),c("sup 25")))
par(new=TRUE)
barplot(mean_best_sim[179:203],col=rgb(1, 0, 0, alpha=0.5),ylim=c(1,1650), ylab="number of lineages",log="y",names.arg=append(seq(1,24),c("sup 25")))


#fig2-3-4
plot(cv)


hist(data_param$N,xlim=c(1000,40000), breaks=seq(0,40000,2000), ylim=c(0,0.0001),col=rgb(1, 1, 1, alpha=0.5), prob=T, ylab="density", xlab="number of importations")
par(new=TRUE)
hist(N_select,xlim=c(1000,40000), breaks=seq(0,40000,2000), ylim=c(0,0.0001),col=rgb(0, 0, 0, alpha=0.5), prob=T, ylab="density", xlab="number of importations")

hist(data_param$R_pre, breaks=seq(2.4,3.4,0.05),xlim=c(2.4,3.4), ylim=c(0,6),col=rgb(1,1,1,alpha=0.5), prob=T, ylab="density", xlab="reproductive number before lockdown")
par(new=TRUE)
hist(R_pre_select,xlim=c(2.4,3.4),breaks=seq(2.4,3.4,0.05), ylim=c(0,6),col=rgb(0,0,0,alpha=0.5), prob=T, ylab="density", xlab="reproductive number before lockdown")

hist(data_param$R_post, xlim=c(0.5,0.9),breaks=seq(0.5,0.9,0.02),ylim=c(0,11),col=rgb(1,1,1,alpha=0.5),prob=T, ylab="density", xlab="reproductive number after the beginning of lockdown")
par(new=TRUE)
hist(R_post_select, xlim=c(0.5,0.9),breaks=seq(0.5,0.9,0.02),ylim=c(0,11),col=rgb(0,0,0,alpha=0.5),prob=T, ylab="density", xlab="reproductive number after the beginning of lockdown")


hist(data_param$p_frechet_pre, xlim=c(0,1), breaks=seq(0,1,0.1), ylim=c(0,1.7), col=rgb(1,1,1,alpha=0.5),prob=T, ylab="density", xlab="p_frechet_pre")
par(new=TRUE)
hist(p_frechet_pre_select, xlim=c(0,1), breaks=seq(0,1,0.1), ylim=c(0,1.7), col=rgb(0,0,0,alpha=0.5),prob=T, ylab="density", xlab="p_frechet_pre")

hist(data_param$p_frechet_post, xlim=c(0,1), breaks=seq(0,1,0.1), ylim=c(0,1.7), col=rgb(1,1,1,alpha=0.5),prob=T, ylab="density", xlab="p_frechet_post")
par(new=TRUE)
hist(p_frechet_post_select, xlim=c(0,1), breaks=seq(0,1,0.1), ylim=c(0,1.7), col=rgb(0,0,0,alpha=0.5),prob=T, ylab="density", xlab="p_frechet_post")

#fig5
plot(donnees_simul_taille$nb_samples[donnees_simul_taille$nb_samples>1]~donnees_simul_taille$beginning[donnees_simul_taille$nb_samples>1], axes=F, ylab="lineage size", xlab="day", pch=".", cex=2, log="y")
axis(side=2)
axis(side=1, at=c(1,32,61,84,122,137),label=c("1st January", "1st February", "1st March", "24st March", "1st May","16th May"))


#fig6
plot(var_lignees_dirac~var_simul_dirac, xlab="variance of the number of secondary infections", ylab="variance of linages size", lwd=2, pch="x", cex=1)
lm_dirac=lm(var_lignees_dirac~var_simul_dirac)
abline(lm_dirac, col="blue")

plot(var_lignees_exp~var_simul_exp, xlab="variance of the number of secondary infections", ylab="variance of linages size", lwd=2, pch="x", cex=1)
lm_exp=lm(var_lignees_exp~log(var_simul_exp))
abline(lm_exp, col="blue")