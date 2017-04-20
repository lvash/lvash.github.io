
myMaxentRes<-function(points=cbind(runif(50,min=-83.7,max=-60.8),runif(50, min=14.1, max=26.5)), preds= layers1, name = "species name", reps=10){
    e2<-vector(mode="numeric")
    for(i in 1:reps){
        group <- dismo::kfold(points, 5)
        pres_train <- points[group != 1, ]
        pres_test <- points[group == 1, ]
        
        backg <- dismo::randomPoints(preds, n=800, warn=0)
        colnames(backg) = c('lon', 'lat')
        group <- dismo::kfold(backg, 5)
        backg_train <- backg[group != 1, ]
        backg_test <- backg[group == 1, ]
        
        xm <- dismo::maxent(x=preds, removeDuplicates=T, p=pres_train,a=backg_train, args=c("-J","-P"))
        ev <- evaluate(pres_test, backg_test, xm, preds)
        e2[i]<-ev@auc }
    xmFULL <- dismo::maxent(x=preds, removeDuplicates=T, p=points,a=backg, args=c("-J","-P"))
    px <- predict(preds, xmFULL, progress="")
    mp <-plot(px, main=name)
    return(list(cat("Reps=", reps, ",", "Avg AUC=", mean(e2)), xmFULL, mp))
}
