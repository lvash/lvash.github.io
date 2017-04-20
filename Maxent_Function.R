bioclimLU<-stack("bioclim-landuse1.tif")
names(bioclimLU)<-c("landuse","bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19")
bioclimLU1<-crop(bioclimLU, extent(-86, -58, 10, 28))
elevation<-raster("GDEM-10km-colorized.tif")
elevationlayer<-crop(elevation, extent(-86, -58, 10, 28))
newresElevation<-resample(elevationlayer,bioclimLU1$bio1,method="ngb")
layersLUEL<-stack(newresElevation, bioclimLU1)
names(layersLUEL)[1] <- "elevation"
names(layersLUEL)

data("wrld_simpl")
ws1<-rasterize(wrld_simpl,layersLUEL)
values(layersLUEL)[is.na(values(layersLUEL))]<-0
layers1<-mask(layersLUEL,ws1)
layers1$landuse <- as.factor(layers1$landuse)

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
    mp <-plot(px, main=c("Predicted distribution for", name, "with all points and predictors"))
    return(list(cat("Reps=", reps, ",", "Avg AUC=", mean(e2)), xmFULL, mp))
}
