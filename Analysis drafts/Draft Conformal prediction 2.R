
for (i in 1:1){
  data("apparelTrans")
  all = apparelTrans
  splitWeek = 40
  coverage = 0
  ntraining = 100
  ncalibrate = 90
  ntest = 250 - ntraining - ncalibrate
  customers = unique(apparelTrans$Id)
  true = data.table("Id" = results$"Id",
                    "True" = new3$CLV)
  q = rep(0,ncalibrate)
  smp = sample(customers, ntest, replace = FALSE)
  test = all[Id %in% smp,]
  working = all[!(Id %in% smp),]
  workingcustomers = unique(working$Id)
  
  while (coverage < 0.88 | coverage > 0.92){
    tryCatch(
      {
        train = sample(workingcustomers, ntraining, replace = FALSE)
        calibrate = working[!(Id %in% train), ]
        train = working[Id %in% train, ]
        controltable = data.table("Id" = new3[Id %in% unique(calibrate$Id), "Id"],
                                  "Predicted" = 0,
                                  "True" = new3[Id %in% unique(calibrate$Id), "CLV"],
                                  "Lower" = 0,
                                  "Upper" = 0,
                                  "Covered" = 0)
        colnames(controltable) = c("Id", "Predicted", "True", "Lower", "Upper", "Covered")
        trainCLV = clvdata(train,
                           date.format="ymd", 
                           time.unit = "week",
                           estimation.split = splitWeek,
                           name.id = "Id",
                           name.date = "Date",
                           name.price = "Price")
        
        calibrateCLV = clvdata(calibrate,
                               date.format="ymd",
                               time.unit = "week",
                               estimation.split = splitWeek,
                               name.id = "Id",
                               name.date = "Date",
                               name.price = "Price")
        
        trainModelpnbd = pnbd(clv.data = trainCLV)
        calibrateModelpnbd = pnbd(clv.data = calibrateCLV)
        trainModelgg = gg(clv.data = trainCLV)
        calibrateModelgg = gg(clv.data = calibrateCLV)
        
        calibrateModelpnbd@prediction.params.model[1] = trainModelpnbd@prediction.params.model[1]
        calibrateModelpnbd@prediction.params.model[2] = trainModelpnbd@prediction.params.model[2]
        calibrateModelpnbd@prediction.params.model[3] = trainModelpnbd@prediction.params.model[3]
        calibrateModelpnbd@prediction.params.model[4] = trainModelpnbd@prediction.params.model[4]
        
        calibrateModelgg@prediction.params.model[1] = trainModelgg@prediction.params.model[1]
        calibrateModelgg@prediction.params.model[2] = trainModelgg@prediction.params.model[2]
        calibrateModelgg@prediction.params.model[3] = trainModelgg@prediction.params.model[3]
        
        # Make predictions
        results_pnbd_calibrate = predict(calibrateModelpnbd)
        results_gg_calibrate = predict(calibrateModelgg)
      
        # Calculate the predicted CLV with the predicted DERT and predicted mean spending
        controltable$Predicted = results_pnbd_calibrate$DERT * results_gg_calibrate$predicted.mean.spending
        
        controltable$Lower = controltable$Predicted - (q * controltable$Predicted)
        controltable$Upper = controltable$Predicted + (q * controltable$Predicted)
        controltable$Covered = (controltable$True < controltable$Upper & controltable$True > controltable$Lower)
        coverage = mean(controltable$Covered)
        if (coverage < 0.88){
          q = q * 1.05
        }
        else{
          q = q * 0.99
        }
        print(q)
        print(coverage)
      },
      error = function(e){},
      warning = function(w){}
    )
  }


  f = a + bx + cx^2 + dx^3 + ex^4
  
  parametertable = data.table(expand.grid(c(-2,-1,0,1,2), c(-2,-1,0,1,2), c(-1,0,1), c(-1,0,1), c(-1,0,1)))
  all = apparelTrans
  ntest = 50
  customers = unique(apparelTrans$Id)
  smp = sample(customers, ntest, replace = FALSE)
  test = all[Id %in% smp,]
  working = all[!(Id %in% smp),]
  workingcustomers = unique(working$Id)
  
  for (i in 1:10){
    smp = sample(workingcustomers, ntraining, replace = FALSE)
    calibrate = working[!(Id %in% smp), ]
    train = working[Id %in% smp, ]
    trainCLV = clvdata(train,
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = splitWeek,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")
  
    trainModelpnbd = pnbd(clv.data = trainCLV)
    trainModelgg = gg(clv.data = trainCLV)
    results_pnbd_calibrate = predict(trainModelpnbd)
    results_gg_calibrate = predict(trainModelgg)
  
    table = data.table("Id" = smp,
                       "Predicted" = results_pnbd_calibrate$DERT * results_gg_calibrate$predicted.mean.spending)
    table = merge(x = table, y = new3[,c("Id", "CLV")], all.x = TRUE)
  
    name = paste("run", i, sep = " ")
  
    parametertable[, (name) := rep(0,675)]
    for (j in nrow(parametertable)){
      a = parametertable[j, 1]
      b = parametertable[j, 2]
      c = parametertable[j, 3]
      d = parametertable[j, 4]
      e = parametertable[j, 5]
      counter = 0
      
      for (k in 1:nrow(table)){
        prediction = table[k,2]
        width = a + b*prediction + c*prediction^2 + d*prediction^3 + e*prediction^4
        lower = prediction - width
        upper = prediction + width
        if (table[k,3] > lower & table[k,3] < upper){
          counter = counter + 1
          print(paste("Prediction:", table[k,3], "  --  Width:", width, "  --  Parameters:", a, b, c, d, e))
        }
      }
      parametertable[j,length(parametertable)] = counter
    }
  }
}


f1 = function(pred, param){
  width = param[1] + param[2] * pred + param[3] * pred^2 + param[4] * pred^3 + param[5] * pred^4
  return(width)
}

f2 = function(pred, param){
  return(param[1] + (pred < 10) * pred * param[2] + (pred >= 10 & pred < 30) * pred * param[3] + (pred >= 30 & pred < 70) * pred * param[4] + (pred >= 70) * pred * param[5])
}

compute_coverage = function(true, pred, param){
  width = f2(pred, param)
  #width = param[1] + param[2] * pred + param[3] * pred^2 + param[4] * pred^3 + param[5] * pred^4
  l = pred - as.numeric(width)
  u = pred + as.numeric(width)
  print(paste("sum(true < u & true > l)/length(true):", sum(true < u & true > l)/length(true)))
  return(sum(true < u & true > l)/length(true))
}

objective = function(param){
  #compute_coverage(new3$CLV, results$predicted.CLV, param)
  width = f2(results$predicted.CLV, param)
  #width = param[1] + param[2] * pred + param[3] * pred^2 + param[4] * pred^3 + param[5] * pred^4
  l = results$predicted.CLV - as.numeric(width)
  u = results$predicted.CLV + as.numeric(width)
  print(paste("sum(true < u & true > l)/length(true):", sum(new3$CLV < u & new3$CLV > l)/length(new3$CLV)))
  sum(new3$CLV < u & new3$CLV > l)/length(new3$CLV)
}

p = rep(10,5)

optim_result <- optimx(par = p, fn = objective, 
                      method = "L-BFGS-B",  # specify the optimization method that supports constraints
                      lower = rep(-Inf,5),  # lower bounds for each parameter
                      upper = rep(Inf,5),  # upper bounds for each parameter
                      control = list(pgtol = 0.000000000001))
                      #control = list(fnscale = -1))  # maximize the objective function
p
optim_result


#######################
p = optim_result$par
compute_coverage2 = function(true, pred, p){
  width = f2(pred,p)
  print(paste("width", width))
  print(paste("param[1]", p[1], "param[2]", p[2], "param[3]", p[3], "pred", pred))
  l = pred - width
  u = pred + width
  results = data.table("true" = true,
                       "pred" = pred,
                       "lower" = l,
                       "upper" = u,
                       #"width" = width,
                       "covered" = true < u & true > l)
  return(results)
}

r = compute_coverage2(new3$CLV, results$predicted.CLV, p)


#######################

library(optimx)
importance = cut(results$predicted.CLV, breaks = c(-Inf, 10, 30, 70, Inf), labels = c(1,2,3,4))
p = rep(100,5)

objectivefn = function(p){
  mean(compute_width(p)) + (covered(p) < 0.8) * 9654344823487
}

compute_width = function(p){
  width = (f2(results$predicted.CLV, p) / results$predicted.CLV) * as.numeric(importance)
  return(width)
}

covered = function(p){
  width = compute_width(p)
  lower = results$predicted.CLV - width
  upper = results$predicted.CLV + width
  inside = (new3$CLV > lower & new3$CLV < upper)
  return(sum(inside)/length(inside))
}

optim_result = optim(par = p, fn = objectivefn,
                     method = "L-BFGS-B",
                     lower = rep(0,5),
                     upper = rep(Inf,5)
                     )

optim_result = optimx(par = p, fn = objectivefn,
                      method = "L-BFGS-B",
                      control = list(fnscale = -1),
                      # lower = rep(0,5),
                      # upper = rep(Inf,5),
                      ui = matrix(covered, nrow = 1), ci = x)

compute_width(optim_result$par)
covered(optim_result$par)

#

compute_width = function(p){
  width = p[1] + 2*p[6] + (results$predicted.CLV < 10) * results$predicted.CLV * p[2] + (results$predicted.CLV >= 10 & results$predicted.CLV < 30) * results$predicted.CLV * p[3] + (results$predicted.CLV >= 30 & results$predicted.CLV < 70) * results$predicted.CLV * p[4] + (results$predicted.CLV >= 70) * results$predicted.CLV * p[5]
  width = width / (results$predicted.CLV * as.numeric(importance))
  print(width)
  return(mean(width))
}

wrapper = function(x) compute_width(x)

covered = function(x){
  width = compute_width(x)
  lower = results$predicted.CLV - width
  upper = results$predicted.CLV + width
  inside = (new3$CLV > lower & new3$CLV < upper)
  return(sum(inside)/length(inside))
}

nlp = OP(F_objective(F = wrapper, n = 1),
         F_constraint(covered, dir = "<=", rhs = 0.8),
         maximum = FALSE)
#ROI_applicable_solvers(nlp)
r = ROI_solve(nlp, solver = "nlminb", start = rep(1,6))

, method = "NLOPT_LD_SLSQP")









