# MA 0.1.73 2023-06-12: added F_ML()
# JW: 1.1.69: changed order of CL effect names (reviewer comment)
# JW: 0.0.43 2022-11-02: fixed compute_results() error when no measModel
#                        pop.size.max added in compute_results()
#                        compute_results() got new param minTidentify for error checking in backend
# JW: 0.0.36 2022-10-06: corrected labelL for IS and AB and convention labels symmetrical matrices covs
# MH: 0.0.35 2022-10-05: moved log.data out of genoud list
# JW: 0.0.31 2022-10-05: dbLog added, if clause around optmze() removed for error catching within
# JW: 0.0.30 2022-10-04: coherent labeling in off-diags of labelsM in symmetrical matrices 
# JW: 0.0.29 2022-09-02: error in IS and AB matrices, and RES, corrected (i.e., labelsL and labelsM); for testing, target.params and their values also in output 
# JW: 0.0.26 2022-08-31: error in labelsM corrected
# MH: 0.0.24 2022-07-27: new version of helper.functions.R (2022-07-27 14:50)
# MH: 0.0.23 2022-07-27: disabled browser in line 411
# JW: 0.0.22 2022-07-25

## frontend & backend
# for labels of target parameter input widgets in frontend
# output: named list
# form: e.g. user see's "proc" (frontend) but saved as "AR_proc1" (backend)
labelsL <- function(procNames, paramClass, measMod){ 
  procNb <- length(procNames)
  labels <- list()
  count <- 0
    switch(paramClass,
           
         ## Gamma
         "ARCL" = {
           # diagonal
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){
                 count <- count + 1
                 labels[[count]] <- paste0("AR_", procNames[i])
                 names(labels)[[count]] <- paste0("AR ", procNames[i])
               }
             }
           }
           # off-diagonal
           for (i in 1:procNb){
             for (j in 1:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0("CL_", procNames[j], "_", procNames[i]) 
                 names(labels)[[count]] <- paste0("CL ", procNames[j], " → ", procNames[i]) # bc column -> row
               }
             }
           }
         },
         
         ## Omega
         "RES" = { 
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[j], " ↔ ", procNames[i])
               }
             }
           }
         },
         
         ## Theta_i
         "I" = { 
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[j], " ↔ ", procNames[i])
               }
             }
           }
         },
         
         ## Theta_S
         "S" = { 
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[j], " ↔ ", procNames[i])
               }
             }
           }
         },
         
         ## Theta_A
         "A" = { 
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[j], " ↔ ", procNames[i])
               }
             }
           }
         },
         
         ## Theta_B
         "B" = { 
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", procNames[j], " ↔ ", procNames[i])
               }
             }
           }
         },
         
         ## Psi
         "UNIQ" = { # only single-indicator for now
           measModelNb <- length(measMod) 
           # indNames <- c()
           # for (i in seq(measModelNb)) {
           #   indNb <- get("input")[[paste0("indicat_", measMod[i])]] 
           #   tmp <- c()
           #    for (n in 1:indNb){
           #     tmp[n] <- paste0(measMod[i], "_", n)
           #    }
           #   indNames <- append(indNames, tmp)
           # }
           # indNames <- indNames[-1] # otherwise first element is primitive("c")
           # indNb <- length(indNames)
           indNb <- measModelNb
           indNames <- measMod
           # diagonal (var)
           for (i in 1:indNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", indNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", indNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:indNb){
             for (j in i:indNb){
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", indNames[j], "_", indNames[i])
                 names(labels)[[count]] <- paste0(paramClass, " ", indNames[j], " ↔ ", indNames[i])
               }
             }
           }
         }, 
         
         ## Theta_AB
         "AB" = {
           for (i in 1:procNb){
             for (j in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0("A_", procNames[i], "_", "B_", procNames[j])
                 names(labels)[[count]] <- paste0("A ", procNames[i], " ↔ ", "B ", procNames[j])
             }
           }
         }, 
         
         ## Theta_IS
         "IS" = {

           for (i in 1:procNb){
             for (j in 1:procNb){
               count <- count + 1
               labels[[count]] <- paste0("I_", procNames[i], "_", "S_", procNames[j])
               names(labels)[[count]] <- paste0("A ", procNames[i], " ↔ ", "B ", procNames[j])
             }
           }
         })
    return(labels)
}

## backend
# for names of params in backend; 
# output: matrix
# form: e.g. "AR_proc1"
labelsM <- function(procNames, paramClass, measMod){ 
  procNb <- length(procNames) 
  labels <- matrix("", nrow=procNb, ncol=procNb) # später modifizieren wenn multiple indicators!
  switch(paramClass,
         
         ## Gamma
         "ARCL" = {
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){ # diagonal
                 labels[i, j] <- paste0("AR_", procNames[i])
               } else { # off-diagonal
                 labels[i, j] <- paste0("CL_", procNames[j], "_", procNames[i]) 
               }
             }
           }
         },
         
         ## Omega
         "RES" = {
           for (i in 1:procNb){
             for (j in i:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
               }
             }
           }
           labels[lower.tri(labels)] <- t(labels)[lower.tri(labels)]
         },
         
         ## Theta_I
         "I" = {
           for (i in 1:procNb){
             for (j in i:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
               }
             }
           }
           labels[lower.tri(labels)] <- t(labels)[lower.tri(labels)]
         },
         
         ## Theta_S
         "S" = {
           for (i in 1:procNb){
             for (j in i:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
               }
             }
           }
           labels[lower.tri(labels)] <- t(labels)[lower.tri(labels)]
         },
         
         ## Theta_A
         "A" = {
           for (i in 1:procNb){
             for (j in i:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
               }
             }
           }
           labels[lower.tri(labels)] <- t(labels)[lower.tri(labels)]
         },
         
         ## Theta_B
         "B" = {
           for (i in 1:procNb){
             for (j in i:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[j], "_", procNames[i])
               }
             }
           }
           labels[lower.tri(labels)] <- t(labels)[lower.tri(labels)]
         },
         
         ## Psi
         "UNIQ" = { # andere dimension!
           measModelNb <- length(measMod) # names of processes with measurement model
           indNames <- c()
           # for (i in seq(measModelNb)) {
           #   indNb <- 1
             ### for later use with diff nb of indicators
             #indNb <- get("input")[[paste0("indicat_", measMod[i])]] 
             # tmp <- c()
             # for (n in 1:indNb){
             #   tmp[n] <- paste0(measMod[i], "_", n)
             # }
             # indNames <- append(indNames, tmp)
           # }
           # indNames <- indNames[-1] # otherwise first element is primitive("c") 
           # indNb <- length(indNames)
          
         indNb <- measModelNb
         indNames <- measMod
           labels <- matrix("", nrow=indNb, ncol=indNb) # other dimensions than first initialization!
           for (i in 1:indNb){
             for (j in i:indNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", indNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", indNames[j], "_", indNames[i])
               }
             }
           }
           labels[lower.tri(labels)] <- t(labels)[lower.tri(labels)]
         }, 
         
         ## Theta_AB
         "AB" = {
           ANames <- c()
           BNames <- c()
           for (i in 1:procNb){
             ANames[i] <- paste0("A_", procNames[i])
             BNames[i] <- paste0("B_", procNames[i])
           }
           labels <- matrix("", nrow=procNb, ncol=procNb)
           for (i in 1:procNb){
             for (j in 1:procNb){
               labels[i, j] <- paste0(ANames[i], "_", BNames[j])
             }
           }
         }, 
         
         ## Theta_IS
         "IS" = {
           INames <- c()
           SNames <- c()
           for (i in 1:procNb){
             INames[i] <- paste0("I_", procNames[i])
             SNames[i] <- paste0("S_", procNames[i])
           }
           labels <- matrix("", nrow=procNb, ncol=procNb)
           for (i in 1:procNb){
             for (j in 1:procNb){
               labels[i, j] <- paste0(INames[i], "_", SNames[j])
             }
           }
         })
  return(labels)
}

# frontend - backend connection
compute_results <- function(budget,
                            alpha,
                            costN,
                            minN,
                            maxN,
                            costT,
                            minT,
                            maxT,
                            minTidentify,
                            modelClass,
                            procNames,
                            measModel,
                            ARCL,
                            targetARCL,
                            RES,
                            targetRES,
                            UNIQ,
                            targetUNIQ,
                            I,
                            targetI,
                            S,
                            targetS,
                            IS,
                            targetIS,
                            A,
                            targetA,
                            B,
                            targetB,
                            AB,
                            targetAB,
                            popSize,
                            dbLog
                            ){
  procNames_List <-
    as.list(unlist(strsplit(procNames, split = "\\, |\\,| ")))
  nP <- length(procNames_List)
  if (!is.null(measModel)){ # if meas model specified
    nM <- length(measModel)
    UNIQ <- matrix(as.numeric(UNIQ), ncol=nM)
    UNIQ[upper.tri(UNIQ)] <- t(UNIQ)[upper.tri(UNIQ)]
    }
  ARCL <- matrix(as.numeric(ARCL), ncol=nP)
  RES <- matrix(as.numeric(RES), ncol=nP)
  RES[upper.tri(RES)] <- t(RES)[upper.tri(RES)]
  I <- matrix(as.numeric(I), ncol=nP)
  I[upper.tri(I)] <- t(I)[upper.tri(I)]
  S <- matrix(as.numeric(S), ncol=nP)
  S[upper.tri(S)] <- t(S)[upper.tri(S)]
  IS <- matrix(as.numeric(IS), ncol=nP*2)
  IS[upper.tri(IS)] <- t(IS)[upper.tri(IS)]
  diag(IS) <- 0
  A <- matrix(as.numeric(A), ncol=nP)
  A[upper.tri(A)] <- t(A)[upper.tri(A)]
  B <- matrix(as.numeric(B), ncol=nP)
  B[upper.tri(B)] <- t(B)[upper.tri(B)]
  AB <- matrix(as.numeric(AB), ncol=nP*2)
  AB[upper.tri(AB)] <- t(AB)[upper.tri(AB)]
  diag(AB) <- 0
  
  # standard (i.e., CLPM)
  input_H1 <- list(
    model = modelClass,
    alpha = alpha,
    n_ov = nP,
    Gamma = list(values = ARCL, 
                 labels = labelsM(procNames_List, "ARCL")),
    Omega = list(values = RES,
                 labels = labelsM(procNames_List, "RES")),
    Psi = NULL,
    Theta_I = NULL,
    Theta_S = NULL,
    Theta_IS = NULL,
    Theta_A = NULL,
    Theta_B = NULL,
    Theta_AB = NULL
  )
  target.parameters <- c(targetARCL, targetRES)
  target.parameters.values.h0 <- rep(0, length(target.parameters)) # for now every H0_param=0
  
  # how to use switch with code blocks: https://stackoverflow.com/questions/7825501/switch-statement-usage
  switch(modelClass,
         "fclpm" = { ########### erstmal nur hier testen ob problem mit UNIQ
           if (!is.null(measModel)){
             input_H1$Psi <- list(values = UNIQ,
                                  labels = labelsM(NULL, "UNIQ", measModel))
             target.parameters <- c(target.parameters, targetUNIQ)
             target.parameters.values.h0 <- rep(0, length(target.parameters))
           }
         },
         "ri-clpm" = {
           input_H1$Theta_I <- list(values = I,
                                   labels = labelsM(procNames_List, "I"))
           target.parameters <- c(target.parameters, targetI)
           target.parameters.values.h0 <- rep(0, length(target.parameters))
         },
         "starts" = {
           if (!is.null(measModel)){
             input_H1$Psi <- list(values = UNIQ,
                                  labels = labelsM(NULL, "UNIQ", measModel))
             input_H1$Theta_I <- list(values = I,
                                      labels = labelsM(procNames_List, "I"))
             target.parameters <- c(target.parameters, targetUNIQ, targetI)
             target.parameters.values.h0 <- rep(0, length(target.parameters))
           } else {
             input_H1$Theta_I <- list(values = I,
                                      labels = labelsM(procNames_List, "I"))
             target.parameters <- c(target.parameters, targetI)
             target.parameters.values.h0 <- rep(0, length(target.parameters))
           }
         },
         "lcm-sr" = {
           input_H1$Theta_I <- list(values = I,
                                   labels = labelsM(procNames_List, "I"))
           input_H1$Theta_S <- list(values = S,
                                   labels = labelsM(procNames_List, "S"))
           input_H1$Theta_IS = list(values = IS,
                                    labels = labelsM(procNames_List, "IS"))
           target.parameters <- c(target.parameters, targetI, targetS, targetIS)
           target.parameters.values.h0 <- rep(0, length(target.parameters))
         },
         "alt" = {
           input_H1$Theta_A <- list(values = A,
                                   labels = labelsM(procNames_List, "A"))
           input_H1$Theta_B <- list(values = B,
                                   labels = labelsM(procNames_List, "B"))
           input_H1$Theta_AB <- list(values = AB,
                                    labels = labelsM(procNames_List, "AB"))
           target.parameters <- c(target.parameters, targetA, targetB, targetAB)
           target.parameters.values.h0 <- rep(0, length(target.parameters))
         },
         "lcs" = {
           if (!is.null(measModel)){
             input_H1$Psi <- list(values = UNIQ,
                                  labels = labelsM(NULL, "UNIQ", measModel))
             input_H1$Theta_A <- list(values = A,
                                      labels = labelsM(procNames_List, "A"))
             target.parameters <- c(target.parameters, targetUNIQ, targetA)
             target.parameters.values.h0 <- rep(0, length(target.parameters))
           } else {
             input_H1$Theta_A <- list(values = A,
                                      labels = labelsM(procNames_List, "A"))
             target.parameters <- c(target.parameters, targetA)
             target.parameters.values.h0 <- rep(0, length(target.parameters))
           }
         }
  )

  specs <- list(input_H1 = input_H1,
                N = NULL,
                timepoints = NULL,
                names_process = NULL,
                names_ov = NULL)
  
    res <- optmze(
      model = list(
        "specification" = specs,
        "target.parameters" = target.parameters,
        "target.parameters.values.h0" = target.parameters.values.h0
      ),
      study = list(
        "budget" = budget,
        "target.power" = 0.80,
        "l2.cost" = costN,
        "l1.cost" = costT,
        alpha = alpha,
        T = 8 ######### vorher: T = 8
      ),
      optimize = list(
        "what" = c("power"),
        "direction" = c("max"),
        "via" = c("power"),
        "par" = c("T"),
        "via.function" = c("calculate.power.LRT"),
        "optimizer" = c("genoud"),
        "starting.values" = "round(mean(c(par.min.set,par.max.set)))",
        "set.seed.value" = "random"
      ),
      constraints = list(
        "T.min" = minT,
        "T.max" = maxT,
        "N.min" = minN,
        "N.max" = maxN,
        "T.min.identify"=minTidentify,
        "T.integer" = TRUE,
        "N.integer" = FALSE
      ),
      genoud = list(
        "pop.size" = popSize,
        "pop.size.max"=1000, # max from sliderInput(inputId = "popSize")
        "max.generations" = 100,
        "wait.generations" = 1,
        "boundary.enforcement" = 2,
        "solution.tolerance" = 0.001
      ),
      log.data=dbLog, # MH 0.0.35 2022-10-05, moved log.data out of genoud list
	  verbose = FALSE
    )
    
  # for dev output in testing phase:
  testing <- list(specs=specs,
                  target.parameters=target.parameters,
                  target.parameters.values.h0=target.parameters.values.h0,
                  res=res)
  
  return(testing)
}

F_ML <- function(S, Sigma, N) {
  S <- S * (N - 1) / N
  log(det(Sigma)) + sum(diag(S %*% solve(Sigma))) - log(det(S)) - nrow(S)
}
