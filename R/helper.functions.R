# JW: 0.0.26 2022-08-31: error in labelsM removed
# MH: 0.0.24 2022-07-27: new version of helper.functions.R (2022-07-27 14:50)
# MH: 0.0.23 2022-07-27: disabled browser in line 411
# JW: 0.0.22 2022-07-25

# for labels of target parameter input widgets; output: list
labelsL <- function(procNames, paramClass, measMod){ #  procNames=input$procNames_List, measMod=input$measModel
  nP<- length(procNames)
  labels <- list()
  count <- 0
    switch(paramClass,
           
         "ARCL" = {
           procNb <- nP
           # diagonal
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){
                 count <- count + 1
                 labels[[count]] <- paste0("AR_", procNames[i])
                 names(labels)[[count]] <- unlist(procNames[i])
               }
             }
           }
           # off-diagonal
           for (i in 1:procNb){
             for (j in 1:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0("CL_", procNames[i], "_", procNames[j]) 
                 names(labels)[[count]] <- paste0(procNames[j], " → ", procNames[i]) # bc column -> row
               }
             }
           }
         },
         
         "RES" = { 
           procNb <- nP
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- unlist(procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
                 names(labels)[[count]] <- paste0(procNames[i], " ↔ ", procNames[j])
               }
             }
           }
         },
         
         "I" = { 
           procNb <- nP
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- unlist(procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
                 names(labels)[[count]] <- paste0(procNames[i], " ↔ ", procNames[j])
               }
             }
           }
         },
         
         "S" = { 
           procNb <- nP
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- unlist(procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
                 names(labels)[[count]] <- paste0(procNames[i], " ↔ ", procNames[j])
               }
             }
           }
         },
         
         "A" = { 
           procNb <- nP
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- unlist(procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
                 names(labels)[[count]] <- paste0(procNames[i], " ↔ ", procNames[j])
               }
             }
           }
         },
         
         "B" = { 
           procNb <- nP
           # diagonal (var)
           for (i in 1:procNb){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i])
                 names(labels)[[count]] <- unlist(procNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:procNb){
             for (j in i:procNb){ #!
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
                 names(labels)[[count]] <- paste0(procNames[i], " ↔ ", procNames[j])
               }
             }
           }
         },
         
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
                 labels[[count]] <- paste0(paramClass, indNames[i])
                 names(labels)[[count]] <- unlist(indNames[i])
           }
           # off-diagonal (cov)
           for (i in 1:indNb){
             for (j in i:indNb){
               if (i != j){
                 count <- count + 1
                 labels[[count]] <- paste0(paramClass, indNames[i], " - ", indNames[j])
                 names(labels)[[count]] <- paste0(indNames[i], " ↔ ", indNames[j])
               }
             }
           }
         }, 
         
         "AB" = {
           procNb <- nP
           preNames <- c()
           for (i in 1:procNb){
             count <- count + 1
             preNames[count] <- paste0("A_", procNames[i])
             count <- count + 1
             preNames[count] <- paste0("B_", procNames[i])
           }
           count <- 0
           ABNb <- length(preNames)
           for (i in 1:ABNb){
             for (j in i:ABNb){
               if (i != j){ # only off-diagonal
                 count <- count + 1
                 labels[[count]] <- paste0(preNames[i], "_", preNames[j])
                 names(labels)[[count]] <- paste0(preNames[i], " ↔ ", preNames[j]) 
               }
             }
           }
         }, 
         
         "IS" = {
           procNb <- nP
           preNames <- c()
           for (i in 1:procNb){
             count <- count + 1
             preNames[count] <- paste0("I_", procNames[i])
             count <- count + 1
             preNames[count] <- paste0("S_", procNames[i])
           }
           count <- 0
           ISNb <- length(preNames)
           for (i in 1:ISNb){
             for (j in i:ISNb){
               if (i != j){ # only off-diagonal
                 count <- count + 1
                 labels[[count]] <- paste0(preNames[i], "_", preNames[j])
                 names(labels)[[count]] <- paste0(preNames[i], " ↔ ", preNames[j])
               }
             }
           }
           })
    return(labels)
}

# for names of params in backend; output: matrix
labelsM <- function(procNames, paramClass, measMod){ # use names, not numbers!
  procNb <- length(procNames) # all quadratic matrices, but not all symmetric´
  labels <- matrix("", nrow=procNb, ncol=procNb) # später modifizieren wenn multiple indicators!
  switch(paramClass,
         
         "ARCL" = {
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){ # diagonal
                 labels[i, j] <- paste0("AR_", procNames[i])
               } else { # off-diagonal
                 labels[i, j] <- paste0("CL_", procNames[i], "_", procNames[j]) 
               }
             }
           }
         },
         
         "RES" = {
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
               }
             }
           }
         },
         
         "I" = {
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
               }
             }
           }
         },
         
         "S" = {
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
               }
             }
           }
         },
         
         "A" = {
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
               }
             }
           }
         },
         
         "B" = {
           for (i in 1:procNb){
             for (j in 1:procNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i])
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(paramClass, "_", procNames[i], "_", procNames[j])
               }
             }
           }
         },
         
         "UNIQ" = { # andere dimension!
           measModelNb <- length(measMod) # names of processes with measurement model
           indNames <- c()
           for (i in seq(measModelNb)) {
             indNb <- 1
             #indNb <- get("input")[[paste0("indicat_", measMod[i])]] 
             tmp <- c()
             for (n in 1:indNb){
               tmp[n] <- paste0(measMod[i], "_", n)
             }
             indNames <- append(indNames, tmp)
           }
           indNames <- indNames[-1] # otherwise first element is primitive("c")
           indNb <- length(indNames)
           labels <- matrix("", nrow=indNb, ncol=indNb) # other dimensions thatn first initialization!
           for (i in 1:indNb){
             for (j in 1:indNb){
               if (i == j){ # diagonal (var)
                 labels[i, j] <- indNames[i]
               } else { # off-diagonal (cov)
                 labels[i, j] <- paste0(indNames[i], " - ", indNames[j])
               }
             }
           }
         }, 
         
         "AB" = {
           count <- 0
           preNames <- c()
           for (i in 1:procNb){
             count <- count + 1
             preNames[count] <- paste0("A_", procNames[i])
             count <- count + 1
             preNames[count] <- paste0("B_", procNames[i])
           }
           ABNb <- length(preNames)
           labels <- matrix("", nrow=ABNb, ncol=ABNb)
           for (i in 1:ABNb){
             for (j in 1:ABNb){
               if (i == j){ # diagonal
                 labels[i, j] <- preNames[i]
               } else { # off-diagonal
                 labels[i, j] <- paste0(preNames[i], "_", preNames[j])
               }
             }
           }
         }, 
         
         "IS" = {
           count <- 0
           preNames <- c()
           for (i in 1:procNb){
             count <- count + 1
             preNames[count] <- paste0("I_", procNames[i])
             count <- count + 1
             preNames[count] <- paste0("S_", procNames[i])
           }
           ISNb <- length(preNames)
           labels <- matrix("", nrow=ISNb, ncol=ISNb)
           for (i in 1:ISNb){
             for (j in 1:ISNb){
               if (i == j){ # diagonal
                 labels[i, j] <- preNames[i]
               } else { # off-diagonal
                 labels[i, j] <- paste0(preNames[i], "_", preNames[j])
               }
             }
           }
         })
  return(labels)
}

compute_results <- function(budget,
                            alpha,
                            costN,
                            minN,
                            maxN,
                            costT,
                            minT,
                            maxT,
                            modelClass,
                            procNames,
                            measModel,
                            popSize,
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
                            targetAB){
  procNames_List <-
    as.list(unlist(strsplit(procNames, split = "\\, |\\,| ")))
  nP <- length(procNames_List)
  nM <- length(measModel)
  ARCL <- matrix(as.numeric(ARCL), ncol=nP)
  RES <- matrix(as.numeric(RES), ncol=nP)
  RES[upper.tri(RES)] <- t(RES)[upper.tri(RES)]
  UNIQ <- matrix(as.numeric(UNIQ), ncol=nM)
  UNIQ[upper.tri(UNIQ)] <- t(UNIQ)[upper.tri(UNIQ)]
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
         "fclpm" = {
           input_H1$Psi <- list(values = UNIQ,
                               labels = labelsM(NULL, "UNIQ", measModel))
           target.parameters <- c(target.parameters, targetUNIQ)
           target.parameters.values.h0 <- rep(0, length(target.parameters))
         },
         "ri-clpm" = {
           input_H1$Theta_I <- list(values = I,
                                   labels = labelsM(procNames_List, "I"))
           target.parameters <- c(target.parameters, targetI)
           target.parameters.values.h0 <- rep(0, length(target.parameters))
         },
         "starts" = {
           input_H1$Psi <- list(values = UNIQ,
                               labels = labelsM(NULL, "UNIQ", measModel))
           input_H1$Theta_I <- list(values = I,
                                   labels = labelsM(procNames_List, "I"))
           target.parameters <- c(target.parameters, targetUNIQ, targetI)
           target.parameters.values.h0 <- rep(0, length(target.parameters))
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
           input_H1$Psi <- list(values = UNIQ,
                               labels = labelsM(NULL, "UNIQ", measModel))
           input_H1$Theta_A <- list(values = A,
                                   labels = labelsM(procNames_List, "A"))
           target.parameters <- c(target.parameters, targetUNIQ, targetA)
           target.parameters.values.h0 <- rep(0, length(target.parameters))
         }
  )
  
  specs <- list(input_H1 = input_H1,
                N = NULL,
                timepoints = NULL,
                names_process = NULL,
                names_ov = NULL)
  
  if (length(target.parameters.values.h0) != 0){
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
        "T.integer" = TRUE,
        "N.integer" = FALSE
      ),
      genoud = list(
        "pop.size" = popSize,
        "max.generations" = 100,
        "wait.generations" = 1,
        "boundary.enforcement" = 2,
        "solution.tolerance" = 0.001
      ),
      verbose = FALSE
    )
  } else {
    res <- NULL
  }
  
  testing <- list(specs=specs,
                  res=res)
  
  return(testing)
}