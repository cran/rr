context("Tests regression code (rrreg)")

rm(list=ls())

data(nigeria)

####### FORCED DESIGN ####################

## Define design parameters
p <- 2/3  # probability of answering honestly in Forced Response Design
p1 <- 1/6 # probability of forced 'yes'
p0 <- 1/6 # probability of forced 'no'

## Rescale respondent age covariate
nigeria$cov.age.10 <- nigeria$cov.age/10
nigeria$cov.age.10sq <- nigeria$cov.age.10^2

test_that("rrreg works", {
  skip_on_cran()
  
  #### rrreg
  set.seed(3)
  rr.q1.reg.obj1 <- rrreg(rr.q1 ~ cov.asset.index + cov.married + 
                            cov.age.10 + cov.age.10sq + cov.education + cov.female,   
                          data = nigeria, p = p, p1 = p1, p0 = p0, 
                          design = "forced-known")
  
  summary(rr.q1.reg.obj1)
  
  #change start
  set.seed(1)
  rr.q1.reg.obj2 <- rrreg(rr.q1 ~ cov.asset.index + cov.married + 
                            cov.age.10 + I(cov.age.10^2) + cov.education + cov.female,   
                          data = nigeria, p = p, p1 = p1, p0 = p0, 
                          design = "forced-known", start = c(rep(.02, 7)))
  
  summary(rr.q1.reg.obj2)
  
  #change maxIter
  set.seed(3)
  rr.q1.reg.obj3 <- rrreg(rr.q1 ~ cov.asset.index + cov.married + 
                            cov.age.10 + I(cov.age.10^2) + cov.education + cov.female,   
                          data = nigeria, p = p, p1 = p1, p0 = p0, maxIter = 200, 
                          design = "forced-known")
  
  summary(rr.q1.reg.obj3)
  
  #verbose
  set.seed(2)
  rr.q1.reg.obj4 <- rrreg(rr.q1 ~ cov.asset.index + cov.married + 
                            cov.age.10 + I(cov.age.10^2) + cov.education + cov.female,   
                          data = nigeria, p = p, p1 = p1, p0 = p0, verbose = T,
                          design = "forced-known")
  
  #optim
  set.seed(2)
  rr.q1.reg.obj5 <- rrreg(rr.q1 ~ cov.asset.index + cov.married + 
                            cov.age.10 + I(cov.age.10^2) + cov.education + cov.female,   
                          data = nigeria, p = p, p1 = p1, p0 = p0, optim = TRUE,
                          design = "forced-known")
  
  coef(rr.q1.reg.obj5)
  vcov(rr.q1.reg.obj5)
  
  #em.converge
  set.seed(2)
  rr.q1.reg.obj6 <- rrreg(rr.q1 ~ cov.asset.index + cov.married + 
                            cov.age.10 + I(cov.age.10^2) + cov.education + cov.female,   
                          data = nigeria, p = p, p1 = p1, p0 = p0, em.converge = 10^3,
                          design = "forced-known")
  #glmMaxIter
  set.seed(3)
  rr.q1.reg.obj7 <- rrreg(rr.q1 ~ cov.asset.index + cov.married + 
                            cov.age.10 + I(cov.age.10^2) + cov.education + cov.female,   
                          data = nigeria, p = p, p1 = p1, p0 = p0, glmMaxIter = 50000,
                          design = "forced-known")
  
  #solve.tolerance 
  set.seed(2)
  rr.q1.reg.obj8 <- rrreg(rr.q1 ~ cov.asset.index + cov.married + 
                            cov.age.10 + I(cov.age.10^2) + cov.education + cov.female,   
                          data = nigeria, p = p, p1 = p1, p0 = p0, design = "forced-known",
                          solve.tolerance = 2.220446e-16)
  
  #With only one variable
  set.seed(1)
  rr.q1.reg.obj9 <- rrreg(rr.q1 ~ cov.asset.index, data = nigeria, 
                          p = p, p1 = p1, p0 = p0,
                          design = "forced-known")
  
  #With only intercept
  set.seed(1)
  rr.q1.reg.obj10 <- rrreg(rr.q1 ~ 1, data = nigeria, 
                           p = p, p1 = p1, p0 = p0,
                           design = "forced-known")

  #### predict.rrreg ####
  rr.q1.reg.pred1 <- predict(rr.q1.reg.obj1, given.y = FALSE, 
                             avg = TRUE, quasi.bayes = TRUE, 
                             n.sims = 10000)
  
  #given.y
  rr.q1.reg.pred2 <- predict(rr.q1.reg.obj1, given.y = TRUE, 
                             avg = TRUE, quasi.bayes = TRUE,
                             n.sims = 10000)
  
  #alpha
  rr.q1.reg.pred3 <- predict(rr.q1.reg.obj1, given.y = TRUE, alpha = .1, 
                             avg = TRUE, quasi.bayes = TRUE,
                             n.sims = 10000)
  
  #n.sims
  rr.q1.reg.pred4 <- predict(rr.q1.reg.obj1, given.y = TRUE, 
                             avg = TRUE, quasi.bayes = TRUE, 
                             n.sims = 20000)
  
  #avg
  rr.q1.reg.pred5 <- predict(rr.q1.reg.obj1, given.y = TRUE, 
                             avg = FALSE, quasi.bayes = TRUE, 
                             n.sims = 1000)
  
  #newdata
  rr.q1.reg.pred6 <- predict(rr.q1.reg.obj1, given.y = TRUE, 
                             avg = TRUE, quasi.bayes = TRUE, 
                             newdata = nigeria, 
                             n.sims = 1000)
  
  #newdata without y because given.y = FALSE
  rr.q1.reg.pred6.1 <- predict(rr.q1.reg.obj1, given.y = FALSE, 
                               newdata = nigeria[,c("cov.asset.index", "cov.married", 
                                                    "cov.age", "cov.education", "cov.female", "cov.age.10", "cov.age.10sq")])
  
  #quasi.bayes
  rr.q1.reg.pred7 <- predict(rr.q1.reg.obj1, given.y = TRUE, 
                             avg = TRUE, quasi.bayes = FALSE, 
                             n.sims = 1000)
  
  
  ## MAKE SIMULATIONS FOR PREDICT, ADD TO THE RRREG SIMULATIION
  
  ### rrreg.predictor ### Aside from the usual fitted probabilities error warning, it's good
  set.seed(3)  
  rr.q1.pred.obj1 <- rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                                       cov.education + cov.female + rr.q1, 
                                     rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                                     data = nigeria, verbose = FALSE, 
                                     p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  print(rr.q1.pred.obj1)
  summary(rr.q1.pred.obj1)
  coef(rr.q1.pred.obj1)
  vcov(rr.q1.pred.obj1)
  
  
  #fit.sens
  set.seed(3)
  rr.q1.pred.obj2 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, fit.sens = "glm",
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  print(rr.q1.pred.obj2)
  summary(rr.q1.pred.obj2)
  coef(rr.q1.pred.obj2)
  vcov(rr.q1.pred.obj2)
  
  #fit.outcome
  set.seed(3)
  rr.q1.pred.obj3 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, fit.outcome = "glm",
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  print(rr.q1.pred.obj3)
  summary(rr.q1.pred.obj3)
  coef(rr.q1.pred.obj3)
  vcov(rr.q1.pred.obj3)
  
  #bstart / parstart = FALSE
  set.seed(3)
  rr.q1.pred.obj4 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, bstart = coef(rr.q1.reg.obj1),
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  #tstart
  set.seed(3)
  rr.q1.pred.obj5 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, tstart =c(coef(rr.q1.reg.obj1), .02),
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  #parstart
  set.seed(1)
  rr.q1.pred.obj6 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = TRUE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, 
                    p = p, p1 = p1, p0 = p0, design = "forced-known") 
  
  #maxIter
  set.seed(3)
  rr.q1.pred.obj7 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, maxIter = 10500, 
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  summary(rr.q1.pred.obj7)
  print(rr.q1.pred.obj7)
  
  #verbose
  set.seed(3)
  rr.q1.pred.obj8 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = TRUE, 
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  #optim
  set.seed(3)
  rr.q1.pred.obj9 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, optim = TRUE, 
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  #em.converge
  set.seed(3)
  rr.q1.pred.obj10 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, em.converge = 10^(-3),  
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  #glmMaxIter
  set.seed(3)
  rr.q1.pred.obj11 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = TRUE,
                    data = nigeria, verbose = FALSE, glmMaxIter = 21000,  
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  #estconv
  set.seed(3)
  rr.q1.pred.obj12 <- 
    rrreg.predictor(civic ~ cov.asset.index + cov.married + cov.age.10 + I(cov.age.10^2) + 
                      cov.education + cov.female + rr.q1, 
                    rr.item = "rr.q1", parstart = FALSE, estconv = FALSE,
                    data = nigeria, verbose = FALSE,  
                    p = p, p1 = p1, p0 = p0, design = "forced-known")
  
  
  ### predict.rrreg.predictor ###
  rr.q1.pred.pred1 <- predict(rr.q1.pred.obj1, 
                              avg = TRUE, quasi.bayes = TRUE, 
                              n.sims = 10000)
  #alpha
  rr.q1.pred.pred2 <- predict(rr.q1.pred.obj1, alpha = .1, 
                              avg = TRUE, quasi.bayes = TRUE, 
                              n.sims = 10000)
  
  rr.q1.pred.pred1
  rr.q1.pred.pred2 #se's and CI's are different as they should be
  
  #n.sims
  rr.q1.pred.pred3 <- predict(rr.q1.pred.obj1,  
                              avg = TRUE, quasi.bayes = TRUE, 
                              n.sims = 10500)
  
  #avg
  rr.q1.pred.pred4 <- predict(rr.q1.pred.obj1,  
                              avg = FALSE, quasi.bayes = TRUE, 
                              n.sims = 10000)
  
  #newdata
  rr.q1.pred.pred5 <- predict(rr.q1.pred.obj1, newdata = nigeria,
                              avg = TRUE, quasi.bayes = TRUE, 
                              n.sims = 10000) #works, same est as pred1
  
  #newdata error message, works
  #predict(rr.q1.pred.obj1, newdata = NA,
  #                        avg = TRUE, quasi.bayes = TRUE, 
  #                        n.sims = 10000)
  
  #quasi.bayes
  rr.q1.pred.pred6 <- predict(rr.q1.pred.obj1, 
                              avg = TRUE, quasi.bayes = FALSE, 
                              n.sims = 10000)
  
  #fix.z as value
  rr.q1.pred.pred7 <- predict(rr.q1.pred.obj1, fix.z = .4,
                              avg = TRUE, quasi.bayes = TRUE, 
                              n.sims = 10000)
  
  #fix.z as vector
  rr.q1.pred.pred8 <- predict(rr.q1.pred.obj1, fix.z = rep(.4, nrow(rr.q1.pred.obj1$data)),
                              avg = TRUE, quasi.bayes = TRUE, 
                              n.sims = 10000)
  
  #fix.z as value, probability error
  #predict(rr.q1.pred.obj1, fix.z = 1.4,
  #        avg = TRUE, quasi.bayes = TRUE, 
  #        n.sims = 10000)
  
  
  ### power.rr.test ###
  # changing designs
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, n = 200, 
                presp = .2, presp.null = 0,
                design = "forced-known", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  power.rr.test(p = 2/3, n = 200, 
                presp = .2, presp.null = 0,
                design = "mirrored", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  power.rr.test(p = 2/3, n = 200, 
                presp = .2, presp.null = 0,
                design = "disguised", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  power.rr.test(p = 2/3, q = 1/3, n = 200, 
                presp = .2, presp.null = 0,
                design = "unrelated-known", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, r = .5, n = 200, 
                presp = .2, presp.null = 0,
                design = "forced-unknown", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  power.rr.test(p = 2/3, q = 1/3, r = .5, n = 200, 
                presp = .2, presp.null = 0,
                design = "unrelated-unknown", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  #n
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, n = 2000, 
                presp = .2, presp.null = 0,
                design = "forced-known", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  #presp
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, n = 200, 
                presp = .4, presp.null = 0,
                design = "forced-known", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  #presp.null
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, n = 200, 
                presp = .2, presp.null = .1,
                design = "forced-known", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  #sig.level
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, n = 200, 
                presp = .2, presp.null = 0,
                design = "forced-known", sig.level = .05,
                type = "one.sample", alternative = "one.sided")
  
  #prespT, prespC, prespT.null, prespC.null for type = "two.sample"
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, n = 200, 
                prespT = .2, prespC = .1, prespT.null = 0, prespC.null = 0,
                design = "forced-known", sig.level = .01,
                type = "two.sample", alternative = "one.sided")
  
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, n = 200, 
                prespT = .2, prespC = .1, prespT.null = .1, prespC.null = 0,
                design = "forced-known", sig.level = .01,
                type = "two.sample", alternative = "one.sided")
  
  #power (gives us n), 
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, power = 0.9868872, 
                presp = .2, presp.null = 0,
                design = "forced-known", sig.level = .01,
                type = "one.sample", alternative = "one.sided")
  
  #error that n or power needs to be null
  #power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, power = .8, n = 200,
  #              presp = .2, presp.null = 0,
  #              design = "forced-known", sig.level = .01,
  #              type = "one.sample", alternative = "one.sided")
  
  #alternative
  power.rr.test(p = 2/3, p1 = 1/6, p0 = 1/6, n = 200,
                presp = .2, presp.null = 0,
                design = "forced-known", sig.level = .01,
                type = "one.sample", alternative = "two.sided")
  
})
