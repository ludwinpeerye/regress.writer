regress.writer = function(reg) {
  
 # Ethan J. Ludwin-Peery
 # 2017

 # e.g.:
  #A significant regression equation was found for Purity (F(6,1179) = 38.16, p < .001)
  #, with an adjusted R2 of .547. 
  # Purity was significantly predicted by subjects’ RWA (β = .0, p < .001), self-reported social conservatism (β = .0, p < .001), gender (β = .0, p < .001), and age (β = .0, p < .001).
  
  summ = summary(reg)
  pval = pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE)
  if (pval < .05) {
    cat("A significant regression equation was found (F(", summ$fstatistic[2] , ", ", summ$fstatistic[3], ") = ", round(summ$fstatistic[1], 2), ", p < ", 
        sep = "")
    if (pval < .001) {
      cat(".001")
    }  else if (pval < .01) {
      cat(".01")
    } else {
      cat(".05")
    }
    cat("), with an adjusted R2 of ", round(summ$adj.r.squared, 3) , ". ", sep = "")
    cat("The outcome variable was significantly predicted by subjects’ ")
    
    gr_than_1 = F

    for (i in 2:length(reg$coefficients)) {
      if (coef(summ)[i, 4] < .05) {
        
        if (tail(which((coef(summ)[,4] < .05)  %in% TRUE), 1) == i && gr_than_1 == T) { cat("and ", sep = "") }
        gr_than_1 = T
        
        if (coef(summ)[i, 4] < .001) { coef_p = .001 }
        else if (coef(summ)[i, 4] < .01)  {coef_p = .01}
        else {coef_p = .05}
        
        cat(names(coef(reg))[i], " (β = ", round(coef(summ)[i, 1], 3), ", p < ",coef_p, ")", sep= "")
        
        if (tail(which((coef(summ)[,4] < .05)  %in% TRUE), 1) == i) {
          cat(". ", sep = "")
        } else { cat(", ", sep = "") }
        
      }
    }  
      
      
    } else {
      cat("NS")
    }
    
  }
