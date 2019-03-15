

mcBFtest <-function(x, y, method, MC)
{

  if (missing(MC)){

    if (missing(method)){t.test(x, y, var.equal=TRUE)
    }
    else if (method=="t"){t.test(x, y, var.equal=TRUE)
    }
    else if (method=="W"){t.test(x, y, var.equal=FALSE)
    }}

  else if (method=="Monte Carlo"){

    n1 <- length(x)
    n2 <- length(y)
    df <- n1+n2-2

    obs <- t.test(x, y, var.equal=TRUE)
    obs.T <- obs$statistic

    mc <- MC
    lam <- (var(x)/n1)/(var(x)/n1+var(y)/n2)

    MC.T <- rnorm(mc)/sqrt(lam*rchisq(mc,n1-1)/(n1-1)+ (1-lam)*rchisq(mc,n2-1)/(n2-1))
    p.value = mean(abs(MC.T) >= abs(obs.T))

    print("Monte Carlo Two Sample t-test")
    list(p.value=p.value,
         c("mean of x:"=mean(x),"mean of y:"=mean(y)))

  }
}

