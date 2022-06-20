

# Chi2 --------------------------------------------------------------------

chi22 = function (data, alp = 0.5, del = 0.05)
{
  dat <- data
  p <- ncol(dat) - 1
  alpha <- alp
  delta <- del
  d <- 0.1
  inconRate <- incon(dat)
  Disc <- chiM(dat, alpha = alpha)$Disc.data
  while (inconRate < delta) {
    if (alpha <= 0.2)
      break
    alpha <- alpha - d
    Disc <- chiM(Disc, alpha = alpha)$Disc.data
    inconRate <- incon(Disc)
  }
  options(digits = 3)
  eps <- 0.01
  sig <- array(alpha, p)
  d <- 0.05
  cutp = list()
  for (i in 1:p) {
    while (TRUE) {
      val <- value(i, dat, alpha = sig[i]) ########
      Disc <- val$disc
      inconRate <- incon(Disc)
      sig[i] <- sig[i] - d
      if (inconRate > delta || sig[i] <= eps)
        break
    }
    cutp[[i]] <- val$cuts
  }
  return(list(cutp = cutp, Disc.data = Disc))
}


# Modified Chi2 -----------------------------------------------------------

modChi22 = function (data, alp = 0.5) 
{
  dat <- data
  p <- ncol(dat) - 1
  alpha <- alp
  d <- 0.1
  LevO <- LevCon(dat)
  Disc <- chiM(dat, alpha = alpha)$Disc.data
  LevN <- LevCon(Disc)
  e <- 0.01
  while ((LevN - LevO) < e) {
    if (alpha <= 0.2) 
      break
    alpha <- alpha - d
    Disc <- chiM(Disc, alpha = alpha)$Disc.data
    LevN <- LevCon(Disc)
    LevO <- LevN
  }
  options(digits = 3)
  eps <- 1e-04
  sig <- array(alpha, p)
  d <- 10
  cutp = list()
  for (i in 1:p) {
    LevO <- LevCon(Disc)
    while (TRUE) {
      val <- value(i, dat, alpha = sig[i])
      Disc <- val$disc
      LevN <- LevCon(Disc)
      if (LevN < LevO || sig[i] <= eps) 
        break
      LevO <- LevN
      sig[i] <- sig[i]/d
    }
    cutp[[i]] <- val$cuts
  }
  return(list(cutp = cutp, Disc.data = Disc))
}


# Extended Chi2 -----------------------------------------------------------

extendChi22 = function (data, alp = 0.5) 
{
  dat <- data
  p <- ncol(dat) - 1
  alpha <- alp
  d = 0.1
  XiO <- Xi(dat)
  Disc <- chiM(dat, alpha = alpha)$Disc.data
  XiN <- Xi(Disc)
  e <- 0.01
  while ((XiN - XiO) < e) {
    if (alpha <= 0.2) 
      break
    alpha <- alpha - d
    Disc <- chiM(Disc, alpha = alpha)$Disc.data
    XiN <- Xi(Disc)
    XiO <- XiN
  }
  options(digits = 3)
  eps <- 1e-04
  sig <- array(alpha, p)
  d <- 10
  cutp = list()
  for (i in 1:p) {
    XiO <- Xi(Disc)
    while (TRUE) {
      val <- value(i, dat, alpha = sig[i])
      Disc <- val$disc
      XiN <- Xi(Disc)
      XiO <- XiN
      sig[i] <- sig[i]/d
      if (XiN < XiO || sig[i] <= eps) 
        break
    }
    cutp[[i]] <- val$cuts
  }
  return(list(cutp = cutp, Disc.data = Disc))
}
