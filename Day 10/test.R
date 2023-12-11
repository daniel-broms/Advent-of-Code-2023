data10 <- unname(unlist(read.fwf("Day 10/input.txt", widths = rep(1L, 140L))))
tl <- list(L = c(-1i, 1), J = c(-1i, -1), "7" = c(1i, -1), F = c(1i, 1)) #turn list

lp <- ((which(data10 == "S") - 1) %% 140 + 1)* 1i + which(data10 == "S") %/% 140
dir <- 1i #found manually by looking at input

for (k in seq_len(1e5) + 1L) {
  lp[k] <- lp[k - 1L] + dir
  cur <- data10[Re(lp[k]) * 140 + Im(lp[k])]
  if (cur %in% c("L", "J", "7", "F")) {
    dir <- tl[[cur]][abs(Re(dir) + 2 * Im(dir))]
  } else if (cur == "S") break
  
}
#part1-------
(k - 1L) / 2L

#part 2------- 495
ar <- sum((Im(lp)[-length(lp)] + Im(lp[-1])) * diff(Re(lp))) / 2L
abs(ar) + 1L - (k - 1L) / 2L


