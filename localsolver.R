library(localsolver)

data <- list(
  players = length(d$id),
  teams = 10L,
  qb = as.numeric(d$Position %in% 'QB'),
  rb = as.numeric(d$Position %in% 'RB'),
  wr = as.numeric(d$Position %in% 'WR'),
  te = as.numeric(d$Position %in% 'TE'),
  k = as.numeric(d$Position %in% 'K'),
  def = as.numeric(d$Position %in% 'DEF'),
  FFPTs = d$FFPTs
  )

ip <- "function model() {

  x[1..players][1..teams] <- bool();

  flex[p] <- rb[p] + wr[p] + te[p];
  
  for [p in 1..players]
    constraint sum[t in 1..teams](x[p][t]) <= 1;
  
  for [t in 1..teams]
    constraint sum[p in 1..players](x[p][t]) <= 16;
  
  for [t in 1..teams] {
    constraint sum[p in 1..players](qb[p] * x[p][t]) >= 1;
    constraint sum[p in 1..players](rb[p] * x[p][t]) >= 2;
    constraint sum[p in 1..players](wr[p] * x[p][t]) >= 2;
    constraint sum[p in 1..players](te[p] * x[p][t]) >= 1;
    constraint sum[p in 1..players](k[p] * x[p][t]) >= 1;
    constraint sum[p in 1..players](def[p] * x[p][t]) >= 1;
    constraint sum[p in 1..players](flex[p] * x[p][t]) >= 6;
  }
  
  maximize sum[p in 1..players][t in 1..teams](FFPTs[p] * x[p][t]);
}"

lsp <- ls.problem(ip)
lsp <- set.params(lsp = lsp, lsTimeLimit = 100)
lsp <- set.params(lsp = lsp, lsNbThreads = 1)
print(lsp)
lsp.solution <- ls.solve(lsp = lsp, data = data)

