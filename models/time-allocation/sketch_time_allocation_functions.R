data.frame(
  difficulty = seq(0, 1, 0.0001)
) -> df_tasks

1:nrow(df_tasks) /
  nrow(df_tasks) ->
  df_tasks$
  index

df_tasks$
  difficulty ->
  df_tasks$
  linear

1 / nrow(df_tasks) ->
  df_tasks$
  const

df_tasks$
  difficulty |>
  exp() ->
  df_tasks$
  exp

(1 +
  df_tasks$
  difficulty) |>
  log() ->
  df_tasks$
  log

do.call(
  function(x){(1 + exp(-30 * (x - 0.5))) ^ -1}
  , args = list(df_tasks$difficulty)
) ->
  df_tasks$
  logistic

do.call(
  function(x){x ^ (1 / (1 - 0.9))}
  , args = list(df_tasks$difficulty)
) ->
  df_tasks$
  competent

do.call(
  function(x){x ^ (1 / (1 - 0.5))}
  , args = list(df_tasks$difficulty)
) ->
  df_tasks$
  midwit

do.call(
  function(x){x ^ (1 / (1 - 0.25))}
  , args = list(df_tasks$difficulty)
) ->
  df_tasks$
  incompetent

do.call(
  function(x){x ^ (1 / (1 - 0))}
  , args = list(df_tasks$difficulty)
) ->
  df_tasks$
  loser

# time to complete a task vs task difficulty
ggplot2::qplot(x = df_tasks$index, y = df_tasks$linear, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$const, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$exp, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$log, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$logistic, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$competent, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$midwit, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$incompetent, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$loser, geom = 'line', xlab = 'task difficulty', ylab = 'time to complete')

# time allocation vs task difficulty
ggplot2::qplot(x = df_tasks$index, y = df_tasks$linear / sum(df_tasks$linear), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$const / sum(df_tasks$const), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$exp / sum(df_tasks$exp), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$log / sum(df_tasks$log), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$logistic / sum(df_tasks$logistic), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$competent / sum(df_tasks$logistic), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$midwit / sum(df_tasks$logistic), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$incompetent / sum(df_tasks$logistic), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')
ggplot2::qplot(x = df_tasks$index, y = df_tasks$loser / sum(df_tasks$logistic), geom = 'line', xlab = 'task difficulty', ylab = 'time allocation')

# time allocation distribution
ggplot2::qplot(df_tasks$linear / sum(df_tasks$linear), geom = 'density', xlim = c(0,max(df_tasks$linear / sum(df_tasks$linear))), xlab = 'time allocation', ylab = 'frequency')
# ggplot2::qplot(df_tasks$const / sum(df_tasks$const), geom = 'density', xlim = c(0,max(df_tasks$const / sum(df_tasks$const))), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$const / sum(df_tasks$const), geom = 'histogram', xlim = c(0, 1.1 * max(df_tasks$const / sum(df_tasks$const))), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$exp / sum(df_tasks$exp), geom = 'density', xlim = c(0,max(df_tasks$exp / sum(df_tasks$exp))), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$log / sum(df_tasks$log), geom = 'density', xlim = c(0,max(df_tasks$log / sum(df_tasks$log))), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$logistic / sum(df_tasks$logistic), geom = 'density', xlim = c(0,max(df_tasks$logistic / sum(df_tasks$logistic))), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$competent / sum(df_tasks$competent), geom = 'density', xlim = c(0,max(df_tasks$competent / sum(df_tasks$competent))), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$midwit / sum(df_tasks$midwit), geom = 'density', xlim = c(0,max(df_tasks$midwit / sum(df_tasks$midwit))), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$incompetent / sum(df_tasks$incompetent), geom = 'density', xlim = c(0,max(df_tasks$incompetent / sum(df_tasks$incompetent))), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$loser / sum(df_tasks$loser), geom = 'density', xlim = c(0,max(df_tasks$loser / sum(df_tasks$loser))), xlab = 'time allocation', ylab = 'frequency')

ggplot2::qplot(df_tasks$linear / sum(df_tasks$linear), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$const / sum(df_tasks$const), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$const / sum(df_tasks$const), geom = 'histogram', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$exp / sum(df_tasks$exp), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$log / sum(df_tasks$log), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$logistic / sum(df_tasks$logistic), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$competent / sum(df_tasks$competent), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$midwit / sum(df_tasks$midwit), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$incompetent / sum(df_tasks$incompetent), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
ggplot2::qplot(df_tasks$loser / sum(df_tasks$loser), geom = 'density', xlim = c(0,1), xlab = 'time allocation', ylab = 'frequency')
