parameter_to_expression <- function(parameter) {
  if (parameter == "beta_n") {
    return(bquote(bold(beta[N])))
  } else if (parameter == "beta_phi") {
    return(bquote(bold(beta[Phi])))
  } else if (parameter == "lambda") {
    return(bquote(bold(lambda[0])))
  } else if (parameter == "mu") {
    return(bquote(bold(mu[0])))
  }
}

model_to_label <- function(model) {
  if (model == "ddd") {
    return(bquote(bold(beta[N])))
  } else if (model == "pd") {
    return(bquote(bold(beta[Phi])))
  } else if (model == "ed") {
    return(bquote(bold(lambda[0])))
  } else if (model == "nnd") {
    return(bquote(bold(mu[0])))
  }
}