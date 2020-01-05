predict.regsubset=function(object,newdata,id,...){
  form = as.formula(object$call[[2]]);
  mat=model.matrix(form,newdata);
  coefs=coef(object, id=id);
  xvars=names(coefs);
  mat[,xvars]%*%coefs
}