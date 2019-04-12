L_error_cannot_apply_non_clos:
INFO;
SHOW("ERROR!(applic) tried to apply a non procedure",FPARG(1));
exit(-1);

L_error_lambda_args_count:
INFO;
SHOW("ERROR!(lambda-simple) number of arguments sent is incorrect",FPARG(1));
exit(-1);
L_error_lambda_opt_args_count:
INFO;
SHOW("ERROR!(lambda opt) number of arguments sent is incorrect",FPARG(1));
exit(-1);
