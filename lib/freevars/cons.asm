JUMP(L_cons_closure)
L_cons_body:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1),IMM(2));
  JUMP_NE(L_error_incorrect_num_args);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), T_PAIR);
  MOV(INDD(R0, 1), FPARG(2));
  MOV(INDD(R0, 2), FPARG(3));
  POP(FP);
  RETURN;
L_cons_closure:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(01));
    MOV(INDD(R0,3),LABEL(L_cons_body));


