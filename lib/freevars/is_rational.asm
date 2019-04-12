JUMP(LratClos);
LratBody:
  PUSH(FP);
  MOV(FP, SP);

  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_error_inChar_not_1_item_dmh);

  MOV(R1, FPARG(2));
  CMP(IND(R1), T_INTEGER);
  JUMP_NE(L_IS_SOB_rat_FALSE2);

  MOV(R0, SOB_TRUE);
  JUMP(L_IS_SOB_rat_EXIT2);

  L_IS_SOB_rat_FALSE2:  
  MOV(R0, SOB_FALSE);


  L_IS_SOB_rat_EXIT2:
  POP(FP);
  RETURN;
    
LratClos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(022230));
    MOV(INDD(R0,3),LABEL(LratBody));
