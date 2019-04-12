JUMP(LnumClos);
LnumBody:
  PUSH(FP);
  MOV(FP, SP);

  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_error_inChar_not_1_item_dmh);

  MOV(R0, FPARG(2));
  CMP(IND(R0), T_INTEGER);
  JUMP_EQ(L_IS_SOB_num_TRUE2);
  MOV(R0, SOB_FALSE);
  JUMP(L_IS_SOB_num_EXIT2);
  L_IS_SOB_num_TRUE2:
  MOV(R0, SOB_TRUE);
  L_IS_SOB_num_EXIT2:
  POP(FP);
  RETURN;
    
LnumClos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(022230));
    MOV(INDD(R0,3),LABEL(LnumBody));
