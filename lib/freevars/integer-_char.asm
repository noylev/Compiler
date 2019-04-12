JUMP(LIntToCharClos);
LIntToCharBody:
  PUSH(FP);
  MOV(FP, SP);

  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_error_inChar_not_1_item_dmh);

  MOV(R0, FPARG(2));
  CMP(IND(R0), T_INTEGER);
  JUMP_NE(L_error_inCharToInt_not_char_item_dmh);
  PUSH(INDD(R0,1));
  CALL(MAKE_SOB_CHAR)
  DROP(1);
  POP(FP);
  RETURN;
    
LIntToCharClos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(022230));
    MOV(INDD(R0,3),LABEL(LIntToCharBody));
