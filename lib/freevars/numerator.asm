JUMP(LNumeratorClos);
LNumeratorBody:
  PUSH(FP);
  MOV(FP, SP);

  CMP(FPARG(1), IMM(1));
  JUMP_NE(L_error_inChar_not_1_item_dmh);
  MOV(R0, FPARG(2));
  MOV(R0, INDD(R0,1));
	PUSH(IMM(1));
	PUSH(R0);
	CALL(MAKE_SOB_INTEGER);
	DROP(2);
  POP(FP);
  RETURN;
    
LNumeratorClos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(022230));
    MOV(INDD(R0,3),LABEL(LNumeratorBody));
