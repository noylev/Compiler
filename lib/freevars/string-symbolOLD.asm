JUMP(L_StrToSym_closure);
L_StrToSym_body:
    PUSH(FP);
    MOV(FP,SP);
	
    CMP(FPARG(1),IMM(1));
    JUMP_NE(L_error_incorrect_num_args);
MOV(R2,IMM(7));
L_StrToSym_loop:
    
    CMP(IND(R2),IMM(-1));
    JUMP_EQ(L_StrToSym_CREATE_SYM);
MOV(R1,FPARG(2));
MOV(R3,IND(FPARG(2)));

    MOV(R3,IND(R2));
    INCR(R3);
    CMP(IND(R3),FPARG(2));
    

   JUMP_EQ(L_StrToSym_RET_R2);

    MOV(R2,IND(R2));
    ADD(R2,2);

    JUMP(L_StrToSym_loop);



L_StrToSym_CREATE_SYM:
	PUSH(IMM(-1));
	PUSH(FPARG(2));
	CALL(MAKE_SOB_SYMBOL);
	DROP(2);
   
   MOV(IND(R2),R0);
JUMP(L_StrToSym_EXIT);

L_StrToSym_RET_R2:
   MOV(R0,IND(R2));
   
L_StrToSym_EXIT:


    POP(FP);
    RETURN;
    
L_StrToSym_closure:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(033311));
    MOV(INDD(R0,3),LABEL(L_StrToSym_body));
