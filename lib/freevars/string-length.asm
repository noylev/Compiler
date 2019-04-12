JUMP(L_String_Length_Ref_Clos);
L_String_Length_Ref_Body:
	PUSH(FP);
	MOV(FP,SP);
	
    
        MOV(R5,INDD(FPARG(2),1));
	PUSH(IMM(1));
        PUSH(IMM(R5));
        CALL(MAKE_SOB_INTEGER);
        DROP(2);
       	
	POP(FP);
        RETURN;

L_String_Length_Ref_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(014310));
    MOV(INDD(R0,3),LABEL(L_String_Length_Ref_Body));
