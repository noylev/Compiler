JUMP(L_Modulo_Clos);
L_Modulo_Body:
	PUSH(FP);
	MOV(FP,SP);

	MOV(R0,INDD(FPARG(2),1));
	MOV(R1,INDD(FPARG(3),1));
	
	REM(R0,R1);
	MOV(R1,IMM(1));
        PUSH(R1);
        PUSH(R0);
        CALL(MAKE_SOB_INTEGER);
        DROP(2);
        
	
	POP(FP);
	RETURN;

L_Modulo_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011112));
    MOV(INDD(R0,3),LABEL(L_Modulo_Body));
