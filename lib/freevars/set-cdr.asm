JUMP(L_set_Cdr_Clos);
L_set_Cdr_Body:
	PUSH(FP);
	MOV(FP,SP);

	CMP(FPARG(1),IMM(2));
	JUMP_NE(L_set_Cdr_Body_ERROR);
        CMP(INDD(FPARG(2),0),T_PAIR);
	JUMP_NE(L_set_Cdr_Body_type_ERROR);

   	MOV(INDD(FPARG(2),2),FPARG(3));
	MOV(R0,SOB_VOID);
L_set_Cdr_Body_exit:
	POP(FP);
	RETURN;




L_set_Cdr_Body_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Cdr didn't get three arguments", FPARG(1));
exit(-1);
L_set_Cdr_Body_general_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Cdr error", FPARG(1));
exit(-1);
L_set_Cdr_Body_type_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Cdr type error", INDD(FPARG(2),0));
exit(-1);
L_set_Cdr_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_set_Cdr_Body));
