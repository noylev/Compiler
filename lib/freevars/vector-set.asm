JUMP(L_set_Vector_Clos);
L_set_Vector_Body:
	PUSH(FP);
	MOV(FP,SP);

	CMP(FPARG(1),IMM(3));
	JUMP_NE(L_set_Vector_Body_ERROR);
        CMP(INDD(FPARG(2),0),T_VECTOR);
	JUMP_NE(L_set_Vector_Body_type_ERROR);
        CMP(INDD(FPARG(3),0),T_INTEGER);
	JUMP_NE(L_set_Vector_Body_type_ERROR);
        CMP(INDD(FPARG(2),1),INDD(FPARG(3),1));
	JUMP_LE(L_set_Vector_Body_general_ERROR);
   	MOV(R1,INDD(FPARG(3),1));
	ADD(R1,IMM(2));
	MOV(R2,FPARG(4));
	MOV(INDD(FPARG(2),R1),R2);
	MOV(R0,SOB_VOID);
L_set_Vector_Body_exit:
	POP(FP);
	RETURN;




L_set_Vector_Body_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Vector didn't get three arguments", FPARG(1));
exit(-1);
L_set_Vector_Body_general_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Vector error", FPARG(1));
exit(-1);
L_set_Vector_Body_type_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Vector type error", INDD(FPARG(2),0));
exit(-1);
L_set_Vector_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_set_Vector_Body));
