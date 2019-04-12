JUMP(L_Vector_Ref_Clos);
L_Vector_Ref_Body:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1),IMM(2));
	JUMP_NE(L_Vector_Ref_Body_ERROR);
	CMP(IND(FPARG(2)),T_VECTOR);
	JUMP_NE(L_Vector_Ref_Body_ERROR);
	CMP(IND(FPARG(3)),T_INTEGER);
	JUMP_NE(L_Vector_Ref_Body_ERROR);
	CMP(INDD(FPARG(3),1),INDD(FPARG(2),1));
	JUMP_GE(L_Vector_Ref_Body_ERROR);
	MOV(R1,INDD(FPARG(3),1));
	ADD(R1,IMM(2));
	MOV(R0,INDD(FPARG(2),R1));
	DROP(1);
	RETURN;
L_Vector_Ref_Body_ERROR:
	INFO;
 	SHOW("ERRRO!, Vector_Ref didn't get two arguments", FPARG(1));

L_Vector_Ref_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_Vector_Ref_Body));
