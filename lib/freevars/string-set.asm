JUMP(L_set_String_Clos);
L_set_String_Body:
	PUSH(FP);
	MOV(FP,SP);

	CMP(FPARG(1),IMM(3));
	JUMP_NE(L_set_String_Body_ERROR);
        CMP(INDD(FPARG(2),0),T_STRING);
	JUMP_NE(L_set_String_Body_type_ERROR);
        CMP(INDD(FPARG(4),0),T_CHAR);
	JUMP_NE(L_set_String_Body_type_ERROR);
        CMP(INDD(FPARG(3),0),T_INTEGER);
	JUMP_NE(L_set_String_Body_type_ERROR);
        CMP(INDD(FPARG(2),1),INDD(FPARG(3),1));
	JUMP_LE(L_set_String_Body_general_ERROR);
   	
   	
   	MOV(R1,INDD(FPARG(3),1));
	ADD(R1,IMM(2));
	MOV(R2,INDD(FPARG(4),1));
	MOV(INDD(FPARG(2),R1),R2);
	MOV(R0,SOB_VOID);

L_set_String_Body_exit:
	POP(FP);
	RETURN;




L_set_String_Body_ERROR:
	INFO;
 	SHOW("ERRRO!, set_String didn't get three arguments", FPARG(1));
exit(-1);
L_set_String_Body_general_ERROR:
	INFO;
 	SHOW("ERRRO!, set_String error", FPARG(1));
exit(-1);
L_set_String_Body_type_ERROR:
	INFO;
 	SHOW("ERRRO!, set_String type error", INDD(FPARG(2),0));
exit(-1);
L_set_String_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_set_String_Body));
