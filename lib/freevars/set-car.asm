JUMP(L_set_Car_Clos);
L_set_Car_Body:
	PUSH(FP);
	MOV(FP,SP);

	CMP(FPARG(1),IMM(2));
	JUMP_NE(L_set_Car_Body_ERROR);
	
	MOV(R1,FPARG(1));
	MOV(R2,FPARG(2));
	MOV(R3,FPARG(3));
   	MOV(INDD(FPARG(2),1),R3);
	MOV(R0,SOB_VOID);
L_set_Car_Body_exit:
	POP(FP);
	RETURN;




L_set_Car_Body_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Car didn't get three arguments", FPARG(1));
exit(-1);
L_set_Car_Body_general_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Car error", FPARG(1));
exit(-1);
L_set_Car_Body_type_ERROR:
	INFO;
 	SHOW("ERRRO!, set_Car type error", INDD(FPARG(2),0));
exit(-1);
L_set_Car_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_set_Car_Body));
