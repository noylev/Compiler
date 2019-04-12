JUMP(L_Make_String_Clos);
L_Make_String_Body:
	PUSH(FP);
	MOV(FP,SP);

        MOV(R3,IMM(0))  
        
        CMP(FPARG(1),IMM(1));
	JUMP_EQ(L_Make_String_1ARG);

	CMP(FPARG(1),IMM(2));
	JUMP_NE(L_Make_String_Body_ERROR);

	MOV(R3,INDD(FPARG(3),1)); //CHAR to push
L_Make_String_1ARG:
	MOV(R1,INDD(FPARG(2),1)); //iterator on arguments to R1

L_Make_String_Body_loop:      

	CMP(R1,IMM(0));
	JUMP_EQ(L_Make_String_Body_loop_exit);
        PUSH(R3);
	DECR(R1);
	JUMP(L_Make_String_Body_loop);   
L_Make_String_Body_loop_exit:
	PUSH(INDD(FPARG(2),1));
	CALL(MAKE_SOB_STRING);
	DROP(1);
	DROP(INDD(FPARG(2),1));
L_Make_String_Body_exit:
	POP(FP);
	RETURN;
	

L_Make_String_Body_ERROR:
	INFO;
 	SHOW("ERRRO!, Make_String didn't get two arguments", FPARG(1));

L_Make_String_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_Make_String_Body));
