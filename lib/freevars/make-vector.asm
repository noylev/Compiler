JUMP(L_Make_Vector_Clos);
L_Make_Vector_Body:
	PUSH(FP);
	MOV(FP,SP);

        CMP(FPARG(1),IMM(1));
	JUMP_EQ(L_Make_Vector_1ARG);

	
	CMP(FPARG(1),IMM(2));
	JUMP_NE(L_Make_Vector_Body_ERROR);

	MOV(R3,FPARG(3)); //CHAR to push
        JUMP(MAKE_VECTOER_2ARG);
L_Make_Vector_1ARG:

	PUSH(1);
	PUSH(0);
	CALL(MAKE_SOB_INTEGER);
	DROP(2)
        MOV(R3,R0)  
            
MAKE_VECTOER_2ARG:
	MOV(R1,INDD(FPARG(2),1)); //iterator on arguments to R1

L_Make_Vector_Body_loop:      

	CMP(R1,IMM(0));
	JUMP_EQ(L_Make_Vector_Body_loop_exit);
        PUSH(R3);
	DECR(R1);
	JUMP(L_Make_Vector_Body_loop);   
L_Make_Vector_Body_loop_exit:
	PUSH(INDD(FPARG(2),1));
	CALL(MAKE_SOB_VECTOR);
	DROP(1);
	DROP(INDD(FPARG(2),1));
L_Make_Vector_Body_exit:
	POP(FP);
	RETURN;
L_Make_Vector_Body_ERROR:
	INFO;
 	SHOW("ERRRO!, Make_Vector didn't get two arguments", FPARG(1));

L_Make_Vector_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_Make_Vector_Body));
