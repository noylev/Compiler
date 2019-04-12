JUMP(L_List_Clos);
L_List_Body:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R3,IMM(SOB_NIL)); //the neutral is ZERO
	MOV(R2,FPARG(1)); //number of arguments to R2
	ADD(R2,IMM(1));
	MOV(R1,IMM(1)); //iterator on arguments to R1
L_List_Body_loop:          	
	CMP(R1,R2);
	JUMP_EQ(L_List_Body_loop_exit);
        PUSH(IMM(3));
        CALL(MALLOC);
        DROP(1);
        MOV(IND(R0), T_PAIR);
        MOV(INDD(R0, 1), FPARG(R2));
        MOV(INDD(R0, 2), R3);
	DECR(R2);
        MOV(R3,R0)
	JUMP(L_List_Body_loop);   
L_List_Body_loop_exit:
        MOV(R0,R3);
	POP(FP);
	RETURN;

L_List_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_List_Body));
