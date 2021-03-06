JUMP(L_List_Clos);
L_List_Body:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R0,SOB_NIL); //the neutral is ZERO
	MOV(R2,FPARG(1)); //number of arguments to R2
	ADD(R2,IMM(2));
	MOV(R1,IMM(2)); //iterator on arguments to R1
L_List_Body_loop:          
	
	CMP(R1,R2);
	JUMP_EQ(L_List_Body_loop_exit);
        PUSH(R0);
        PUSH(FPARG(R2));
	CALL(MAKE_SOB_PAIR));
        DROP(2);
	DEC(R2);
	JUMP(L_List_Body_loop);   
L_List_Body_loop_exit:

	POP(FP);
	RETURN;

L_List_Clos:
    PUSH(3);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,0),IMM(T_CLOSURE));
    MOV(INDD(R0,1),IMM(011110));
    MOV(INDD(R0,2),LABEL(L_List_Body));
