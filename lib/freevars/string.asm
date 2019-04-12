JUMP(L_String_Clos);
L_String_Body:
	PUSH(FP);
	MOV(FP,SP);
     
	MOV(R1,FPARG(1)); //iterator on arguments to R1
	MOV(R5,IMM(2));
	
	
L_String_Body_loop:      

	CMP(R1,IMM(0));
	JUMP_EQ(L_String_Body_loop_exit);

	PUSH(INDD(FPARG(R5),1));
 	DECR(R1);
 	INCR(R5);

	JUMP(L_String_Body_loop);   


	
L_String_Body_loop_exit:
	PUSH(FPARG(1));
	CALL(MAKE_SOB_STRING);
	DROP(1);
	DROP(IMM(FPARG(1)));
	
L_String_Body_exit:
	POP(FP);
	RETURN;

L_String_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(0176210));
    MOV(INDD(R0,3),LABEL(L_String_Body));
