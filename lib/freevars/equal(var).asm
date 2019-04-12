JUMP(L_Equal_Clos);
L_Equal_Body:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R0,IMM(SOB_TRUE)); //the neutral is true
	MOV(R2,FPARG(1)); //number of arguments to R2
	ADD(R2,IMM(2));
	MOV(R1,IMM(2)); //iterator on arguments to R1
L_Equal_Body_loop:          
	
	CMP(R1,R2);
	JUMP_EQ(L_Equal_Body_loop_exit);

	CMP(INDD(FPARG(2),1),INDD(FPARG(R1),1));
	JUMP_NE(L_Equal_Body_loop_false);
	CMP(INDD(FPARG(2),2),INDD(FPARG(R1),2));
	JUMP_NE(L_Equal_Body_loop_false);
	INCR(R1);
	JUMP(L_Equal_Body_loop);   
L_Equal_Body_loop_false:
	MOV(R0,SOB_FALSE);
	JUMP(L_Equal_Body_loop_exit);
	L_Equal_Body_loop_mid:


L_Equal_Body_loop_exit:

	POP(FP);
	RETURN;

L_Equal_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_Equal_Body));
