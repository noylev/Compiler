JUMP(L_appendTwo_closure);
L_appendTwo_body:
    PUSH(FP);
    MOV(FP,SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);
	PUSH(R4);
	MOV(R1,FPARG(0));
	MOV(R2,FPARG(1));
	MOV(R3,0);

	MOV(R0,R1);
	CMP(R2,SOB_NIL);
	JUMP_EQ(L_appendTwo_EXIT);
	MOV(R0,R2);
	CMP(R1,SOB_NIL);
	JUMP_EQ(L_appendTwo_EXIT);

	L_appendTwo_LOOP_FOR_THE_CREATION:
	CMP(R1,SOB_NIL);
	JUMP_EQ(L_appendTwo_LOOP_FOR_appendTwoING);
	PUSH(INDD(R1,1));
	INCR(R3);
	MOV(R1,INDD(R1,2));
	JUMP(L_appendTwo_LOOP_FOR_THE_CREATION);

	L_appendTwo_LOOP_FOR_appendTwoING:
	CMP(R3,IMM(0));
	JUMP_EQ(L_appendTwo_DONE);
	POP(R4);
	PUSH(R2);
	PUSH(R4)
	CALL(MAKE_SOB_PAIR);
	DROP(2);
	MOV(R2,R0);
	DECR(R3);
	JUMP(L_appendTwo_LOOP_FOR_appendTwoING);
	
	L_appendTwo_DONE:
	
	MOV(R0,R2);
	L_appendTwo_EXIT:
	POP(R4);
	POP(R3);
	POP(R2);
	POP(R1);
    POP(FP);
    RETURN;


L_error_incorrect_list:
SHOW("ERROR!(CORE-FREE-VARS) numbLALALALALAnt is incorrect",INDD(R2,0));
exit(-1);


L_appendTwo_closure:
