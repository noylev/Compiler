JUMP(L_append_closure);
L_append_body:
    PUSH(FP);
    MOV(FP,SP);

    CMP(FPARG(1),IMM(2));
    JUMP_NE(L_error_incorrect_num_args);

    CMP(INDD(FPARG(3),0),T_NIL);
    JUMP_NE(L_append_NOT_VOID);

    MOV(R0,FPARG(2));
    JUMP(L_append_EXIT);

L_append_NOT_VOID:
    MOV(R1,FPARG(2));
	CMP(INDD(FPARG(2),0),T_NIL);
	JUMP_EQ(L_append_EXIT2);

L_append_loop:
    MOV(R3,0);
    MOV(R4,FPARG(3));
    CMP(INDD(R1,0),T_PAIR);
    JUMP_NE(L_error_incorrect_list);
	PUSH(INDD(R1,1));
	INCR(R3);
    MOV(R2,INDD(R1,2))
    CMP(IND(R2),T_NIL);
    JUMP_EQ(L_append_DO_APPEND)
    MOV(R1,R2);
    JUMP(L_append_loop);



L_append_DO_APPEND:
	PUSH(R4);
	CALL(MAKE_SOB_PAIR);
	DROP(3);
	MOV(R4,R0);
	DECR(R3);
CMP(R3,IMM(0));
	JUMP_NE(L_append_DO_APPEND);
   MOV(R0,R4);
JUMP(L_append_EXIT);
L_append_EXIT2:
   MOV(R0,FPARG(3));
L_append_EXIT:
    POP(FP);
    RETURN;


L_error_incorrect_list:
INFO;
SHOW("ERROR!(CORE-FREE-VARS) numbLALALALALAnt is incorrect",INDD(R2,0));
exit(-1);


L_append_closure:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(033311));
    MOV(INDD(R0,3),LABEL(L_append_body));




