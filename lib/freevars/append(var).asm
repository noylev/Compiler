JUMP(L_Append_Clos);
L_Append_Body:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R0,SOB_NIL); //the neutral is ZERO
	
	MOV(R2,FPARG(1)); //number of arguments to R2
	ADD(R2,IMM(2));
	MOV(R1,IMM(3)); //iterator on arguments to R1

	CMP(FPARG(1),IMM(2));
	JUMP_LT(L_Append_Body_fewer_then_2);
	MOV(R5,FPARG(2));
    
	
L_Append_Body_loop:          
	
	CMP(R1,R2);
	JUMP_EQ(L_Append_Body_loop_exit);
	
	MOV(R6,FPARG(R1));
	PUSH(R6);
	PUSH(R5);
   	CALL(L_appendTwo_body);
	DROP(2);
	MOV(R5,R0);
	INCR(R1);
	JUMP(L_Append_Body_loop);   
L_Append_Body_loop_exit:
	
	//  GCD ON R0
	
	POP(FP);
	RETURN;

	
L_Append_Body_fewer_then_2:
        CMP(FPARG(1),IMM(1));
        JUMP_LT(L_Append_Zero_Item);
	MOV(R0,FPARG(2));
	POP(FP);
	RETURN;
	
L_Append_Zero_Item:
	MOV(R0,SOB_NIL);
	POP(FP);
	RETURN;

L_Append_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_Append_Body));
