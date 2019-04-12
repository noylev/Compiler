JUMP(L_Plus_Clos);
L_Plus_Body:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R0,IMM(0)); //the neutral is ZERO
	
	MOV(R2,FPARG(1)); //number of arguments to R2
	ADD(R2,IMM(2));
	MOV(R1,IMM(3)); //iterator on arguments to R1

	CMP(FPARG(1),IMM(2));
	JUMP_LT(L_Plus_Body_fewer_then_2);
	MOV(R5,INDD(FPARG(2),1));
	MOV(R6,INDD(FPARG(2),2));
    
	
L_Plus_Body_loop:          
	
	CMP(R1,R2);
	JUMP_EQ(L_Plus_Body_loop_exit);
	
	MOV(R7,INDD(FPARG(R1),1));
	MOV(R8,INDD(FPARG(R1),2));
   
        MUL(R5,R8);     //MON1
        MUL(R7,R6);     //MON2
        MUL(R6,R8);     //DOM
        
        ADD(R5,R7);
        
        PUSH(R6);
        PUSH(R5);
        CALL(MAKE_SOB_INTEGER);
        DROP(2);
        
	MOV(R5,INDD(R0,1));
	MOV(R6,INDD(R0,2));
	INCR(R1);
	JUMP(L_Plus_Body_loop);   
L_Plus_Body_loop_exit:
	
	;//  GCD ON R0
	
	POP(FP);
	RETURN;

	
L_Plus_Body_fewer_then_2:
        CMP(FPARG(1),IMM(1));
        JUMP_LT(L_Plus_Zero_Item);
        
        MOV(R5,INDD(FPARG(2),1));
	MOV(R6,INDD(FPARG(2),2));
	
        PUSH(R6);
        PUSH(R5);
        CALL(MAKE_SOB_INTEGER);
        DROP(2);
        
	POP(FP);
	RETURN;
	
L_Plus_Zero_Item:
        MOV(R5,IMM(0));
	MOV(R6,IMM(1));
        PUSH(R6);
        PUSH(R5);
        CALL(MAKE_SOB_INTEGER);
        DROP(2);
	POP(FP);
	RETURN;

L_Plus_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_Plus_Body));
