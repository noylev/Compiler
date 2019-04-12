JUMP(L_Little_Clos);
L_Little_Body:
	PUSH(FP);
	MOV(FP,SP);
        MOV(R0, SOB_TRUE);

	MOV(R2,FPARG(1)); //number of arguments to R2
	ADD(R2,IMM(2));
	MOV(R1,IMM(3)); //iterator on arguments to R1

	CMP(FPARG(1),IMM(2));
	JUMP_LT(L_Little_Body_fewer_then_2);
	MOV(R5,INDD(FPARG(2),1));
	MOV(R6,INDD(FPARG(2),2));
    
	
L_Little_Body_loop:          
	
	CMP(R1,R2);
	JUMP_EQ(L_Little_Body_loop_exit);
	
	MOV(R7,INDD(FPARG(R1),1));
	MOV(R8,INDD(FPARG(R1),2));
   
        MUL(R5,R8);     //MON1
        MUL(R7,R6);     //MON2
        
        CMP(R5,R7);
        JUMP_GE(L_Little_False);
        
        MOV(R5,INDD(FPARG(R1),1));
	MOV(R6,INDD(FPARG(R1),2));
	INCR(R1);
	JUMP(L_Little_Body_loop);   

L_Little_Body_loop_exit:

	POP(FP);
	RETURN;

L_Little_False:

        MOV(R0, SOB_FALSE);
	POP(FP);
	RETURN;

L_Little_Body_fewer_then_2:
        JUMP(L_Little_Body_loop_exit);
      

L_Little_Clos:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(011110));
    MOV(INDD(R0,3),LABEL(L_Little_Body));
