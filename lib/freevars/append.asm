JUMP(L_append_closure);
L_append_body:
    PUSH(FP);
    MOV(FP,SP);
	PUSH(R1);
	PUSH(R2);
MOV(R0,SOB_NIL);
  MOV(R4,IMM(3));
  MOV(R5,FPARG(1));
 CMP(R5,IMM(0));
JUMP_EQ(L_append_EXIT);

  DECR(R5); 
  
  
        MOV(R3,IMM(2));
        MOV(R1,FPARG(2));

APPEND_START_OVER:   
    
 
L_append_NOT_VOID:
        

L_append_loop:
    CMP(INDD(R1,0),T_NIL);
    JUMP_EQ(L_append_EXIT2);

    MOV(R2,INDD(R1,2))
    CMP(INDD(R2,0),T_NIL);
    JUMP_EQ(L_append_DO_APPEND)
    MOV(R1,R2);
    JUMP(L_append_loop);


L_append_DO_APPEND:
   MOV(INDD(R1,2),FPARG(R4));
   
   INCR(R4);
   DECR(R5);
   CMP(R5,IMM(0));
   JUMP_NE(APPEND_START_OVER);

   
   MOV(R0,FPARG(2));
   
   
   DECR(R5);
   CMP(R5,IMM(0));
 
   
    JUMP(L_append_EXIT);

L_append_EXIT2:
   MOV(R0,FPARG(3));

   L_append_EXIT:
	POP(R2);
	POP(R1);
    POP(FP);
    RETURN;


L_error_incorrect_list:
SHOW("ERROR!(CORE-FREE-VARS) numbLALALALALAnt is incorrect",INDD(R2,0));
exit(-1);




