
JUMP(L_apply_closure);
L_apply_body:

    PUSH(FP);   
       MOV(FP, SP);   
    PUSH(R1);
    PUSH(R2);
    PUSH(R3);
    PUSH(R4);
    PUSH(R5);
    PUSH(R6);
    PUSH(R7);
    PUSH(R8);
    
       MOV(R6, IMM(0));    
       MOV(R2, FPARG(3));   
    
    L_apply_loop:   
       CMP(IND(R2), T_NIL);   
       JUMP_EQ(L_apply_loop_exit);   
       PUSH(INDD(R2, 1));   
       MOV(R2, INDD(R2, 2));   
       INCR(R6);   
       JUMP(L_apply_loop);   
    
    L_apply_loop_exit:   
       MOV(R4, SP);   
       SUB(R4, R6);   
       MOV(R3, SP);   
       DECR(R3);   
    
    L_apply_reverse_loop:   
       CMP(R4, R3);   
       JUMP_GE(L_apply_reverse_exit);   
       PUSH(R6);   
       MOV(R6, STACK(R4));   
       MOV(STACK(R4), STACK(R3));   
       MOV(STACK(R3), R6);   
       POP(R6);   
       INCR(R4);   
       DECR(R3);   
       JUMP(L_apply_reverse_loop);   
       L_apply_reverse_exit:   
       PUSH(R6);   
       MOV(R4, FPARG(2));   
       PUSH(INDD(R4, 1));   
       CALLA(INDD(R4, 2));   
       MOV(R2, STARG(0));    
       ADD(R2, IMM(2));   
       SUB(SP, R2); 
       
       POP(R8);
       POP(R7);
       POP(R6);
       POP(R5);
       POP(R4);
       POP(R3);
       POP(R2);
       POP(R1);

       
       POP(FP);   
       RETURN;   

L_apply_closure:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(033311));
    MOV(INDD(R0,3),LABEL(L_apply_body));

