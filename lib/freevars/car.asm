JUMP(L_car_closure);
L_car_Body:
    PUSH(FP);
    MOV(FP,SP);
    CMP(FPARG(1),IMM(1));
    JUMP_NE(L_error_incorrect_num_args);
    MOV(R0,FPARG(2));
    CMP(INDD(R0,0),IMM(T_PAIR));
    JUMP_NE(L_error_not_a_pair_car);
    MOV(R0,INDD(R0,1));
    POP(FP);
    RETURN;
L_car_closure:
    PUSH(4);
    CALL(MALLOC);
    DROP(1);
    MOV(INDD(R0,1),IMM(T_CLOSURE));
    MOV(INDD(R0,2),IMM(033311));
    MOV(INDD(R0,3),LABEL(L_car_Body));


