"JUMP(L_StrToSym_closure);\n"
"L_StrToSym_body:
"    PUSH(FP);\n"
"    MOV(FP,SP);\n"
"	PUSH(R1);\n"
"	PUSH(R2);\n"
"
"    CMP(FPARG(1),IMM(1));\n"
"    JUMP_NE(L_error_incorrect_num_args);\n"

"MOV(R1,@@@@@FIRST-SYMBOL@@@@@);\n"

"L_StrToSym_loop:\n"
"    CMP(INDD(R1,2),IMM(-1));\n"
"    JUMP_EQ(L_StrToSym_CREATE_SYM);\n"

"    CMP(INDD(R1,1),FPARG(2));\n"
"   JUMP_EQ(L_StrToSym_RET_R1);\n"

"    CMP(INDD(R1,0),T_SYMBOL);\n"
"    JUMP_NE(L_error_incorrect_list);\n"
"    MOV(R1,INDD(R1,2));\n"
"    JUMP(L_StrToSym_loop);\n"



"L_StrToSym_CREATE_SYM:"
"	PUSH(IMM(-1));\n"
"	PUSH(FPARG(2));\n"
"	CALL(MAKE_SOB_SYMBOL);\n"
"	DROP(2);\n"
"   MOV(INDD(R1,2),R0);\n"
"JUMP(L_StrToSym_EXIT);\n"
"L_StrToSym_RET_R1:\n"
"   MOV(R0,R1);\n"
"L_StrToSym_EXIT:\n"
"	POP(R2);\n"
"	POP(R1);\n"
"    POP(FP);\n"
"    RETURN;\n"


"L_error_incorrect_list:\n"
"INFO;\n"
"SHOW("ERROR!(CORE-FREE-VARS) string->symbol is incorrect",INDD(R2,0));\n"
"exit(-1);\n"


"L_StrToSym_closure:\n"
"    PUSH(4);\n"
"    CALL(MALLOC);\n"
"    DROP(1);\n"
"    MOV(INDD(R0,1),IMM(T_CLOSURE));\n"
"    MOV(INDD(R0,2),IMM(033311));\n"
"    MOV(INDD(R0,3),LABEL(L_StrToSym_body));\n"




