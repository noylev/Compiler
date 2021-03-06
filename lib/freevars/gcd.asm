/* 	TAKES TWO ARGUMENTS 
 *	AND RETURNS THEIR GCD VALUE WITH FPARG(0) AS NUMERATOR
 *					 FPARG(1) AS DENOMINATOR / NOT ZERO
 */
JUMP(GCD_SKIP);
GET_GCD:
PUSH(FP);
MOV(FP,SP);
PUSH(R1);
PUSH(R2);
PUSH(R3);

MOV(R1,FPARG(0));
MOV(R2,FPARG(1));
CMP(R2, IMM(0));
JUMP_EQ(L_SECOND_ARG_CANNOT_BE_ZERO);
GCD_LOOP:
MOV(R3, R1);
REM(R3, R2);
CMP(R3, IMM(0));
JUMP_EQ(GCD_EXIT);
MOV(R1, R2);
MOV(R2, R3);
JUMP(GCD_LOOP);
GCD_EXIT:
MOV(R0,R2);
L_SECOND_ARG_CANNOT_BE_ZERO:
POP(R3);
POP(R2);
POP(R1);
POP(FP);
RETURN;
GCD_SKIP:
