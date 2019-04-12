/* scheme/make_sob_integer.asm
 * Takes an integer, and place the corresponding Scheme object in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 *@@@@@@@@@@@@@@@@@@@@@@@@CHANGE LOG@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 * 	TAKES TWO ARGUMENTS 
 *	AND MAKES A RATIONAL NUMBER WITH FPARG(0) AS NUMERATOR
 *					 FPARG(1) AS DENOMINATOR
 */

 MAKE_SOB_INTEGER:
  PUSH(FP);
  MOV(FP, SP);
	PUSH(R1);
	PUSH(R2);
	PUSH(R3);

GCD:
MOV(R1,FPARG(0));
MOV(R2,FPARG(1));
CMP(R2, IMM(0));
//JUMP_LE(L_SECOND_ARG_CANNOT_BE_ZERO);
JUMP(need_to_div);

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
SHOW("GCD RETURNED ", R0);
CMP(R0, IMM(1));
JUMP(no_need_to_div);
	


  need_to_div:
  DIV(FPARG(0),R0);  
  DIV(FPARG(1),R0);

  no_need_to_div:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), T_INTEGER);
  MOV(INDD(R0, 1), FPARG(0));
  MOV(INDD(R0, 2), FPARG(1));
	POP(R3);
	POP(R2);
	POP(R1);
  POP(FP);
  RETURN;




L_SECOND_ARG_CANNOT_BE_ZERO:
fprintf(stderr, "seconds argument to gcd cannot be zero");
exit(-1);
