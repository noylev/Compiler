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
	CMP(FPARG(1),IMM(0));
	JUMP_GE(MAKE_NUM_NO_SWITCHING)
	MUL(FPARG(1),IMM(-1));
	MUL(FPARG(0),IMM(-1));
	MAKE_NUM_NO_SWITCHING:
	PUSH(FPARG(1));
	MOV(R1,FPARG(0));
	CMP(FPARG(0),IMM(0));
	JUMP_GE(MAKE_NUM_ISPOSITIVE);
	MUL(R1,IMM(-1));
	MAKE_NUM_ISPOSITIVE:
	PUSH(R1);
	CALL(GET_GCD);
	DROP(2);
  CMP(R0,IMM(1));
  JUMP_LE(NO_NEED_TO_DIV);
  DIV(FPARG(0),R0);
  DIV(FPARG(1),R0);
NO_NEED_TO_DIV:
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0, 0), T_INTEGER);
  MOV(INDD(R0, 1), FPARG(0));
  MOV(INDD(R0, 2), FPARG(1));
  POP(R1)
  POP(FP);
  RETURN;
