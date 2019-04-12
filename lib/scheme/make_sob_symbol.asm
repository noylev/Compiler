/* scheme/make_sob_char.asm
 * Takes an integer 0 <= n < 256 as an argument, and places 
 * in R0 the corresponding character object
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_SYMBOL);
  MOV(INDD(R0, 1), FPARG(0));
  MOV(INDD(R0, 2), FPARG(1));
  POP(FP);
  RETURN;

