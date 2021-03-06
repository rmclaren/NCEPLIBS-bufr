C> @file
C> @author WOOLLEN @date 2014-11-21
      
C> THIS SUBROUTINE WORKS JUST LIKE SUBROUTINE PKB, IN THAT IT
C>   PACKS AN INTEGER VALUE (NVAL) INTO NBITS BITS OF AN INTEGER ARRAY
C>   (IBAY) STARTING WITH BIT (IBIT+1) AND THEN UPDATES IBIT TO POINT TO
C>   THE LAST BIT THAT WAS PACKED.  THE DIFFERENCE IS THAT THIS SUBROUTINE
C>   WILL WORK FOR CASES WHERE NBITS IS GREATER THAN NBITW (I.E. THE
C>   NUMBER OF BITS IN A MACHINE WORD) BY ZERO'ING OUT ALL OF THE BITS
C>   IN NBITS UP TO THE LAST MACHINE WORD, BEFORE THEN STORING NVAL
C>   WITHIN THE LAST MACHINE WORD.
C>
C> PROGRAM HISTORY LOG:
C> 2014-11-21  J. WOOLLEN -- ORIGINAL AUTHOR, JEFF ATOR'S IDEA
C>
C> USAGE:    CALL PKX (NVAL, NBITS, IBAY, IBIT)
C>   INPUT ARGUMENT LIST:
C>     NVAL     - INTEGER: INTEGER TO BE PACKED
C>     NBITS    - INTEGER: NUMBER OF BITS OF IBAY WITHIN WHICH TO PACK
C>                NVAL
C>     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY NOT YET CONTAINING
C>                PACKED NVAL
C>     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING BIT AFTER
C>                WHICH TO START PACKING
C>
C>   OUTPUT ARGUMENT LIST:
C>     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY NOW CONTAINING
C>                PACKED NVAL
C>     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING LAST BIT
C>                THAT WAS PACKED
C>
C> REMARKS:
C>    THIS SUBROUTINE IS THE INVERSE OF BUFR ARCHIVE LIBRARY ROUTINE
C>    UPB.
C>
C>    THIS ROUTINE CALLS:        PKB
C>    THIS ROUTINE IS CALLED BY: WRCMPS
C>                               Normally not called by any application
C>                               programs.
C>
      SUBROUTINE PKX(NVAL,NBITS,IBAY,IBIT)



      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      DIMENSION IBAY(*)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C      IF NBITS IS > NBITW THEN ZERO BITS UP TO NBITS-NBITW

       NWRD=NBITS/NBITW
       IF(NWRD>0) THEN
          JBIT=IBIT
          DO N=1,NWRD
          CALL PKB(0,NBITW,IBAY,JBIT)
          ENDDO
          IBIT=IBIT+NBITS-NBITW
       ENDIF

C      STORE NVAL IN THE LAST WORD OF THE BIT STRING

       CALL PKB(NVAL,MIN(NBITW,NBITS),IBAY,IBIT)

       RETURN
       END
