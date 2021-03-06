C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS FUNCTION RETURNS THE INTEGER CORRESPONDING TO THE
C>   BIT-WISE REPRESENTATION OF AN INPUT CHARACTER FXY VALUE OF LENGTH
C>   SIX.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C>                           DOCUMENTATION
C>
C> USAGE:    IFXY (ADSC)
C>   INPUT ARGUMENT LIST:
C>     ADSC     - CHARACTER*6: CHARACTER FORM OF DESCRIPTOR (FXY VALUE)
C>
C>   OUTPUT ARGUMENT LIST:
C>     IFXY     - INTEGER: BIT-WISE REPRESENTATION OF DESCRIPTOR (FXY)
C>                VALUE
C>
C> REMARKS:
C>
C>      EXAMPLE:
C>
C>      If ADSC = '063022', then IFXY = 16150 since:
C>
C>      0       63           22
C>
C>      F |     X     |       Y
C>        |           |
C>     0 0 1 1 1 1 1 1 0 0 0 1 0 1 1 0  =
C>
C>      ( 2**13 + 2**12 + 2**11 + 2**10 +
C>              2**9 + 2**8 + 2**4 + 2**2 + 2**1 )  = 16150
C>
C>
C>    THIS ROUTINE CALLS:        None
C>    THIS ROUTINE IS CALLED BY: BFRINI   DXINIT   GETNTBE  GETCFMNG
C>                               IDN30    IREADMT  NEMTAB   NEMTBB
C>                               NEMTBD   NUMTBD   READS3   RESTD
C>                               SNTBDE   SNTBFE   STBFDX   STNTBI
C>                               STSEQ    UFBQCP
C>                               Normally not called by any application
C>                               programs but it could be.
C>
      FUNCTION IFXY(ADSC)



      CHARACTER*6 ADSC

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      READ(ADSC,'(I1,I2,I3)') IF,IX,IY
      IFXY = IF*2**14 + IX*2**8 + IY
      RETURN
      END
