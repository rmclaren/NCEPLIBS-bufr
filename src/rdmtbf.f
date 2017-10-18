	SUBROUTINE RDMTBF ( LUNSTF, LUNLTF, MXMTBF )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDMTBF
C   PRGMMR: ATOR            ORG: NCEP       DATE: 2017-10-17
C
C ABSTRACT:  THIS SUBROUTINE READS MASTER CODE/FLAG TABLE INFORMATION
C   FROM TWO SEPARATE (I.E. ONE STANDARD AND ONE LOCAL) ASCII FILES
C   AND THEN MERGES IT INTO AN INTERNAL MEMORY STRUCTURE.  EACH OF THE
C   TWO INPUT FILES MUST ALREADY BE INDIVIDUALLY SORTED IN ASCENDING
C   ORDER WITH RESPECT TO THE FXY NUMBERS.
C
C PROGRAM HISTORY LOG:
C 2017-10-17  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL RDMTBF ( LUNSTF, LUNLTF, MXMTBF )
C
C   INPUT ARGUMENT LIST:
C     LUNSTF   - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE
C                CONTAINING STANDARD CODE/FLAG TABLE INFORMATION
C     LUNLTF   - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE
C                CONTAINING LOCAL CODE/FLAG TABLE INFORMATION
C     MXMTBF   - INTEGER: MAXIMUM NUMBER OF CODE/FLAG TABLE ENTRIES
C                TO BE STORED IN MERGED INTERNAL MEMORY STRUCTURE.
C
C   OUTPUT ARGUMENT LIST:
C
C REMARKS:
C    THIS ROUTINE CALLS:        ADN30    BORT     GETNTBE  GETTBH
C                               SNTBFE   WRDLEN
C    THIS ROUTINE IS CALLED BY: IREADMT
C                               Not normally called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	CHARACTER*600	STLINE, LTLINE
	CHARACTER*128	BORT_STR
	CHARACTER*6	CMATCH, ADN30

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Call WRDLEN to initialize some important information about the
C	local machine, just in case it hasn't already been called.

	CALL WRDLEN

C	Read and parse the header lines of both files.

	CALL GETTBH ( LUNSTF, LUNLTF, 'F', IMT, IMTV, IOGCE, ILTV )
	
C	Read through the remainder of both files, merging the
C	contents into a unified internal memory structure.

	NMTBF = 0
	CALL GETNTBE ( LUNSTF, ISFXYN, STLINE, IERS )
	CALL GETNTBE ( LUNLTF, ILFXYN, LTLINE, IERL )
	DO WHILE ( ( IERS .EQ. 0 ) .OR. ( IERL .EQ. 0 ) )
	  IF ( ( IERS .EQ. 0 ) .AND. ( IERL .EQ. 0 ) ) THEN
	    IF ( ISFXYN .EQ. ILFXYN ) THEN
	      CMATCH = ADN30 ( ISFXYN, 6 )
	      GOTO 900
	    ELSE IF ( ISFXYN .LT. ILFXYN ) THEN
	      CALL SNTBFE ( ISFXYN, STLINE, MXMTBF )
	      CALL GETNTBE ( LUNSTF, ISFXYN, STLINE, IERS )
	    ELSE
	      CALL SNTBFE ( ILFXYN, LTLINE, MXMTBF )
	      CALL GETNTBE ( LUNLTF, ILFXYN, LTLINE, IERL )
	    ENDIF
	  ELSE IF ( IERS .EQ. 0 ) THEN
	    CALL SNTBFE ( ISFXYN, STLINE, MXMTBF )
	    CALL GETNTBE ( LUNSTF, ISFXYN, STLINE, IERS )
	  ELSE IF ( IERL .EQ. 0 ) THEN
	    CALL SNTBFE ( ILFXYN, LTLINE, MXMTBF )
	    CALL GETNTBE ( LUNLTF, ILFXYN, LTLINE, IERL )
	  ENDIF
	ENDDO

	RETURN
 900	WRITE(BORT_STR,'("BUFRLIB: RDMTBF - STANDARD AND LOCAL'//
     . ' CODE/FLAG TABLE FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)')
     .	 CMATCH(1:1), '-', CMATCH(2:3), '-', CMATCH(4:6)	
	CALL BORT(BORT_STR)
	END
