      SUBROUTINE ERRWRT(STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ERRWRT
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2009-04-21
C
C ABSTRACT: THIS SUBROUTINE WRITES A GIVEN ERROR OR OTHER DIAGNOSTIC
C   MESSAGE TO A USER-SPECIFIED LOGICAL UNIT.  AS DISTRIBUTED WITHIN
C   THE BUFR ARCHIVE LIBRARY, THIS SUBROUTINE WILL WRITE ANY SUCH
C   MESSAGES TO STANDARD OUTPUT; HOWEVER, APPLICATION PROGRAMS MAY
C   SUBSTITUTE AN IN-LINE VERSION OF ERRWRT (OVERRIDING THIS ONE) IN
C   ORDER TO DEFINE AN ALTERNATE DESTINATION FOR SUCH MESSAGES.
C
C PROGRAM HISTORY LOG:
C 2009-04-21  J. ATOR    -- ORIGINAL AUTHOR
C 2012-11-15  D. KEYSER  -- USE FORMATTED PRINT
C
C USAGE:    CALL ERRWRT (STR)
C   INPUT ARGUMENT LIST:
C     STR      - CHARACTER*(*): ERROR MESSAGE TO BE PRINTED TO
C                STANDARD OUTPUT (DEFAULT) OR TO ANOTHER DESTINATION
C                (IF SPECIFIED BY THE USER APPLICATION VIA AN IN-LINE
C                REPLACEMENT FOR THIS SUBROUTINE)
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: BORT     BORT2    CKTABA   CPDXMM
C                               CPYUPD   DATEBF   DUMPBF   HOLD4WLC
C                               IGETPRM  INVCON   INVTAG   INVWIN
C                               JSTNUM   MAKESTAB MAXOUT   MRGINV
C                               MSGUPD   MSGWRT   NVNWIN   OPENBF
C                               OPENBT   PKTDD    RDBFDX   RDMEMM
C                               RDMEMS   READDX   READERME READLC
C                               READMG   READMT   READS3   STRNUM
C                               STRSUC   UFBEVN   UFBIN3   UFBINT
C                               UFBMEM   UFBMEX   UFBOVR   UFBREP
C                               UFBRMS   UFBRW    UFBSEQ   UFBSTP
C                               UFBTAB   UFBTAM   USRTPL   VALX
C                               WRDLEN
C                               Can also be called by application
C                               programs using an in-line version.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*(*) STR

      PRINT'(1X,A)',STR

      RETURN
      END
