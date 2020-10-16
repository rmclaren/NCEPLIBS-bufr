/** @file
    @author ATOR @date 2003-11-04
*/

/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    BORT_EXIT
C   PRGMMR: ATOR             ORG: NP12       DATE: 2003-11-04
C
C ABSTRACT: THIS SUBROUTINE WILL TERMINATE THE APPLICATION PROGRAM AND
C   RETURN AN IMPLEMENTATION-DEFINED NON-ZERO STATUS CODE TO THE
C   EXECUTING SHELL SCRIPT.
C
C PROGRAM HISTORY LOG:
C 2003-11-04  J. ATOR    -- ORIGINAL AUTHOR
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF
C 2004-08-18  J. ATOR    -- USE bufrlib.h INCLUDE FILE
C 2007-01-19  J. ATOR    -- FIX DECLARATION FOR ANSI-C
C
C USAGE:    CALL BORT_EXIT
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: BORT     BORT2
C                               Normally not called by application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"

void bort_exit( void )
{
    exit( EXIT_FAILURE );
}
