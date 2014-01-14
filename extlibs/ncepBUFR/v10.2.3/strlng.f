      SUBROUTINE STRLNG  ( string, lens )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    STRCLN
C   PRGMMR: JACOBS          ORG: NP12       DATE: 2013-06-21
C
C ABSTRACT: THIS SUBROUTINE RETURNS THE NUMBER OF CHARACTERS
c   IN A STRING IGNORING TRAILING NULLS, TABS AND SPACES.
C
C PROGRAM HISTORY LOG:
C 2013-06-21  S. JACOBS -- ORIGINAL AUTHOR
C
C USAGE:    CALL STRLNG ( STRING, LENS )
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: OPENBF
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$
      INCLUDE 'bufrlib.prm'
C*
      CHARACTER*(*)	string
      CHARACTER*1	ch

C*    Get the actual length of the string.
      lens = LEN  ( string )
      IF  ( lens .eq. 0 )  RETURN

C*    Start at last character and loop backwards.
      ip = lens
      DO WHILE  ( ip .gt. 0 )

C*        Get current value of string and check for space, null, tab.
          ch = string ( ip : ip )
	  IF  ( ( ch .eq. CHAR(32) ) .or. ( ch .eq. CHAR(0) ) .or.
     +		( ch .eq. CHAR(9)  ) )  THEN
	      lens = lens - 1
	      ip   = ip - 1
	  ELSE
	      ip   = 0
	  END IF
      END DO
C*
      RETURN
      END
