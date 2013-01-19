      REAL FUNCTION RDIEEE(RIEEE)
CC$$$ SUBPROGRAM DOCUMENTATION BLOCK
C . . . .
C SUBPROGRAM: rdieee 
C PRGMMR: Gilbert ORG: W/NP11 DATE: 2000-05-09
C
C ABSTRACT: This subroutine reads a list of real values in 
C 32-bit IEEE floating point format.
C
C PROGRAM HISTORY LOG:
C 2000-05-09 Gilbert
C
C USAGE:CALL rdieee(rieee,a,num)
C INPUT ARGUMENT LIST:
C rieee - Inputfloating point value in 32-bit IEEE format.
C
C OUTPUT ARGUMENT LIST: None.
C
C REMARKS: None
C
C ATTRIBUTES:
C LANGUAGE: Fortran 90
C MACHINE: IBM SP
C
C$$$

      PARAMETER (two23=2.**(-23))
      PARAMETER (two126=2.**(-126))
C
      EQUIVALENCE(RTEMP,IEEE)
C
      RTEMP=RIEEE
C
C Extract sign bit, exponent, and mantissa
C
      isign=ibits(ieee,31,1)
      iexp=ibits(ieee,23,8)
      imant=ibits(ieee,0,23)
      sign=1.0
      if (isign.eq.1) sign=-1.0
      
      if ( (iexp.gt.0).and.(iexp.lt.255) ) then
        temp=2.0**(iexp-127)
        rdieee=sign*temp*(1.0+(two23*real(imant)))

      elseif ( iexp.eq.0 ) then
        if ( imant.ne.0 ) then
          rdieee=sign*two126*two23*real(imant)
        else
          rdieee=sign*0.0
        endif

C     elseif ( iexp.eq.255 ) then
C       rdieee=sign*huge(rdieee)

      endif
C
      return
      end
