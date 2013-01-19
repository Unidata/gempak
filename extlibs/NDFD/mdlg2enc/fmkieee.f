      REAL FUNCTION FMKIEEE(A)
C   $$$ SUBPROGRAM DOCUMENTATION BLOCK
C    . . . .
C    SUBPROGRAM: mkieee 
C    PRGMMR: Gilbert ORG: W/NP11 DATE: 2000-05-09
C   
C    ABSTRACT: This subroutine stores a list of real values in 
C    32-bit IEEE floating point format.
C   
C    PROGRAM HISTORY LOG:
C    2000-05-09 Gilbert
C   
C    USAGE: CALL mkieee(a)
C    INPUT ARGUMENT LIST:
C    a - Input floating point value.
C   
C    OUTPUT ARGUMENT LIST: None.
C   
C    REMARKS: None
C   
C    ATTRIBUTES:
C    LANGUAGE: Fortran 90
C    MACHINE: IBM SP
C   
C   $$$

      PARAMETER (two23=2.**23)
      PARAMETER (two126=2.**126)
C
      EQUIVALENCE(RTEMP,IEEE)
C
      alog2=alog(2.0)

      ieee=0

      if (a.ne.0.) then
C   
C    Set Sign bit (bit 31 - leftmost bit)
C   
         if (a.lt.0.0) then
           ieee=ibset(ieee,31)
           atemp=abs(a)
         else
           ieee=ibclr(ieee,31)
           atemp=a
         endif
C   
C    Determine exponent n with base 2
C   
         n=INT(flr(alog(atemp)/alog2))
         iexp=n+127
C    overflow
         if (n.gt.127) iexp=255
         if (n.lt.-127) iexp=0
C    set exponent bits ( bits 30-23 )
         call mvbits(iexp,0,8,ieee,23)
C   
C    Determine Mantissa
C    
         if (iexp.ne.255) then
           if (iexp.ne.0) then
             atemp=(atemp/(2.0**n))-1.0
           else
             atemp=atemp*two126
           endif
           imant=nint(atemp*two23)
         else
           imant=0
         endif
C    set mantissa bits ( bits 22-0 )
         call mvbits(imant,0,23,ieee,0)
      endif
C
      FMKIEEE=RTEMP
      return
      end
