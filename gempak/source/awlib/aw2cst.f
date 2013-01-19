	SUBROUTINE AW_2CST ( dlta, newnum, iret )
C************************************************************************
C* AW_2CST                                                              *
C*                                                                      *
C* This subroutine calculates the two's complement for a short vector.  *
C* 								        *
C* AW_2CST ( DLTA, NEWNUM, IRET )                                *
C*								        *
C* Input parameters: 						        *
C*	DLTA   		BYTE		Input short vector              *
C*								        *
C* Output parameters:						        *
C*	NEWNUM          INTEGER		New vector value                *
C*	IRET		INTEGER		Return code                     *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Added assignment do loop;added parens.  *
C************************************************************************
        BYTE		dlta
        INTEGER         tmpbt, newnum, subnum 
	INTEGER         sum(12)
        INTEGER         ibte
C------------------------------------------------------------------------
 	iret = 0
C
C*      Reassign byte to an integer.
C
            ibte = dlta
C
C*      Subtracting 1 from the input delta value.  Checking each bit 
C*      position and resetting for 2's compliment.  This is done for
C*      any values that are negatively signed.
C
	IF ( ibits( ibte, 6, 1) .eq. 1 ) THEN
            subnum = ibits (ibte, 0, 7 ) - 1
            DO i = 1, 6
                tmpbt = ibits( subnum, i-1, 1) 
		IF ( tmpbt .eq. 0 ) THEN
		    tmpbt = 1
                  ELSE
		    tmpbt = 0
                END IF 
		sum(i) = tmpbt
            END DO
            newnum = ( sum(1)*1 + sum(2)*2 + sum(3)*4 + sum(4)*8
     +             + sum(5)*16 + sum(6)*32 ) * (-1)
          ELSE
	     newnum = ibits ( ibte, 0, 7 )	   
	END IF
C*
	RETURN
	END
