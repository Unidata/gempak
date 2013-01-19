	SUBROUTINE AW_2CLG ( dlta, newnum, iret )
C************************************************************************
C* AW_2CLG                                                              *
C*                                                                      *
C* This subroutine calculates the two's complement for a long vector.   *
C* 								        *
C* AW_2CLG ( DLTA, NEWNUM, IRET )                                *
C*								        *
C* Input parameters: 						        *
C*	DLTA   		INTEGER*2	Input long vector               *
C*								        *
C* Output parameters:						        *
C*	NEWNUM          INTEGER		New vector value                *
C*	IRET	   	INTEGER		Return code                     *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Added assignment for dlta               *
C************************************************************************
        INTEGER*2      dlta
        INTEGER        tmpbt, newnum, subnum
	INTEGER        sum(12)
        INTEGER        ibte 
C------------------------------------------------------------------------
 	iret = 0
        ibte = dlta
C
C*      Subtracting 1 from the input delta value.
C*      Checking each bit position and resetting for 2's compliment. 
C
	IF ( ibits ( ibte, 12, 1) .eq. 1 ) THEN
            subnum = ibits ( ibte, 0, 13) - 1
            DO i = 1, 12 
                tmpbt = ibits( subnum, i-1, 1) 
		IF ( tmpbt .eq. 0 ) THEN
		    tmpbt = 1
                  ELSE
		    tmpbt = 0
                END IF 
		sum(i) = tmpbt
            END DO
            newnum = ( sum(1)*1 + sum(2)*2 + sum(3)*4 + 
     +                sum(4)*8 + sum(5)*16 + sum(6)*32 + 
     +                sum(7)*64 + sum(8)*128 + sum(9)*256 + 
     +                sum(10)*512 + sum(11)*1024 + 
     +                sum(12)*2048 ) * (-1)
	  ELSE
	      newnum = ibits ( ibte, 0, 13)
        END IF
C*
	RETURN
	END
