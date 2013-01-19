	SUBROUTINE GG_LNUP ( fnlpts, iclr, iltyp, iret )
C************************************************************************
C* GG_LNUP								*
C*									*
C* This subroutine connects the quadrants of the wind and sea feet arcs *
C*									*
C* GG_LNUP ( FNLPTS, ICLR, ILTYP, IRET )				*
C*									*
C* Input parameters:							*
C*	FNLPTS (4,4)	REAL		Beginning/end arc points	*
C*	ICLR		INTEGER		Line color			*
C*	ILTYP		INTEGER		Line type			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 6/00   					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        REAL            fnlpts(4,4)
C*
        REAL		slat(2), slon(2)
C-----------------------------------------------------------------------
	iret = 0
C
C*      Set color and line type.
C
 	CALL GSCOLR ( iclr, ier )
        CALL GSLINE ( iltyp, 0 , 2, 0, ier )
        DO jj = 1,4
C
C*          Set up the points to be converted back to map coords.
C
            IF  (jj .eq. 1) THEN
                   slat(1)  = fnlpts(4,3)
                   slon(1)  = fnlpts(4,4)
                   slat(2)  = fnlpts(1,1)
                   slon(2)  = fnlpts(1,2)
            END IF
 	    IF  (jj .eq. 2) THEN
                   slat(1)  = fnlpts(1,3)
                   slon(1)  = fnlpts(1,4)
                   slat(2)  = fnlpts(2,1)
                   slon(2)  = fnlpts(2,2)
            END IF
 	    IF  (jj .eq. 3) THEN
                   slat(1)  = fnlpts(2,3)
                   slon(1)  = fnlpts(2,4)
                   slat(2)  = fnlpts(3,1)
                   slon(2)  = fnlpts(3,2)
            END IF
            IF  (jj .eq. 4) THEN
                   slat(1)  = fnlpts(3,3)
                   slon(1)  = fnlpts(3,4)
                   slat(2)  = fnlpts(4,1)
                   slon(2)  = fnlpts(4,2)
            END IF
C
C*	    Draw connecting lines between arc segments.
C
	    CALL GLINE ( 'M', 2, slat, slon, ier )
        END DO
C*
	RETURN
	END
