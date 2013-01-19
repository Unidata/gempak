	SUBROUTINE G2T_COMPASS ( cdir, kdir, iret )
C************************************************************************
C* G2T_COMPASS	 							*
C*									*
C* This subroutine converts a 8-point compass direction to wind 	*
C* direction.								*
C* 									*
C* G2T_COMPASS ( CDIR, KDIR, IRET )					* 
C*									*
C* Input parameters:							*
C*	CDIR		CHAR*		Compass direction 		*
C*									*
C* Output parameters:							*
C*	KDIR		INTEGER		Prevalent wind in degrees	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		 6/07						*
C* T. Lee/SAIC		 8/07	Added missing value			*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	cdir
	CHARACTER*2	dirs(8)
	DATA		dirs /'NE','E','SE','S','SW','W','NW','N'/
C-----------------------------------------------------------------------
	iret = 0
C
	IF  ( cdir .eq. ' ' )  THEN
	    kdir = IMISSD
	    RETURN
	  ELSE
C
	    DO ii = 1, 8
		IF ( cdir .eq. dirs ( ii ) )  THEN
		    kdir = ii
		    RETURN
		END IF
	    END DO
	END IF
C*
	RETURN
	END
