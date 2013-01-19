	SUBROUTINE G2T_SORT ( ktype, nz, iret )
C************************************************************************
C* G2T_SORT								*
C*									*
C* This subroutine sorts wind and wave heights data based on its	*
C* frequency distribution. The maximum  are stored in the lowest index 	*
C* number.								*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER			Data type		*
C*						 1 = wave		*
C*						 2 = wind		*
C*	NZ		INTEGER			Nth zone area		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER			Return code		*
C*						  0 = normal return	*
C*						-11 = incorrect type	*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		11/06						*
C* T. Lee/SAIC		 3/08	Cleaned up				*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	IF  ( ktype .eq. 1 )  THEN
	    kr = MXWHGT
	  else if ( ktype .eq. 2 )  THEN
	    kr = MXSPED
	  else
	    iret = -11
	    RETURN
	END IF
	kr_1 = kr - 1 
C
	DO nb = 1, nsubzn ( nz ) 
	    DO nt = 1, ngrdtm 
		DO kx = 1, kr_1
		    DO ky = kx + 1, kr
			IF ( ktype .eq. 1 )  THEN
			    IF ( wavpct ( nb, nt, ky ) .ge. 
     +				 wavpct ( nb, nt, kx) )  THEN
				 b1 = wavpct ( nb, nt, ky )
				 b2 = wavpct ( nb, nt, kx )
				 k1 = icwave ( nb, nt, ky )
				 k2 = icwave ( nb, nt, kx )
				 wavpct ( nb, nt, ky ) = b2
				 wavpct ( nb, nt, kx ) = b1
				 icwave ( nb, nt, ky ) = k2
				 icwave ( nb, nt, kx ) = k1
			    END IF
			  ELSE
			    IF ( winpct ( nb, nt, ky ) .ge. 
     +				 winpct ( nb, nt, kx) )  THEN
				 b1 = winpct ( nb, nt, ky )
				 b2 = winpct ( nb, nt, kx )
				 k1 = icwind ( nb, nt, ky )
				 k2 = icwind ( nb, nt, kx )
				 winpct ( nb, nt, ky ) = b2
				 winpct ( nb, nt, kx ) = b1
				 icwind ( nb, nt, ky ) = k2
				 icwind ( nb, nt, kx ) = k1
			    END IF
			END IF
		    END DO
		END DO
	    END DO
	END DO
C*
	RETURN
	END
