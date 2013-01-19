	SUBROUTINE G2T_WRNX ( ntype, iret )
C************************************************************************
C* G2T_WRNX								*
C*									*
C* This subroutine sets the warning flags to .FALSE. for discontinuous	*
C* conditions from 5th to last period (NGRDTM).  Per NWS Instruction	*
C* 10-311, for the extended period, headline only for the first occur-	*
C* -rence of same category warning since second warning is the same	*
C* intensity.								*
C*									*
C* G2T_WRNX ( NTYPE, IRET )						*
C*									*
C* Input parameters:							*
C*	NTYPE		INTEGER		Storm type			*
C*					  1: Hurricane			*
C*					  2: Storm			*
C*					  3: Gale			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		 1/07						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	LOGICAL		wflag(ngrdtm)
	LOGICAL		done, first
C-----------------------------------------------------------------------
	iret = 0
C
C*	Fill flags from any storms.
C
	IF  ( ntype .eq. 1 )  THEN
	    DO ii = 1, ngrdtm
		wflag ( ii ) = fhurr ( ii )
	    END DO
	  ELSE IF ( ntype .eq. 2 )  THEN
	    DO ii = 1, ngrdtm
		wflag ( ii ) = fstorm ( ii )
	    END DO
	  ELSE IF ( ntype .eq. 3 )  THEN
	    DO ii = 1, ngrdtm
		wflag ( ii ) = fgale ( ii )
	    END DO
	END IF
C
C*	Check the 5th period (9th time step) and beyond.  If discontinuity 
C*	occurs, set the rest of the flags to .FALSE..		
C
	first = .true.
	done  = .false.
	nt = 9
C
	DO WHILE ( .not. done )
	    IF ( wflag ( nt ) )  THEN
		first = .false.
	      ELSE
		IF ( .not. first )  THEN
		    DO kk = nt, ngrdtm
			wflag ( kk ) = .false.
		    END DO
		    done = .true.
		END IF
	    END IF
	    nt = nt + 1
	    IF ( nt .gt. ngrdtm )  done = .true.
	END DO
C
C*	Reset the flags.
C
	IF  ( ntype .eq. 1 )  THEN
	    DO ii = 1, ngrdtm
		fhurr ( ii ) = wflag ( ii )
	    END DO
	  ELSE IF ( ntype .eq. 2 )  THEN
	    DO ii = 1, ngrdtm
		fstorm ( ii ) = wflag ( ii )
	    END DO
	  ELSE IF ( ntype .eq. 3 )  THEN
	    DO ii = 1, ngrdtm
		fgale ( ii ) = wflag ( ii )
	    END DO
	END IF
C*
	RETURN
	END
