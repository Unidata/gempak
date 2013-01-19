	SUBROUTINE RD_MV12 ( ps12tb, iprms, rdata, iret )
C************************************************************************
C* RD_MV12								*
C*									*
C* This subroutine moves 12-hour parameters in 3-hour synoptic time	*
C* postions to 6-hour synoptic time positions based on the position	*
C* moving table.							*
C*									*
C* RD_MV12  ( PS12TB, IPRMS, RDATA, IRET )				*
C*									*
C* Input parameters:							*
C*      PS12TB (*)	INTEGER		Position moving table for 12-hr *
C*	IPRMS (*)	INTEGER		Position of parameter in output *
C*									*
C* Input and output parameters:						*
C*	RDATA		REAL		Forecast data			*
C*	   ( MMPARM,* )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C* F. J. Yen/NCEP	11/02		Updated prolog and for overflow	*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	INTEGER		ps12tb (*), iprms (*)
    	REAL            rdata ( MMPARM, * )
C------------------------------------------------------------------------
	iret = 0
C*
C 
C*	Loop through the 12-hourly parameters which are the first 6
C*	parameters (in cprms defined in rddcod) and move to 6 hour
C*	synoptic time positions if necessary.
C
	DO ii = 1,6
	    ipm = iprms (ii)
	    DO jj = 22, 1, -1
		IF ( rdata ( ipm, jj ) .ne. RMISSD ) THEN
		    IF ( ps12tb (jj) .eq. 1 ) THEN
			rdata ( ipm, jj + 1 ) = rdata ( ipm, jj ) 
			rdata ( ipm, jj ) = RMISSD
		      ELSE IF ( ps12tb (jj) .eq. - 1 ) THEN
			IF ( jj .ne. 1 ) THEN
			    rdata ( ipm, jj -1 ) = rdata ( ipm, jj )
			    rdata ( ipm, jj ) = RMISSD
			END IF
		    END IF
		END IF
	    END DO
	END DO
C*
	RETURN
	END
