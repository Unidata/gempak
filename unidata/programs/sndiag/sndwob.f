	FUNCTION SND_WOB ( temp )
C************************************************************************
C* SND_WOB								*
C*									*
C* This function will help compute the Wet-bulb potential temperature.	*
C*									*
C* SND_WOB ( TEMP )							*
C*									*
C* Input parameters:							*
C*	TEMP		REAL		Temperature			*
C*									*
C* Output parameters:							*
C*	SND_WOB		REAL		Wobus function			*
C**									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C------------------------------------------------------------------------
	xxx = temp - 20.
	IF  ( xxx .gt. 0. ) THEN
	    pol = 1.                   + xxx * (  3.6182989e-03 +
     +		xxx * ( -1.3603273e-05 + xxx * (  4.9618922e-07 +
     +		xxx * ( -6.1059365e-09 + xxx * (  3.9401551e-11 +
     +		xxx * ( -1.2588129e-13 + xxx * (  1.6688280e-16 )))))))
	    SND_WOB = ( 29.930 / ( pol**4) ) + ( .96 * xxx ) - 14.8
	ELSE
	    pol = 1.                   + xxx * ( -8.8416605e-03 +
     +		xxx * (  1.4714143e-04 + xxx * ( -9.6719890e-07 +
     +		xxx * ( -3.2607217e-08 + xxx * ( -3.8598073e-10)))))
	    SND_WOB = 15.130 / ( pol**4)
	END IF
C*
	RETURN
	END
