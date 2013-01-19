	SUBROUTINE G2T_MWDIR ( kdir, rdir, iret )
C************************************************************************
C* G2T_MWDIR	 							*
C*									*
C* This subroutine computes mean cardinal wind direction.		*
C* 									*
C* G2T_MWDIR ( KDIR, RDIR, IRET )					* 
C*									*
C* Input parameters:							*
C*	KDIR (2)	INTEGER		Cardinal wind direction		*
C*									*
C* Output parameters:							*
C*	RDIR		REAL		Mean Cardinal wind direction	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		 6/07						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	INTEGER		kdir(2)
C-----------------------------------------------------------------------
	iret = 0
C
C
C*	Get the mean cardinal wind direction.
C
	IF (  kdir ( 1 ) .ne. IMISSD .and. kdir ( 2 ) .ne. IMISSD ) THEN
	    IF ( kdir ( 1 ) .ne. 8 .and. kdir ( 2 ) .ne. 8 )  THEN
		rdir  = ( kdir ( 1 ) + kdir ( 2 ) ) / 2.
	      ELSE
		IF ( kdir ( 1 ) .eq. 8 )  THEN
		    IF ( kdir ( 2 ) .eq. 1 )  THEN
			rdir = .5
		      ELSE
			rdir = ( kdir ( 1 ) + kdir ( 2 ) ) / 2.
		    END IF
		  ELSE
		    IF ( kdir ( 1 ) .eq. 1 )  THEN
			rdir = .5
		      ELSE
			rdir = ( kdir ( 1 ) + kdir ( 2 ) ) / 2.
                    END IF
                END IF
            END IF
	  ELSE
	    IF ( kdir ( 1 ) .eq. IMISSD )  THEN
		rdir = kdir ( 2 )
	      ELSE
		rdir = kdir ( 1 )
	    END IF
	END IF
C*
	RETURN
	END
