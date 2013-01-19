	SUBROUTINE AF_ATBX  ( istart, maxsc, dgotin, dgot, fqot, tpot,
     +			      iret )
C************************************************************************
C* AF_ATBX								*
C*									*
C* This subroutine checks the syntax and then optionally decodes an     *
C* expanded turbulence definition from an AIREP report. 	 	*
C*									*
C* AF_ATBX ( ISTART, MAXSC, DGOTIN, DGOT, FQOT, TPOT, IRET )            *
C*									*
C* Input parameters:							*
C*	ISTART		INTEGER	     Index of starting "like-type" group*
C*	MAXSC 		INTEGER	     No. of "like-type" groups to check *
C*	DGOTIN		REAL	     Degree of turbulence already found *
C*									*
C* Output parameters:							*
C*	DGOT		REAL	     Degree of turbulence 		*
C*	FQOT		REAL	     Frequency of turbulence		*
C*	TPOT		REAL	     Type of turbulence 		*
C*	IRET		INTEGER	     Return code 			*
C*				       0 = normal return 		*
C*				      -1 = no turbulence data found     *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/00	                                        *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	LOGICAL		chkfrq, done
C*
	PARAMETER	( NTPIDS = 4 )
	CHARACTER	tpids ( NTPIDS )*4
C*
	PARAMETER	( NFQIDS = 12 )
	CHARACTER	fqids ( NFQIDS )*3
	INCLUDE		'ERMISS.FNC'
	INCLUDE		'affnc.fnc'
C*
	DATA		tpids
     +			/ 'CAT ', 'CHOP', 'LLWS', 'CHP ' /
C*
	DATA		fqids
     +			/ 'CON', 'STE', 'CNT', 'STD', 'CNS', 'INT',
     +			  'OCN', 'OCA', 'OCC', 'OCS', 'ONC', 'ISO' /
C*
C-----------------------------------------------------------------------
	iret  = 0
	isdx  = istart
	dgot  = RMISSD
	fqot  = RMISSD
	tpot  = RMISSD
	dgot2 = RMISSD
C
	IF ( .not. ERMISS ( dgotin ) ) isdx = isdx + 1
C
	maxii  = IEDX ( isdx, maxsc, nflds )
	dgot1  = dgotin
	chkfrq = .false.
	done   = .false.
	ii     = isdx
	IF ( ii .gt. maxii )  done = .true.
C
C*	Parse the syntax of the turbulence definition.
C
	DO WHILE ( .not. done )
C
C*	    Check for a frequency indicator.
C
	    CALL ST_FIND ( fields ( ii ) (:3), fqids, NFQIDS, jfq, ier )
	    IF ( ( jfq .gt. 0 ) .and. ( ERMISS ( fqot ) ) ) THEN
C
C*		Get the frequency of turbulence.
C
		IF ( jfq .le. 5 ) THEN
		    fqot = 3.
		  ELSE IF ( jfq .le. 6 ) THEN
		    fqot = 2.
		  ELSE
		    fqot = 1.
		END IF
		IF ( ( .not. ERMISS ( dgot1 ) ) .and. 
     +		     ( ERMISS ( dgot2 ) ) )  chkfrq = .true.
	      ELSE
C
C*		Check for an intensity indicator.
C
		CALL AF_TBID ( fields (ii) (:lensf (ii)), dgotqq, ier )
		IF ( .not. ERMISS ( dgotqq ) ) THEN
		    IF ( ERMISS ( dgot1 ) ) THEN
			dgot1 = dgotqq
		      ELSE
			IF ( ERMISS ( dgot2 ) ) dgot2 = dgotqq
		    END IF
		  ELSE
C
C*		    Check for a type indicator.
C
		    CALL ST_FIND ( fields ( ii ) (:4), tpids, NTPIDS,
     +				   jtp, ier )
		    IF ( ( jtp .gt. 0 ) .and. ( ERMISS ( tpot ) ) ) THEN
C
C*			Get the type of turbulence.
C
			IF ( jtp .eq. 4 )  jtp = 2
			tpot = FLOAT ( jtp )
		    END IF
		END IF
	    END IF
C
	    IF ( ( ii .eq. maxii ) .or. 
     +		 ( fields ( ii ) (:2) .eq. 'IC' ) .or.
     +		 ( fields ( ii ) (:2) .eq. 'WX' ) .or.
     +		 ( fields ( ii ) (:3) .eq. 'MID' ) )  THEN
		done = .true.
	      ELSE
		ii = ii + 1
	    END IF
	END DO
C
C*	Check to see if there is a range of intensities.
C
	IF ( ( .not. ERMISS ( dgot1 ) ) .and. 
     +	     ( .not. ERMISS ( dgot2 ) ) ) THEN
	    dgot = NINT ( dgot1 + dgot2 ) / 2
	    IF ( NINT ( dgot ) .eq. 7 ) THEN
	        dgot = 8.
	      ELSE IF ( NINT ( dgot ) .eq. 1 ) THEN
		dgot = 2.
	    END IF
C
C*	    If frequency indicator only applies to second intensity,
C*	    drop it.
C
            IF ( chkfrq )  fqot = RMISSD
	  ELSE
	    dgot = dgot1
	END IF
C
	IF ( ERMISS ( dgot ) .and. ERMISS ( fqot ) .and.
     +	     ERMISS ( tpot ) ) iret = -1
C*
	RETURN
	END
