	SUBROUTINE AF_RTIM  ( irundt, ibuldt, irptdt, iret )
C************************************************************************
C* AF_RTIM								*
C*									*
C* This subroutine constructs a report date-time from the run date-time,*
C* bulletin date-time, report day (if it exists), report hour, and	*
C* report minutes.							*
C*									*
C* AF_RTIM  ( IRUNDT, IBULDT, IRPTDT, IRET )				*
C*									*
C* Input parameters:							*
C*	IRUNDT (5)	INTEGER		Run date-time			*
C*					= (YYYY, MM, DD, HH, MM ) 	*
C*	IBULDT (5)	INTEGER		Bulletin date-time		*
C*					= (YYYY, MM, DD, HH, MM ) 	*
C*									*
C* Output parameters:							*
C*	IRPTDT (5)	INTEGER		Report date-time		*
C*					= (YYYY, MM, DD, HH, MM ) 	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = could not compute IRPTDT	*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Keep reports within 9 hours before or	*
C*				4 hours after bulletin date-time 	*
C* J. Ator/NP12		01/97	Keep reports within 10 days before	*
C*				run date-time 				*
C* J. Ator/NP12		02/97	Keep reports within 9 hours before or	*
C*				1 hour after bulletin date-time 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		12/97	AF_RTMD -> DC_RTIM			*
C* J. Ator/NCEP		12/98	Added YEAR, MNTH, DAYS to /INTF/	*
C* J. Ator/NCEP		04/00	Use report day if it exists		*
C* J. Ator/NCEP		10/00	AF_RTMH -> DC_RTMH; change call sequence*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	INTEGER		irundt (*), ibuldt (*), irptdt (*)
C-----------------------------------------------------------------------
	iret = -1
C
C*	Retrieve the report hour and report minutes from the
C*	interface arrays.
C
	irpthr = INT ( rivals ( irhour ) )
	irptmn = INT ( rivals ( irminu ) )
	IF  ( ( irpthr .eq. IMISSD ) .or.
     +	      ( irptmn .eq. IMISSD ) )  THEN
	    RETURN
	END IF
C
C*	Does the report day exist?
C
	irptdy = INT ( rivals ( irdays ) )
	IF  ( irptdy .ne. IMISSD )  THEN
C
C*	    Use the run date-time, report day, report hour, and report
C*	    minutes to construct a report date-time.
C
	    CALL DC_RTIM  ( irundt, irptdy, irpthr, irptmn, 10, irptdt,
     +			    ierrtm )
	    IF  ( ierrtm .ne. 0 )  THEN
		logmsg = 'DC_RTIM'
		CALL DC_WLOG  ( 2, 'AF', 4, logmsg, ierwlg )
		RETURN
	    END IF
	ELSE
C
C*	    Use the bulletin date-time, report hour, and report minutes
C*	    to construct a report date-time.
C
	    CALL DC_RTMH  ( ibuldt, irpthr, irptmn, 9, 1, irptdt,
     +			    ierrth )
	    IF  ( ierrth .ne. 0 )  THEN
		logmsg = 'DC_RTMH'
		CALL DC_WLOG  ( 2, 'AF', 4, logmsg, ierwlg )
		RETURN
	    END IF
C
C*	    Store the report day into the interface arrays.
C
	    rivals ( irdays ) = FLOAT ( irptdt (3) )
	END IF
C
C*	Store the report year and report month into the
C*	interface arrays.
C
	rivals ( iryear ) = FLOAT ( irptdt (1) )
	rivals ( irmnth ) = FLOAT ( irptdt (2) )
C
	iret = 0
C*
	RETURN
	END
