	SUBROUTINE PC_MVRT ( datain, iret )
C************************************************************************
C* PC_MVRT								*
C*									*
C* This subroutine computes vertical coordinate data for a station.	*
C* The following parameters are computed at every level in the data	*
C* set:  PRES, THTA, HGHT, SGMA, TEMP, DWPT.  Note that SGMA cannot	*
C* currently be computed.						*
C*									*
C* PC_MVRT ( DATAIN, IRET )						*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*)
C*
	REAL		data (MAXPRM), datout (MAXPRM)
C-----------------------------------------------------------------------
	iret  = 0
	index = 4
C
C*	Check to see if vertical data has already been computed.
C
	IF  ( .not. vtbflg )  THEN
	    DO  ilev = 1, jnumlv
		CALL  PC_GLEV  ( ilev, datain, jdsprm, data, ier )
		CALL  PC_COMP  ( index, data, datout, ier )
		DO  i = 1, MAXVRT
		    IF  ( vcomp (i) )  THEN
			vdata ( i, ilev ) = datout (i)
		      ELSE
			vdata ( i, ilev ) = RMISSD
		    END IF
		END DO
	    END DO
	END IF
C
C*	Set flag to indicate vertical table has been made.
C
	vtbflg = .true.
C*
	RETURN
	END
