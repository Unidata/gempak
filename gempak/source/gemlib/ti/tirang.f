	SUBROUTINE TI_RANG  ( dattim, strtim, endtim, mrange, iret )
C************************************************************************
C* TI_RANG								*
C*									*
C* This subroutine computes the range of the input time in minutes.	*
C*									*
C* TI_RANG  ( DATTIM, STRTIM, ENDTIM, MRANGE, IRET )			*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*72		Date/time input			*
C*									*
C* Output parameters:							*
C*	STRTIM		CHAR*		Start time			*
C*	ENDTIM		CHAR*		End time			*
C*	MRANGE		INTEGER		Time range in minutes		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C**									*
C* Log:									*
C* T. Lee/GSC		 5/01						*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)   dattim, strtim, endtim 
	CHARACTER	tarr(2)*20, systim*20
	INTEGER		itype
C------------------------------------------------------------------------
	iret = 0
	strtim = ' '
	endtim = ' '
	mrange = 0
C
	IF  ( dattim .eq. ' ' )  RETURN
C
C*	Parse the date/time input.
C
        CALL ST_CLST ( dattim, '-', ' ', 2, tarr, ntarr, iret )
        strtim = tarr (1)
	IF  ( ntarr .lt. 2 )  THEN
	    endtim = strtim
	  ELSE
	    endtim = tarr (2)
	END IF
C
C*	Create standard GEMPAK time.
C
	itype = 1
        CALL CSS_GTIM ( itype, systim, ier )
        CALL TI_STAN ( strtim, systim, strtim, ier )
        CALL TI_STAN ( endtim, strtim, endtim, ier )
        CALL TI_STAN ( endtim, systim, endtim, ier )
C
C*      Compute time range.
C
        CALL TI_DIFF ( endtim, strtim, mrange, iret )
C*
	RETURN
	END
