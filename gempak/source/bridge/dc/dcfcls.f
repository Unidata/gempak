	SUBROUTINE DC_FCLS ( iret )
C************************************************************************
C* DC_FCLS								*
C*									*
C* This routine closes all the open output files based on the type	*
C* of the file.								*
C*									*
C* DC_FCLS ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/95						*
C* S. Jacobs/NCEP	 7/96	Changed type 5 from FL_CLOS to SF_CLOS	*
C* A. Hardy/GSC		 5/99   Added type 6 for ASCII files            *
C************************************************************************
	INCLUDE		'dccmn.cmn'
C------------------------------------------------------------------------
	iret = 0
C
C*	Close all open files.
C
	DO  i = 1, maxfil
	    IF  ( lunopn (i) .ne. 0 )  THEN
		IF  ( ( iftype .eq. 1 ) .or.
     +		      ( iftype .eq. 2 ) .or.
     +		      ( iftype .eq. 5 ) )  THEN
C
C*		    Close a surface file.
C
		    CALL SF_CLOS ( lunopn (i), ier )
		  ELSE IF  ( ( iftype .eq. 3 ) .or.
     +			     ( iftype .eq. 4 ) )THEN
C
C*		    Close a sounding file.
C
		    CALL SN_CLOS ( lunopn (i), ier )
		  ELSE IF ( iftype .eq. 6 ) THEN
C
C*		    Close an ASCII file.
C
		    CALL FL_CLOS ( lunopn (i), ier )
		END IF
		filopn (i) = ' '
		lunopn (i) = 0
		numopn (i) = 0
	    END IF
	END DO
C*
	RETURN
	END
