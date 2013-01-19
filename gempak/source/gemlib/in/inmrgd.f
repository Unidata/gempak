	SUBROUTINE IN_MRGD  ( mrgdat, mrgflg, ipttyp, iret )
C************************************************************************
C* IN_MRGD								*
C*									*
C* This subroutine breaks the user input for MRGDAT into a flag 	*
C* indicating whether merged data are to be used and a type for		*
C* unmerged data.  The default for MRGFLG is true and for IPTTYP	*
C* is 3.								*
C*									*
C* IN_MRGD  ( MRGDAT, MRGFLG, IPTTYP, IRET )				*
C*									*
C* Input parameters:							*
C*	MRGDAT		CHAR*		User input for MRGDAT		*
C*									*
C* Output parameters:							*
C*	MRGFLG		LOGICAL		Merged file type		*
C*	IPTTYP		INTEGER		Unmerged type			*
C*					  1 = man below 100 mb		*
C*					  2 = man & sig below 100 mb	*
C*					  3 = man & sig below & above	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* M. desJardins/GSFC	 4/90	Change default for mrgdat to YES	*
C************************************************************************
	CHARACTER*(*)	mrgdat
	LOGICAL		mrgflg
C*
	CHARACTER	clist (2)*12
C-----------------------------------------------------------------------
	iret = 0
C
C*	Decode input for MRGDAT.
C
	CALL ST_CLST  ( mrgdat, '/', ' ', 2, clist, n, ier )
	CALL ST_LCUC  ( clist (1), clist (1), ier )
	IF  ( clist (1) (1:1) .eq. 'N' )  THEN
	    mrgflg = .false.
	    CALL ST_NUMB  ( clist (2), ipttyp, ier )
	    IF  ( ( ipttyp .lt. 1 ) .or. ( ipttyp .gt. 3 ) )  ipttyp = 3
	  ELSE
	    mrgflg = .true.
	    ipttyp = 0
	END IF
C*
	RETURN
	END
