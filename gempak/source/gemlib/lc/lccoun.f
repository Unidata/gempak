	SUBROUTINE LC_COUN  ( stcn, cnflag, iret )
C************************************************************************
C* LC_COUN								*
C*									*
C* This subroutine checks STCN to see if it is a country abbreviation.	*
C* The following countries are currently recognized:			*
C*									*
C*	US	United States		CN	Canada			*
C*	MX	Mexico			BW	Bangladesh		*
C*	AU	Australia		CI	China			*
C*									*
C* Countries whose abbreviations will conflict with US state names	*
C* should not be added to this list.					*
C*									*
C* LC_COUN  ( STCN, CNFLAG, IRET )					*
C*									*
C* Input parameters:							*
C*	STCN		CHAR*		State / country abbreviation	*
C*									*
C* Output parameters:							*
C*	CNFLAG		LOGICAL		Country flag			*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87						*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C************************************************************************
	PARAMETER	( NCOUN = 6 )
	CHARACTER*(*)	stcn
	LOGICAL		cnflag
C*
	CHARACTER	cnlist (NCOUN)*12
	DATA cnlist	/ 'US', 'CN',' MX', 'BW', 'CI', 'AU' /
C------------------------------------------------------------------------
	iret   = 0
	cnflag = .false.
	icn    = 0
C
C*	Check to see if this is a country.
C
	DO  i = 1, NCOUN
	    IF  ( stcn .eq. cnlist (i) )  cnflag = .true.
	END DO
C*
	RETURN
	END
