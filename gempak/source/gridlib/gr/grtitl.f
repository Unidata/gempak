	SUBROUTINE GR_TITL ( ttlinp, gdtime, ilevfg, level, ivcord,
     +			     parm, iscale, gpoint, ttlstr, shrttl,
     +			     iret )
C************************************************************************
C* GR_TITL								*
C*									*
C* This subroutine creates a title to be displayed in the graphics	*
C* programs. If imbedded within the input title string, characters	*
C* are replaced as follows:						*
C*		^	Time string					*
C*		~	Valid time string				*
C*		@	Level string					*
C*		_	Function/parameter string			*
C*		$	Scaling factor string				*
C*		#	Grid point/station string			*
C*		?	Day of the week flag to include with time	*
C*									*
C* GR_TITL  ( TTLINP, GDTIME, ILEVFG, LEVEL, IVCORD, PARM, ISCALE,	*
C*	      GPOINT, TTLSTR, SHRTTL, IRET )				*
C*									*
C* Input parameters:							*
C*	TTLINP		CHAR*		Input title string		*
C*	GDTIME (2)	CHAR*		Grid time			*
C*	ILEVFG		LOGICAL		Level flag			*
C*	LEVEL  (2)	INTEGER		Grid level			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	PARM		CHAR*12		Grid parameter			*
C*	ISCALE		INTEGER		Scaling factor			*
C*	GPOINT		CHAR*		Grid Point			*
C*									*
C* Output parameters:							*
C*	TTLSTR		CHAR*		Title string			*
C*	SHRTTL		CHAR*		Short title string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* J. Nielsen/SUNYA	 2/91	Added ^, @, _, and *			*
C* J. Whistler/SSAI	 4/91	Added # for gpoint			*
C* J. Whistler/SSAI	 4/91	Changed scale "*" to "$"		*
C* S. Jacobs/EAI	 9/92	Changed section to get levels and coord	*
C*				to allow the use of any vcoord		*
C* S. Jacobs/EAI	11/92	Added calculation of short title	*
C* S. Jacobs/EAI	 9/93	Added ~ for valid time flag		*
C* S. Jacobs/EAI	 9/93	Added GR_SHRT to compute short title	*
C* S. Jacobs/EAI	10/93	Made GPOINT upper case			*
C* S. Jacobs/NMC	 8/94	Added call to GR_MTTL			*
C* S. Jacobs/NMC	 9/94	Fixed default title for plots w/ GPOINT	*
C* T. Piper/GSC		 7/01	Increased ttl from 150 -> 152		*
C************************************************************************
	CHARACTER*(*)	ttlinp, gdtime(2), parm, gpoint, ttlstr, shrttl
	INTEGER		level (2)
	LOGICAL		ilevfg
C*
	CHARACTER	ttl*152, shrtin*72, defttl*12, defshr*12
C------------------------------------------------------------------------
	iret = 0
C
C*	Break the title string into the title and the short title.
C
	ipos = INDEX  ( ttlinp, '|' )
	IF  ( ipos .eq. 1 )  THEN
	    ttl    = ' '
	    shrtin = ttlinp (2 : )
	  ELSE IF  ( ipos .gt. 1 )  THEN
	    ttl    = ttlinp ( : ipos-1 )
	    shrtin = ttlinp ( ipos+1 : )
	  ELSE
	    ttl    = ttlinp
	    shrtin = ' '
	END IF
C
C*	Set default title and short title.
C
	IF  ( ( ilevfg ) .and. ( gpoint .eq. ' ' ) )  THEN
	    defttl = '~  @  _$'
	    defshr = '~ @ _'
	ELSE
	    defttl = '~  _$  #'
	    defshr = '~ _ #'
	END IF
C
C*	Construct the title.
C
	CALL GR_MTTL  ( ttl, defttl, .false., gdtime(1), gdtime(2),
     +			ilevfg, level(1), level(2), ivcord, 1,
     +			parm, iscale, gpoint, ttlstr, iret )
C
C*	Construct the short title.
C
	CALL GR_MTTL  ( shrtin, defshr, .true., gdtime(1), gdtime(2),
     +			ilevfg, level(1), level(2), ivcord, 1,
     +			parm, iscale, gpoint, shrttl, iret )
C*
	RETURN
	END
