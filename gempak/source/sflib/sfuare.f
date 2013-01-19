	SUBROUTINE SF_UARE  ( isffln, area, newfil, arecur, stn, iret )
C************************************************************************
C* SF_UARE								*
C*									*
C* This subroutine sets the search criteria in a surface file using	*
C* the value for AREA input by the user.  The area may be composed of	*
C* subareas which are separated by slashes (/).  This subroutine	*
C* will be more efficient than the equivalent LC_UARE when searching	*
C* for a single station at multiple times.  If the search is not for	*
C* a single station, the appropriate calls to the LC library will	*
C* be made.								*
C*									*
C* SF_UARE  ( ISFFLN, AREA, NEWFIL, ARECUR, STN, IRET )			*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	AREA		CHAR*		Area to be defined		*
C* 	NEWFIL		LOGICAL		New file flag			*
C*									*
C* Input and output parameters:						*
C*	ARECUR		CHAR*		Current area			*
C*									*
C* Output parameters:							*
C*	STN		CHAR*		Center station name		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return 		*
C*					  -1 = invalid area name	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/90						*
C* J. Whistler/SSAI	 3/91		Added a check for a colon in 	*
C*					the area parameter.		*
C* T. Lee/GSC		 5/97		Set ARECUR for single station	*
C* T. Lee/GSC		 5/01		Fixed return code		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)   stn, area, arecur
	LOGICAL         newfil
C------------------------------------------------------------------------
C*	Check if the area is the same.
C
	iret = 0
	IF  ( ( area .ne. arecur ) .or. ( newfil ) .or. 
     +	    ( area .eq. ' ') )  THEN
C
C*	    Check for a single station input.
C
	    onestn ( isffln ) = .false.
	    islash = INDEX ( area, '/' )
	    isemic = INDEX ( area, ';' )
	    icolon = INDEX ( area, ':' )
	    CALL ST_LSTR  ( area, iareln, ier )
	    IF  ( ( islash .eq. 0 ) .and. ( isemic .eq. 0 ) .and.
     +		  ( icolon .eq. 0 ) .and. ( area (1:1) .eq. '@' ) .and.
     +		  ( iareln .gt. 3 ) .and.
     +		  ( sttype ( isffln ) .ne. dttype ( isffln ) ) )  THEN
C
C*		Set the flag to indicate this is one station.  Then
C*		set the station.
C
		onestn ( isffln ) = .true.
		stn    = area (2: )
		CALL SF_FSTN  ( isffln, stn, ier )
		iret = ier
	      ELSE
C
C*		Set up the search area.
C
		CALL LC_SARE  ( area, isffln, stn, ier )
	    END IF
C
C*	    Return errors.
C
	    IF  (  ier .ne. 0 )  THEN
		arecur = ' '
	      ELSE
		arecur = area
	    END IF
	END IF
C*
	RETURN
	END
