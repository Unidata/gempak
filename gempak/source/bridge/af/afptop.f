	SUBROUTINE AF_PTOP  ( istlyr, ietlyr, iptr, htop, iret )
C************************************************************************
C* AF_PTOP								*
C*									*
C* This subroutine decodes the top height of an element within a PIREP  *
C* report.								*
C*									*
C* AF_PTOP ( ISTLYR, IETLYR, IPTR, HTOP, IRET ) 	                *
C*									*
C* Input parameters:							*
C*	ISTLYR		INTEGER		Index of "like-type" group which*
C*					contains start of layer		*
C*	IETLYR		INTEGER		Index of "like-type" group which*
C*					contains end of layer		*
C*									*
C* Output parameters:							*
C*	IPTR		INTEGER		Pointer to next element         *
C*	HTOP		REAL		Top of element in feet    	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/99                                           *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	PARAMETER	( NTIDS = 2 )
	CHARACTER	tids ( NTIDS )*3
C*
	LOGICAL		tophgt
C*
	INCLUDE		'ERMISS.FNC'
	INCLUDE		'affnc.fnc'
C*
	DATA		tids
     +			/ 'TOP', 'TPS' /
C*
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret  = 0
	ifptr = istlyr
	htop  = RMISSD
C
C*	Check for a top height.
C
	tophgt = .false.
	IF ( ifptr .le. ietlyr ) THEN
	    IF ( ( lensf ( ifptr ) .eq. 1 ) .and.
     +	         ( fields ( ifptr ) (1:1) .eq. '-' ) ) THEN
	        ifptr  = ifptr + 1
		tophgt = .true.
	    END IF
C
C*	    Check at most the next four "like-type" groups for a TOP 
C*	    keyword.
C
	    ii    = ifptr
	    maxii = IEDX ( ii, 3, ietlyr )
	    it    = 0
	    DO WHILE ( ( ii .le. maxii ) .and. ( it .eq. 0 ) ) 
		CALL ST_FIND ( fields (ii) (1:3), tids, NTIDS, it, ier )
		ii = ii + 1
		IF ( it .gt. 0 ) THEN
		    ifptr  = ii
		    tophgt = .true.
C
C*		    A TOP keyword was found.  Skip the next group if
C*		    it is not numeric.
C
		    IF ( itypsf ( ifptr ) .ne. NMR ) ifptr = ifptr + 1
		END IF
	    END DO
C
	    IF ( tophgt .and. ( ifptr .le. ietlyr ) ) THEN
		IF ( itypsf ( ifptr ) .eq. NMR ) THEN
C
C*		    Get the top height.
C
		    CALL AF_HHFM ( fields ( ifptr ) ( :lensf (ifptr) ),
     +				   htop, ier )
C
C*		    Check to see if this group is followed by a hyphen
C*		    and a second top height.
C
		    IF ( ( ifptr + 2 ) .le. ietlyr ) THEN
		        IF ( ( fields ( ifptr + 1 ) ( 1:1 ) .eq. '-' )
     +			  .and. ( itypsf ( ifptr + 2 ) .eq. NMR ) ) THEN
			    ifptr = ifptr + 2
			    CALL AF_HHFM ( fields ( ifptr )
     +			         ( :lensf ( ifptr ) ), htop2, ier )
			    IF ( .not. ERMISS ( htop2 ) ) htop = htop2
			END IF
		    END IF
		    ifptr = ifptr + 1
		END IF
	      ELSE IF ( tophgt ) THEN
C
C*		The TOP keyword or hyphen ended the layer.  Do not
C*		mistake a second base height for a top height.
C
		htop = RMISSD
	    END IF
	END IF
	iptr = ifptr
C*
	RETURN
	END
