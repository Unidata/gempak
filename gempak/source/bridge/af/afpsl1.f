	SUBROUTINE AF_PSL1  ( istlyr, ietlyr, iclam, hocb, hoct, iret )
C************************************************************************
C* AF_PSL1								*
C*									*
C* This subroutine decodes a layer of sky cover data from within a	*
C* PIREP report, using the alternate format of hhh NNN hhh.		*
C*									*
C* AF_PSL1 ( ISTLYR, IETLYR, ICLAM, HOCB, HOCT, IRET )                  *
C*									*
C* Input parameters:							*
C*	ISTLYR		INTEGER		Index of "like-type" group which*
C*					contains start of layer		*
C*	IETLYR		INTEGER		Index of "like-type" group which*
C*					contains end of layer		*
C*									*
C* Output parameters:							*
C*	ICLAM		INTEGER		Cloud amount           		*
C*	HOCB		REAL		Base of cloud in feet	 	*
C*	HOCT		REAL		Top of cloud in feet    	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = could not decode layer 	*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 6/99                                           *
C* D. Kidwell/NCEP	 7/99 	Modified comments, prolog meters -> feet*
C* D. Kidwell/NCEP	 8/99 	Changed variable clam to iclam          *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	PARAMETER	( NCIDS = 8 )
	CHARACTER	cids ( NCIDS )*3
C*
	LOGICAL		tophgt
	INCLUDE		'ERMISS.FNC'
	INCLUDE		'affnc.fnc'
C*
	DATA		cids
     +			/ 'SKC', 'SCT', 'BKN', 'OVC', 'CLR', 'FEW',
     +			  'IMC', 'UNK' /
C*
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret  = 0
	ifptr = istlyr
	iclam = IMISSD
	hocb  = RMISSD
	hoct  = RMISSD
	ic1   = 0
	ic2   = 0
C
C*	Check for a cloud base height.
C
	IF ( ( ifptr .le. ietlyr ) .and.
     +	     ( itypsf ( ifptr ) .eq. NMR ) ) THEN
	    CALL AF_HHFM ( fields ( ifptr ) ( :lensf ( ifptr ) ),
     +		           hocb, ier )
	    ifptr = ifptr + 1
	  ELSE
	    iret = -1
	    RETURN
	END IF
C
C*	Check up to 3 "like-type" groups of this layer to locate a
C*	cloud cover amount.
C
	ii    = ifptr
	maxii = IEDX ( ii, 2, ietlyr )
	DO WHILE ( ( ii .le. maxii ) .and. ( ic1 .eq. 0 ) )
	    CALL ST_FIND ( fields ( ii ) (1:3), cids, NCIDS, ic1, ier )
	    ii = ii + 1
	END DO
C
C*	Check to see if this group is followed by a second cloud cover
C*	amount.  The two amounts will be separated by a hyphen or other
C*	phrase.
C
	IF ( ic1 .gt. 0 ) THEN
	    ifptr = ii
	    IF ( ( ifptr + 1 ) .le. ietlyr ) THEN
	        IF ( itypsf ( ifptr ) .ne. NMR )  THEN
	            ifptr = ifptr + 1
		    CALL ST_FIND ( fields ( ifptr ) ( 1:3 ), cids,
     +				   NCIDS, ic2, ier )
		    IF ( ic2 .gt. 0 ) THEN
			iclam = ic2
C
C*			Ensure that the greater amount is saved.
C
			IF ( ( ic1 .gt. ic2 ) .and. ( ic1 .ne. 6 ) )
     +			       iclam = ic1
			ifptr = ifptr + 1
		    END IF
		END IF
	    END IF
	    IF ( ic2 .eq. 0 )   iclam = ic1
	    IF ( iclam .eq. 5 ) iclam = 1
	    IF ( iclam .eq. 7 ) iclam = 4
	    IF ( iclam .eq. 8 ) iclam = IMISSD
	END IF
C
C*	Check up to 3 "like type" groups for a cloud top height.
C
	ii     = ifptr
	maxii  = IEDX ( ii, 2, ietlyr )
	tophgt = .false.
	DO WHILE ( ( ii .le. maxii ) .and. ( .not. tophgt ) ) 
	    IF ( itypsf ( ii ) .eq. NMR ) THEN
C
C*	        Get the cloud top height.
C
		CALL AF_HHFM ( fields ( ii ) ( :lensf ( ii ) ),
     +			       hoct, ier )
		tophgt = .true.
	      ELSE
		ii = ii + 1
	    END IF
	END DO
C
	ifptr = ii
	IF ( tophgt .and. ( ifptr .le. ietlyr ) ) THEN
C
C*	    Check to see if this group is followed by a hyphen
C*	    and a second cloud top height.
C
	    IF ( ( ifptr + 2 ) .le. ietlyr ) THEN
		IF ( ( fields ( ifptr + 1 ) ( 1:1 ) .eq. '-' )
     +		  .and. ( itypsf ( ifptr + 2 ) .eq. NMR ) ) THEN
		    CALL AF_HHFM ( fields ( ifptr + 2 )
     +			        ( :lensf (ifptr) ), hoct2, ier )
		    IF ( .not. ERMISS ( hoct2 ) ) hoct = hoct2
		END IF
	    END IF
	END IF
C*
	RETURN
	END
