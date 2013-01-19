	SUBROUTINE MT_SKY6 ( strsky, strht, isky, idecd, twofld, iret )
C************************************************************************
C* MT_SKY6                                                              *
C*                                                                      *
C* This subroutine will decode up to six sky condition groups, and	*
C* the cloud height/vertical visibility.  These values are stored in    *
C* common.								*
C* 								        *
C* MT_SKY6 ( STRSKY, STRHT, ISKY, IDECD, TWOFLD, IRET )			*
C*								        *
C* Input parameters: 						        *
C*      STRSKY		CHAR*		Sky condition field		*
C*      STRHT		CHAR*		Cloud height field		*
C*								        *
C* Input and output parameters: 				        *
C*	ISKY		INTEGER		Relative number of sky group    *
C*								        *
C* Output parameters:						        *
C*      RIVALS(IRVRTV)	REAL		Vertical visibility (feet)      *
C*      RIVALS(IRCMTN)	REAL		GEMPAK sky cover code           *
C*      RIVALS(IRNSKY)	REAL		Number of sky condition groups  *
C*      RIVALS(IRCTYL)	REAL		Low-level cloud type WMO 0513   *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  2 = auto report with field	*
C*						 decoded having slashes	*
C*					  1 = field decoded		*
C*					  0 = field not decoded		*
C*	TWOFLD		LOGICAL		Two field flag			*
C*	IRET		INTEGER		Return code                     *
C*	      		    		   0 = normal return		*
C*	                                  11 = invalid vert. visibility	*
C*					  -1 = no field found		*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	 4/95                                           *
C* D. Kidwell/NCEP 	 5/96   Modified to use Gempak function PR_CMTN *
C* D. Kidwell/NCEP 	11/96   Modified to use local function MT_CMTN  *
C* K. Tyle/GSC		 1/97	Change call to DC_WLOG; reorganize	*
C*				header and comments			*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP 	 5/97   Removed ERMISS reference to integer arg *
C* D. Kidwell/NCEP 	 6/97   Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP 	 4/98   New interface; increased length of str  *
C* D. Kidwell/NCEP 	 9/02   Moved most of code to BR_SKY6           *
C* F. J. Yen/NCEP	 2/04	Updated call to BR_SKY6 due to CSC.	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	strsky, strht
	LOGICAL 	twofld
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
C
	autof = rivals ( irauto )
	CALL BR_SKY6 ( strsky, strht, autof, isky, vrtv, cmtn, nsky,
     +		       ctyl, idecd, twofld, iret )
C
	IF ( .not. ERMISS ( vrtv ) ) 
     +		   rivals ( irvrtv ) = vrtv
	IF ( isky .lt. 7 ) THEN
	    rivals ( ircmtn ( isky ) ) = cmtn
	  ELSE IF ( isky .eq. 7 ) THEN
	    rivals ( ircmtn ( 1 ) ) = cmtn
	END IF
	IF ( nsky .ne. IMISSD )
     +	           rivals ( irnsky ) = FLOAT ( nsky )
	IF ( ERMISS ( rivals ( irctyl ) ) )
     +		   rivals ( irctyl ) = ctyl
C*
	RETURN
	END
