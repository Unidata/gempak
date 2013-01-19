	SUBROUTINE BR_VALD ( str, ilocb, iloce, fldkey, ipt, iloc, iret)
C************************************************************************
C* BR_VALD                                                              *
C*                                                                      *
C* This subroutine checks for a valid group by searching for a key	*
C* substring denoted by FLDKEY.  It will first check within a 		*
C* specified range within the string; if unsuccessful, it will then	*
C* check through the entire string.					*
C* 								        *
C* BR_VALD ( STR, ILOCB, ILOCE, FLDKEY, IPT, ILOC, IRET )	        *
C*								        *
C* Input parameters: 						        *
C*      STR   		CHAR*		String to be checked		*
C*	ILOCB		INTEGER		First col of str to be checked  *
C*	ILOCE		INTEGER		Last col of str to be checked   *
C*	FLDKEY		CHAR*		Substring to look for           *
C*	IPT		INTEGER		Print flag 			*
C*					  1 = print error message	*
C*								        *
C* Output parameters:						        *
C*	ILOC		INTEGER		Col in str where fldkey begins  *
C*					  0 = field not found	        *
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = fldkey found normally	*
C*					  1 = fldkey found elsewhere 	*
C*	                                 -1 = fldkey not found      	*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP      10/95                                           *
C* K. Tyle/GSC           1/97   Reorganized header and comments;        *
C*                              changed call to DC_WLOG                 *
C* K. Tyle/GSC           2/97   Changed error processing                *
C* D. Kidwell/NCEP       6/97   Removed call to ST_LSTR                 *
C* D. Kidwell/NCEP       4/98   Cleaned up prologue                     *
C* D. Kidwell/NCEP 	 9/02	Renamed from MT_VALD; removed mtcmn.cmn *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	str, fldkey
C------------------------------------------------------------------------
	iloc = INDEX ( str, fldkey )
C
	IF ( iloc .gt. 0 ) THEN
C
C*	    FLDKEY was found in string.
C
	    IF ( ( iloc .ge. ilocb ) .and. ( iloc .le. iloce ) ) THEN
	        iret = 0
	      ELSE 
	        iret = 1
                IF ( ipt .eq. 1 )
     +	             CALL DC_WLOG ( 4, 'BR', 1, fldkey, ierlog )
	    END IF
	  ELSE
	    iret = -1
	END IF
C*
	RETURN
	END
