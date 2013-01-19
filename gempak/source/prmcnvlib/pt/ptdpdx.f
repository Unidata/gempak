	CHARACTER*(*) FUNCTION PT_DPDX  ( dpdc )
C************************************************************************
C* PT_DPDX								*
C*									*
C* This function takes a dewpoint depression  and converts into a	*
C* into a 3-character string.  If the dewpoint depression is greater	*
C* or equal to 30C, the depression is displayed as an "X".      	*
C*									*
C* PT_DPDX  ( DPDC )    						*
C*									*
C* Input parameters:							*
C*      DPDC		REAL		Dewpoint depression		*
C*									*
C* Output parameters:							*
C*	PT_DPDX		CHAR*		Three-character depression	* 
C**									*
C* Log:									*
C* R. Jones/NCEP	09/06		Original version		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	cvalue*8
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C
	PT_DPDX = ' '
C
C*	Check for missing data.
C
	IF  ( ERMISS ( dpdc ) )  RETURN
C
C*	Convert to character string, get length.
C
	number = NINT ( dpdc )
	CALL ST_INLN  ( number, cvalue, lenc, ier )
	IF  ( ier .ne. 0 )  RETURN
C
C*	Pad with zeroes, if necessary
C
	IF  ( lenc .eq. 2 )  THEN
	    PT_DPDX = ' ' // cvalue
            IF ( number .ge. 30 ) PT_DPDX = ' X ' 
	  ELSE IF  ( lenc .eq. 1 )  THEN
	    PT_DPDX = ' ' // cvalue // ' '
	END IF
C*
	RETURN
	END
