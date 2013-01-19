	CHARACTER*(*)  FUNCTION PT_CMCL  ( comx )
C************************************************************************
C* PT_CMCL								*
C*									*
C* This character function returns the character value for the		*
C* combined cloud height and cloud coverage: 				*
C*									*
C*                    CLDx = PT_CMCL ( COMx )				*
C*									*
C* The input value COMx may be computed from the cloud height and 	*
C* coverage using the function PR_COMX.  The output height is given 	*
C* in hundreds of feet; the cloud cover code is the short code:		*
C*									*
C*            0 --->  _ (underscore)					*
C*            1 --->  C							*
C*            2 --->  S							*
C*            3 --->  B							*
C*            4 --->  O							*
C*            5 --->  X							*
C*            6 --->  -S						*
C*            7 --->  -B						*
C*            8 --->  -O						*
C*            9 --->  -X						*
C*									*
C*     Example:  COMX    = 1507.					*
C*               PT_CMCL = 150-B					*
C*									*
C* The characters are left justified in the string.			*
C*									*
C* PT_CMCL  ( COMX )							*
C*									*
C* Input parameters:							*
C*	COMX		REAL	 	Combined height & coverage	*
C*									*
C* Output parameters:							*
C*	PT_CMCL		CHAR*		Character height & coverage	*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* M. desJardins/GSFC	10/87	Rewritten				*
C* M. desJardins/GSFC	10/89	Add partially obscurred with no height  *
C* S. Schotz/GSC 	10/89	Check for clear case			*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* M. desJardins/GSFC	 7/90	Documentation				*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	buff*8, ppp*8
	LOGICAL		parobs
C*
	CHARACTER	cld ( 0: 9 )*8
        INCLUDE       'ERMISS.FNC'
	DATA 		cld / '_', 'C', 'S', 'B', 'O', 'X', '-S', '-B', 
     +			      '-O', '-X'/
C*
C------------------------------------------------------------------------
	PT_CMCL = ' '
C
C*	Check for missing data.
C
	IF  ( ( ERMISS ( comx ) ) .or. ( comx .lt. 0. ) )  THEN
	    RETURN
	END IF
C
C*      Check for clear case
C
	IF  ( comx .eq. 1 )  THEN
	   PT_CMCL = cld (1)
           RETURN
	END IF
C
C*	Extract cloud height and coverage, check for partially obscurred
C
	IF  ( comx .ge. 10000.)  THEN
	    parobs = .true.
	    icomb  = comx - 10000
	  ELSE
	    parobs = .false.
            icomb  = comx
	END IF
	ihgt  = icomb / 10
	icld  = icomb - ihgt * 10
C
C*	Take case of special case, where there is no height and cloud
C*	cover is obscured ( = 5 ) or partially obscurred ( = 9).
C
	IF  ( ( ihgt .eq. 0 ) .and. ( ( icld .eq. 5 ) .or. 
     +        ( icld .eq. 9 ) ) )  THEN
	    PT_CMCL = cld (icld)
	  ELSE
C
C*	    Encode height in buffer.
C
	    CALL ST_INCH  ( ihgt, buff, ier )
	    CALL ST_LSTR  ( buff, lenh, ier )
C
C*	    Add cloud code to buffer.
C
	    PT_CMCL = buff ( 1: lenh ) // cld ( icld )
	    IF  ( parobs )  THEN
		ppp = PT_CMCL
		PT_CMCL = '-X' // ppp
	    END IF
	END IF
C*
	RETURN
	END 
