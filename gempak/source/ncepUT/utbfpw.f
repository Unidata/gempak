	SUBROUTINE UT_BFPW ( pwgrp, iauto, iarr, inumd, ier )
C************************************************************************
C* UT_BFPW 								*
C*									*
C* This subroutine converts present weather groups from METAR  		*
C* reports into BUFR descriptors.      					*
C*									*
C*              CALL UT_BFPW ( pwgrp, iauto, iarr, inumd, ier )		*
C*									*
C* UT_BFPW calls one subroutine to do the table lookup for manned	*
C* METAR reports and another for the unmanned METARs.			*
C*									*
C* UT_BFPW  ( pwgrp, iauto, iarr, inumd, ier ) 				*
C*									*
C* Input parameters:							*
C*	pwgrp		CHAR		Present Weather groups		*
C*	iauto		INTEGER		Automatic/manned switch		*
C*									*
C* Output parameters:							*
C*	iarr   		INTEGER		BUFR descriptors         	*
C*	inumd		INTEGER		Index of last descriptor        *
C*	                                       returned for each group  *
C*	ier		INTEGER		Return code                     *
C**									*
C* Log:									*
C* L. Sager/NCEP	 3/96	                                     	*
C* D. Kidwell/NCEP	 10/96	Cleaned up code; moved to UT_		*
C* D. Kidwell/NCEP	 11/96	Improved error handling			*
C* J. Ator/NP12		 12/97	Changed verbosity level in DC_WLOG call	*
C************************************************************************
	CHARACTER*9	pwgrp (3)
	CHARACTER*9 	pwx, buff
	CHARACTER * 100 logmsg
	INTEGER		iarr (12)
	INTEGER		inumd (3)
C
C*      Begin by initializing the output array
C
        DO  k = 1,12
            iarr (k) = 0
	    IF ( k .lt. 4 ) inumd ( k ) = 0
        END DO
        inum = 0
        ier = 0
	igps = 0
C
C*      Check the manned/auto flag
C
        IF (( iauto .lt. 1 ) .or. ( iauto .gt. 2 )) THEN
            ier = 2
            RETURN
        END IF
C
C*      Begin to process the three present weather groups
C
	DO  i = 1, 3
C 
C*	    Start by removing leading blanks from the string
C
	    CALL ST_LDSP ( pwgrp (i), pwx, leng, iret )
C
C*	    If no present weather to decode, return.
C
	    IF ( leng .eq. 0 ) THEN 
	        IF ( i .eq. 1 ) THEN 
		    ier = 1
	            RETURN
		END IF
	    ELSE
C 
C*	        Call the subroutines to make the descriptors.
C
	 	igps = igps + 1
	        k = 1
	        DO WHILE ( k .lt. leng )
		    buff = pwx (k:leng)
	            IF ( iauto .eq. 1 ) THEN
	                CALL UT_BFPM ( buff, k, iarr, inum, iret )
	            ELSE
	                CALL UT_BFPA ( buff, k, iarr, inum, iret )
	            END IF
	        END DO
	        inumd ( i ) = inum
	    END IF
	END DO
C
C*	Check for unconverted groups
C
	IF ( inumd ( 1 ) .eq. 0 ) ier = 1
	DO i = 2, igps
	    IF ( inumd ( i ) .eq. inumd ( i - 1 ) ) ier = ier + 1
	END DO
	IF ( ier .gt. 0 ) THEN
	    logmsg = 'Weather group(s) not converted to BUFR in string:'
     +		// ' ' // pwgrp(1) // ' ' // pwgrp(2) // ' ' // pwgrp(3)
	    CALL DC_WLOG ( 2, ' ', 0, logmsg, ierlog )
	END IF
	IF ( ier .eq. igps ) ier = 3
	IF ( ier .lt. 3 ) ier = 0
C
	RETURN
	END
