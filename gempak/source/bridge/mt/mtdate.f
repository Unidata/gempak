	SUBROUTINE MT_DATE ( report, irpnt, jda, jhr, jmin, idecd,
     +			     iret )
C************************************************************************
C* MT_DATE                                                              *
C*                                                                      *
C* This subroutine will decode a METAR date field of the forms hhmmZ 	*
C* or ddhhmmZ.								*
C* 								        *
C* MT_DATE ( REPORT, IRPNT, JDA, JHR, JMIN, IDECD, IRET )		*
C*								        *
C* Input parameters: 						        *
C*      REPORT		CHAR*		METAR report                    *
C*								        *
C* Input and output parameters:					        *
C*	IRPNT		INTEGER		Pointer in report	        *
C*								        *
C* Output parameters:						        *
C*	JDA		INTEGER		Report date		        *
C*	JHR		INTEGER		Report hour		        *
C*	JMIN		INTEGER		Report minute 		        *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  2 = time field OK	        *
C*					 -2 = time field needs changes	*
C*					  0 = not a time field      	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = field decoded ok        	*
C*	                                 17 = problem in time field	*
C*                                       -1 = no time field found	*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	10/95	Brief description of change	        *
C* D. Kidwell/NCEP	 3/96	Added check on CHCR as end of field     *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 1/97	Added a call to DC_WLOG			*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* K. Tyle/GSC		 2/97	Ignore wind group ending in 'Z'		*
C* D. Kidwell/NCEP	 6/97	Removed call to ST_LSTR                 *
C* D. Kidwell/NCEP	 4/98	Added check for embedded blank          *
C* D. Kidwell/NCEP	 9/02	MT_VALD -> BR_VALD, MT_DTTM -> BR_DTTM  *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	report
C*
	CHARACTER  	strdat*7
C------------------------------------------------------------------------
	iret = 0
	idecd = 0
	strdat = report ( irpnt:irpnt + 6 )
C
	jda  = IMISSD
	jhr  = IMISSD
	jmin = IMISSD
C
C*	Check for miscoded wind field where 'KZ' is used instead of 'KT'.
C
	CALL BR_VALD ( strdat, 4, 7, 'KZ', 1, iloc, ier )
	IF ( ier .eq. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Check for 'Z' in cols 5 to 7.
C
	CALL BR_VALD ( strdat, 5, 7, 'Z', 1, iloc, iret )
C
C*	Check for an embedded blank with a trailing Z - this can 
C*	happen if the time is hhmm only.
C
	IF ( iret .eq. 0 ) THEN
	    IF ( INDEX ( strdat, CHSPAC ) .eq. 5 ) iret = -1
	END IF
C
	IF ( iret .eq. 0 ) THEN
C
C*	    Z was found in proper column.
C
	    CALL BR_DTTM ( strdat, iloc, jda, jhr, jmin, kret )
	    IF ( kret .eq. 0 ) THEN
		idecd = 2
	      ELSE
		idecd = -2
		iret = kret
	    END IF
	    irpnt = irpnt + iloc
	  ELSE IF ( iret .lt. 0 ) THEN 
C
C*	    Z was not found - check to see if we really have a 
C*	    date / time field with 'Z' left off.
C
	    iend1 = INDEX ( strdat, CHSPAC )
	    iend2 = INDEX ( strdat, CHCR )
	    iend = MAX ( iend1, iend2 )
	    IF ( ( iend .eq. 5 ) .or. ( iend .eq. 7 ) ) THEN
		lens = iend - 1
	        strdat = strdat ( :lens )
		CALL ST_INTG ( strdat ( 1:lens ), idum, nret )
		IF ( ( nret .eq. 0 ) .and. ( idum .ne. 9999 ) ) THEN
C
C*		    Append a 'Z' to the date/time field.
C
		    strdat = strdat ( 1:lens ) // 'Z'
		    CALL BR_DTTM ( strdat, iend, jda, jhr, jmin, kret )
		    iret = kret
		    idecd = -2
		    irpnt = irpnt + lens
		END IF
	    END IF
	  ELSE
C
C*	    Z was found in wrong position.
C
	    iret = 17
 	END IF
C
C*	Write out a message to the decoder log if there is an error.
C
	IF ( iret .gt. 0 ) THEN
     	    CALL DC_WLOG ( 4, 'MT', iret, strdat, ier )
	END IF
C*
	RETURN
	END
