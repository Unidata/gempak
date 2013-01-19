	SUBROUTINE MT_AUTO ( straut, idecd, iret )
C************************************************************************
C* MT_AUTO								*
C*                                                                      *
C* This subroutine checks a METAR report for 'AUTO' or 'COR'.		*
C* Values are stored in common if they are found.			*
C* 								        *
C* MT_AUTO ( STRAUT, IDECD, IRET )                                      *
C*								        *
C* Input parameters: 						        *
C*      STRAUT		CHAR*		Possible AUTO or 'COR' field    *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRAUTO)	REAL		Automatic station flag          *
C*	RIVALS(IRCORN)	REAL		Report correction indicator     *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  1 = field decoded		*
C*					  0 = field not decoded		*
C*	IRET		INTEGER		Return code		        *
C*	      		    		  0 = normal return	 	*
C*					  1 = miscoded field		*
C*	      		    		 -1 = fields not found		*
C*					                                *
C**								        *
C* Log:							                *
C* D. Kidwell/NCEP 	 4/95                                           *
C* D. Kidwell/NCEP 	 9/96	Key on station id for US, Canada        *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP 	 4/98	New interface                           *
C* D. Kidwell/NCEP 	 9/02	Replaced MT_VALD with BR_VALD           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	straut
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret  = 0
	idecd = 0
C
C*	Look for 'AUTO'.
C
	CALL BR_VALD ( straut, 1, 1, 'AUTO', 1, iloc, iret)
	IF ( iret .ge. 0 ) THEN
C	
C*	    'AUTO' was found.
C
	    rivals ( irauto ) = 0.
	    idecd = 1
	  ELSE
C
C*          Look for 'COR' or 'CCA' or 'CCB'.
C
            IF ( contry .eq. 'US  ' ) THEN
                CALL BR_VALD ( straut, 1, 1, 'COR', 1, iloc, jret )
              ELSE IF ( contry .eq. 'CN  ' ) THEN
                CALL BR_VALD ( straut, 1, 1, 'CC', 1, iloc, jret )
              ELSE
                jret = -1
            END IF
            IF ( jret .ge. 0 ) THEN
C
C*              'COR' was found.
C
		rivals ( ircorn ) = 1. 
                idecd = 1
                iret = jret
            END IF
        END IF
	IF ( ERMISS ( rivals ( ircorn ) ) )  rivals ( ircorn ) = 0.
	IF ( iret .gt. 0 ) iret = 1
C*
	RETURN
	END
