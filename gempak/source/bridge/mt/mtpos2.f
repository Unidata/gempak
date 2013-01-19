	SUBROUTINE MT_POS2 ( ifldno, idecd, maxfld, idcfld, donfld, 
     +			     iret )
C************************************************************************
C* MT_POS2                                                              *
C*                                                                      *
C* This subroutine performs post-processing following a multiple	*
C* field decode (a group which can appear more than once in a METAR     *
C* report, such as sky condition or weather phenomena.)  If the expected*
C* field was decoded, MT_NEXT advances the MTCMN pointer IMHERE to the  *
C* next field in the main body of the report.  Parameters used which    *
C* are not in the calling sequence are found in common /postpr/.        *
C*									*
C* If a miscoded field is encountered, the MTCMN logical ERRFLG is	*
C* set to true.								*
C* 								        *
C* MT_POS2 ( IFLDNO, IDECD, MAXFLD, IDCFLD, DONFLD, IRET )		*
C*								        *
C* Input parameters:						        *
C*	IRTD		INTEGER	    Error value returned from decoder   *
C*	IFLDNO		INTEGER	    Sequential field number		*
C*	IDECD		INTEGER	    Decode flag			        *
C*				      2 = auto report with field	*
C*					    decoded having slashes	*
C*				      1 = field decoded		        *
C*				      0 = field not decoded		*
C*	MAXFLD		INTEGER	    Max number of fields		*
C*									*
C* Input and output parameters:						*
C*	IDCFLD		INTEGER	    Number of fields		        *
C*									*
C* Output parameters:	 				                *
C*	IQUIT		INTEGER	    Re-entry point in DO loop	        *
C*	ERRFLG		LOGICAL	    Master error flag for print         *
C*	DONFLD		LOGICAL     Decoder completion flag		*
C*	IRET		INTEGER	    Return code                         *
C*				      0 = normal return		        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 6/95				                *
C* K. Tyle/GSC		 1/97	Reorganized header and comments;	*
C*				changed nret to iret			*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP	 4/98   Revised prologue                        *
C* F. J. Yen/NCEP	 2/04	Updated prologue about idecd		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	LOGICAL donfld
C------------------------------------------------------------------------
	iret = 0
C
	IF ( idecd .ne. 0 ) THEN
	    idcfld = idcfld + 1
	    IF ( idcfld .le. maxfld ) THEN
	    	iquit  = ifldno
	      ELSE
		iquit  = ifldno + 1
		donfld = .true.
	    END IF
	    CALL MT_NEXT ( iret )
	  ELSE
	    donfld = .true.
	    idcfld = idcfld - 1
	END IF
	IF ( irtd .ge. 1 ) errflg = .true.
C*
	RETURN
	END
