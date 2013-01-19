	SUBROUTINE MT_POST ( ifldno, idecd, iret )
C************************************************************************
C* MT_POST                                                              *
C*                                                                      *
C* This subroutine performs post-processing following an individual	*
C* field decode.  If the expected field was decoded, MT_NEXT advances	*
C* the MTCMN pointer IMHERE to the next field in the main body of the	*
C* report.  Parameters used which are not in the calling sequence are   *
C* found in common /postpr/.						*
C*									*
C* If a miscoded field is encountered, the MTCMN logical ERRFLG is	*
C* set to true.								*
C* 								        *
C* MT_POST ( IFLDNO, IDECD, IRET )					*
C*								        *
C* Input parameters:						        *
C*	IRTD		INTEGER	    Error value returned from decoder   *
C*	IFLDNO		INTEGER	    Sequential field number		*
C*	IDECD		INTEGER	    Decode flag			        *
C*				      2 = auto report with field	*
C*					    decoded having slashes	*
C*				      1 = field decoded		        *
C*				      0 = field not decoded		*
C*									*
C* Output parameters:	 					        *
C*	IMHERE		INTEGER	    Pointer for next decode rtn to use  *
C*	IQUIT		Integer     Re-entry point in DO loop           *
C*	ERRFLG		LOGICAL	    Master error flag for print         *
C*	IRET		INTEGER	    Return code                         *
C*				      0 = normal return		        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 6/95				                *
C* K. Tyle/GSC		 1/97	Reorganized header and comments; 	*
C*				changed nret to iret			*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP	 4/98   Revised prologue                        *
C* F. J. Yen/NCEP	 2/04	Updated prologue about idecd		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
C------------------------------------------------------------------------
	iret = 0
C
	imhere = ifldno
	IF ( idecd .ne. 0 ) THEN
	    iquit = ifldno
	    CALL MT_NEXT ( iret )
	END IF
C
C*	Set master error flag if we have a miscoded field.
C
	IF ( irtd .ge. 1 ) errflg = .true.
C*
	RETURN
	END
