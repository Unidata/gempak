	SUBROUTINE DSFILL  ( szfil, iftyp, size, jtype, iret )
C************************************************************************
C* DSFILL								*
C* 									*
C* This subroutine sets the polygon fill attributes.  If these 		*
C* attributes are not positive, no changes are made.   			*
C* 									*
C* DSFILL  ( SZFIL, IFTYP, SIZE, JTYPE, IRET )				*
C*                                                                    	*
C* Input parameters:							*
C* 	SZFIL		REAL		Fill pattern size             	*
C* 					  <=0 = no change		*
C*      IFTYP		INTEGER		Fill pattern type             	*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Fill pattern size		*
C*	JTYPE		INTEGER		Fill pattern type		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 6/97						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	IF  ( szfil .gt. 0. )   tfilsz = szfil
	IF  ( iftyp .gt. 0  )   mfltyp = iftyp
	size  = tfilsz
        jtype = mfltyp
C
C*	Send the information to the driver.
C
	CALL HSFILL ( size, jtype, ier )
C*
	RETURN
	END
