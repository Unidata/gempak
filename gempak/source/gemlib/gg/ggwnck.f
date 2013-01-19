        SUBROUTINE GG_WNCK ( cnties, ncnty, wfocty, knt, create, iret )
C************************************************************************
C* GG_WNCK        							*
C*        								*
C* This subroutine checks if the county list from the most recent WCNs  *
C* match the county list from the latest WOU for a given watch number.  *
C*        								*
C* GG_WNCK ( CNTIES, NCNTY, WFOCTY, KNT, CREATE, IRET )			*
C*        								*
C* Input parameters:        						*
C*        CNTIES(NCNTY) CHAR*		Active county array from WCNs	*
C*	  NCNTY		INTEGER		Number of active counties	*
C*        WFOCTY(KNT) 	CHAR*		County array from latest WOU 	*
C*	  KNT 		INTEGER		Number of WOU counties		*
C*        								*
C* Output parameters:        						*
C*        CREATE	LOGICAL		Flag for creating the text msg. *
C*        IRET		INTEGER		Return code			*
C*					 -1 - Cancellation WOU exist    *
C*        								*
C**        								*
C* Log:        								*
C* A. Hardy/NCEP         7/03						*
C* A. Hardy/NCEP         3/05	Reworked comparision logic		*
C* F. J. Yen/NCEP	 3/07	Replaced dim. of equal to MAX_CNTY.	*
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)	cnties(*), wfocty(*)
	LOGICAL         create
C*
	LOGICAL         equal(MAX_CNTY)
C-----------------------------------------------------------------------
       iret   = 0
       create = .false.
C
       DO ii = 1, ncnty
           equal(ii) = .False.
       END DO
C
C*     Check for matching WOU counties to WCN counties.
C
       DO ii = 1, ncnty
           DO jj = 1, knt
               IF ( cnties(ii) .eq. wfocty(jj) ) THEN
                   equal (ii) = .True.
               END IF
           END DO 
       END DO 
 
C
C*     Check for new WCN county.
C
       DO ii = 1, ncnty
           IF ( equal (ii) .eqv. .False. ) THEN
               create = .True.
               RETURN
           END IF
       END DO
C
C*     Check if counties have been deleted
C
       IF ( knt .gt. ncnty ) THEN
           create = .True.
           RETURN
       END IF
C*        
       RETURN
       END
