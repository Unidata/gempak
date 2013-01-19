	SUBROUTINE GH_WWRM ( ityp, istcon, icon, ibkcon, istdif, idif,
     +			     ibkdif, iret )
C************************************************************************
C* GH_WWRM                                                              *
C*									*
C* This subroutine eliminates new and cancelled breakpoint segments     *
C* which are already included in continued segments for a given watch/  *
C* warning type.                                                        *
C*                                                                      *
C* GH_WWRM ( ITYP, ISTCON, ICON, IBKCON, ISTDIF, IDIF, IBKDIF, IRET )   *
C*                                                                      *
C* Input parameters:                                                    *
C*	ITYP		INTEGER		Watch/warning type code         *
C*	ISTCON		INTEGER		Index of first 'CON' value      *
C*	ICON		INTEGER		Number of 'CON' bkpt values     *
C*	IBKCON(4,*)	INTEGER		Breakpoint pairs for type 'CON' *
C*	ISTDIF		INTEGER		Index of 1st 'NEW' or 'CAN' val *
C*                                                                      *
C* Input and output parameters:						*
C*	IDIF		INTEGER		No. of 'NEW' or 'CAN' bkpt vals *
C*	IBKDIF(4,*)	INTEGER 	Bkpt pairs for 'NEW' or 'CAN'   *
C*									*
C* Output parameters:                                                   *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C************************************************************************
	INTEGER		ibkcon (4,*), ibkdif (4,*)
C*
	INTEGER		kdif (4)
C------------------------------------------------------------------------
	iret = 0
C 
	DO jj = istcon, icon, 2
	    kk = istdif
	    DO WHILE ( kk .le. idif )
		iadd = 0
		IF ( ( ibkcon (ityp,jj+1) .le. ibkdif (ityp,kk) ) .or.
     +		     ( ibkdif ( ityp,kk+1 ) .le. ibkcon ( ityp,jj ) ) )
     +		     THEN
		  ELSE IF ( ibkcon ( ityp,jj ) .le. ibkdif ( ityp,kk ) )
     +		     THEN
		    IF ( ibkdif ( ityp,kk+1 ) .le. ibkcon ( ityp,jj+1 )) 
     +			 THEN
			iadd = -2
		      ELSE
			iadd = 2
			kdif ( 1 ) = ibkcon ( ityp, jj+1 )
			kdif ( 2 ) = ibkdif ( ityp, kk+1 )
		    END IF
		  ELSE IF ( ibkdif ( ityp,kk ) .lt. ibkcon ( ityp,jj) )
     +		     THEN
		    kdif ( 1 ) = ibkdif ( ityp, kk ) 
		    kdif ( 2 ) = ibkcon ( ityp, jj ) 
		    IF ( ibkdif ( ityp,kk+1 ) .le. ibkcon ( ityp,jj+1 ))
     +			 THEN
			iadd = 2
		      ELSE
			iadd = 4
			kdif ( 3 ) = ibkcon ( ityp, jj+1 )
			kdif ( 4 ) = ibkdif ( ityp, kk+1 )
		    END IF
		END IF
C
		IF ( iadd .eq. 0 ) THEN
		    kk = kk + 2
		  ELSE IF ( iadd .lt. 0 ) THEN
		    idif = idif + iadd
		    DO ll = kk, idif
			ibkdif ( ityp, ll ) = ibkdif ( ityp, ll+2 )
		    END DO
		  ELSE
		    kkp = kk + iadd
		    IF ( iadd .eq. 4 ) THEN
			idif = idif + 2
			DO ll = idif, kkp, -1
			    ibkdif ( ityp, ll ) = ibkdif ( ityp, ll-2 )
			END DO
		    END IF
		    DO ll = 1, iadd
			ibkdif ( ityp, kk+ll-1 ) = kdif ( ll )
		    END DO
		    kk = kkp
		END IF
	    END DO
	END DO
C*
	RETURN
	END
