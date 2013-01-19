	SUBROUTINE SNLWPM  ( prmlst, nparms, stnprm, nstnp, luns, nlun, 
     +			     iret )
C************************************************************************
C* SNLWPM								*
C*									*
C* This subroutine writes the list of level parameters to the output	*
C* devices.								*
C*									*
C* SNLWPM  ( PRMLST, NPARMS, STNPRM, NSTNP, LUNS, NLUN, IRET )		*
C*									*
C* Input parameters:							*
C*	PRMLST (NPARMS)	CHAR*		Level parameters		*
C*	NPARMS		INTEGER		Number of level parameters	*
C*	STNPRM (NSTNP)	CHAR*		Station parameters		*
C*	NSTNP		INTEGER		Number of station parameters	*
C*	LUNS   (NLUN)	INTEGER		Output LUNS			*
C*	NLUN		INTEGER		Number of output units		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88	GEMPAK 4.1				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	prmlst (*), stnprm (*)
	INTEGER		luns (*)
C*
	CHARACTER	header*80
C------------------------------------------------------------------------
	iret = 0
C
C*	Write blank line.
C
	DO  i = 1, nlun
	    WRITE  ( luns (i), 1000 )
1000	    FORMAT ( 1X )
	END DO
C
C*	Get level parameters in list.
C
	IF  ( nparms .gt. 0 )  THEN
	    header = ' SNPARM = '
	    ip     = 11
	    DO  i = 1, nparms
		IF  ( ip .gt. 75 )  THEN
		    DO  j = 1, nlun
			WRITE  ( luns (j), 2000 )  header
		    END DO
		    ip = 11
		    header = ' '
		END IF
		header ( ip : ip+4 ) = prmlst (i) // ';'
		ip = ip + 5
	    END DO
	    header ( ip-1 : ) = ' '
C
C*	    Write to each lun.
C
	    DO  i = 1, nlun
		WRITE  ( luns (i), 2000 )  header
2000		FORMAT ( A )
	    END DO
	END IF
C
C*	Get station parameters in list.
C
	IF  ( nstnp .gt. 0 )  THEN
	    header = ' STNPRM = '
	    ip     = 11
	    DO  i = 1, nstnp
		IF  ( ip .gt. 75 )  THEN
		    DO  j = 1, nlun
			WRITE  ( luns (j), 2000 )  header
		    END DO
		    ip = 11
		    header = ' '
		END IF
		header ( ip : ip+4 ) = stnprm (i) // ';'
		ip = ip + 5
	    END DO
	    header ( ip-1 : ) = ' '
C
C*	    Write to each lun.
C
	    DO  i = 1, nlun
		WRITE  ( luns (i), 2000 )  header
	    END DO
	END IF
C*
	RETURN
	END
