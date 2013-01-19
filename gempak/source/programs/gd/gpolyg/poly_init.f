	SUBROUTINE POLY_INIT ( iret )
C************************************************************************
C* POLY_INIT 								*
C*									*
C* This subroutine initializes polygon common block.			*
C*									*
C* POLY_INIT ( IRET )							*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		02/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Default warning categories.  Set WSCA to a high number because small
C*	craft advisory has not been implemented operationally.  Once in
C*	operation, its value is defined by poly_parm.tbl.  Current value
C*	will be overwritten.	
C
	wsca  = 9999.
	wgale = 32.5
	wstrm = 47.5
	whurr = 62.5
C
C*	Initialize wave/wind information.
C
	DO nc = 1, NCATEG
	    ngroup ( nc ) = 0
	    DO ng = 1, MAXGRP
		mpoint ( ng, nc ) = 0
		npoint ( ng, nc ) = 0
		numpts ( ng, nc ) = 0
		DO np = 1, MAXPTS
		    pgrlat ( np, ng, nc ) = RMISSD
		    pgrlon ( np, ng, nc ) = RMISSD
		    kgridu ( np, ng, nc ) = IMISSD
		    kgridv ( np, ng, nc ) = IMISSD
		    isnake ( np, ng, nc ) = IMISSD
		    jsnake ( np, ng, nc ) = IMISSD
		END DO
C
		DO kp = 1, 2
		    imargn ( kp, ng, nc ) = IMISSD
		    jmargn ( kp, ng, nc ) = IMISSD
		END DO
	    END DO
	END DO
C*
	RETURN
	END
