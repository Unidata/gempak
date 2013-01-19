	SUBROUTINE G2T_INIT ( iret )
C************************************************************************
C* G2T_INIT 								*
C*									*
C* This subroutine initializes G2T common block before each new zone.	*
C*									*
C* G2T_INIT ( IRET )							*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07						*
C* T. Lee/SAIC		11/07	Added NT to OFFTXT for combining period	*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Initialize time and bound # for exception.
C
	DO nt = 1, MXTIME
	    grdtm ( nt ) = ' '
	    offtxt ( nt ) = ' '
	    DO kt = 1, 2
	        nbnd  ( kt, nt ) = 0
		fsuba ( kt, nt ) = .false.
		subid ( kt, nt ) = ' '
	    END DO
	END DO
C
C*	Initialize wave/wind information.
C
	DO nt = 1, MXTIME
	    DO nb = 1, MXSZON
		DO np = 1, MXTYPE
		    mnval   ( np, nb, nt ) = IMISSD
		    mxval   ( np, nb, nt ) = IMISSD
		    mnval_s ( np, nb, nt ) = IMISSD
		    mxval_s ( np, nb, nt ) = IMISSD
		END DO
C
		DO nw = 1, MXWHGT
		    wavhst ( nb, nt, nw ) = IMISSD
		    icwave ( nb, nt, nw ) = nw
		    wavpct ( nb, nt, nw ) = 0.
		END DO
C
		DO ns = 1, MXSPED
		    winhst ( nb, nt, ns ) = IMISSD
		    icwind ( nb, nt, ns ) = ns
		    winpct ( nb, nt, ns ) = 0.
		END DO
	    END DO
	END DO
C
C*	Initialize warning flags. 
C
	DO nt = 1, MXTIME
	    fhurr    ( nt ) = .false.
	    fgale    ( nt ) = .false.
	    fstorm   ( nt ) = .false.
	END DO
C
C*	Initialize display buffer.
C
	DO ii = 1, MXTYPE
	    DO jj = 1, MXTRND
		eflag_d ( ii, jj ) = .false.
		id_d ( ii, jj ) = ' '
		DO kk = 1, MAXMIN
		    mxn_d  ( ii, jj, kk ) = IMISSD
		    mxn_de ( ii, jj, kk ) = IMISSD
		END DO
	    END DO
	END DO
C*
	DO ii = 1, NOFDIR
	    DO ij = 1, MXTRND
		wdir_d  ( ii, ij ) = ' '
		wdir_de ( ii, ij ) = ' '
		kdir_d  ( ii, ij ) = IMISSD
		kdir_de ( ii, ij ) = IMISSD
	    END DO
C
	    DO jj = 1, MXSZON
		DO kk = 1, MXTIME
		    wdrmod ( ii, jj, kk ) = ' '	    
		    kdrmod ( ii, jj, kk ) = IMISSD
		END DO
	    END DO
	END DO
C*
	RETURN
	END
