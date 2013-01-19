	SUBROUTINE GG_INFO ( iwtch, inum, type, tbeg, tstp, inpt, 
     +                        xlat, xlon, ntest, iret )
C************************************************************************
C* GG_INFO								*
C*									*
C* This subroutine takes a specific watch number and returns information*
C* relating to that watch.						*
C*									*
C* GG_INFO ( IWTCH, INUM, TYPE, TBEG, TSTP, INPT, XLAT, XLON, NTEST,    *
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	IWTCH 		INTEGER		Watch number 			*
C*	INUM		INTEGER		Index number watch 		*
C*									*
C* Output parameters:							*
C*	TYPE		CHAR*		Watch type			*
C*	TBEG		CHAR*		Watch starting time		*
C*	TSTP		CHAR*		Watch ending time		*
C*	INPT		INTEGER		Number of lat/lon pairs		*
C*      XLAT (INPT)	REAL		Array of lats.			*
C*      XLON (INPT)	REAL		Array of lats.			*
C*	NTEST		INTEGER		Test flag			*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 6/01	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ggcmn.cmn'
C*
	CHARACTER*(*)	tbeg, tstp, type
        REAL            xlat(*), xlon(*)
C*
	CHARACTER	temp4*20
	INTEGER		istrt(5)
C-----------------------------------------------------------------------
	iret = 0
C
        CALL TI_CTOI ( timstr(inum), istrt, ier )
 	CALL TI_ADDM ( istrt, 60, istrt, ier )
 	CALL TI_ITOC ( istrt, temp4, ier )
 	CALL TI_DTM4 ( temp4, tbeg, ier )

        tstp = timstp(inum)
        type  = wtype(inum)
        inpt  =  npt(inum)
        ntest =  itest(inum)
C
        DO jj = 1, npt(inum)
            xlat(jj) = rlat(jj,inum)
            xlon(jj) = rlon(jj,inum)
        END DO
C*
	RETURN
	END
