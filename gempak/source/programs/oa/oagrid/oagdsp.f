	SUBROUTINE OAGDSP  ( gltln, arenam, grltln, deltan, deltax,
     +			     deltay, iextnd, kx, ky, eltln, dltln, 
     +			     iret )
C************************************************************************
C* OAGDSP								*
C*									*
C* OAGDSP  ( GLTLN, ARENAM, GRLTLN, DELTAN, DELTAX, DELTAY, IEXTND,	*
C*           KX, KY, ELTLN, DLTLN, IRET )				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	8/85						*
C* I. Graffman/RDS	3/86	 Added respond				*
C* S. Schotz/GSC	5/90	 Get respnd flag locally from IP_RESP	*
C************************************************************************
	INCLUDE		'oagcmn.cmn'
C*
	REAL		gltln (4), grltln (4), eltln (4), dltln (4)
	CHARACTER*(*)	arenam
	INTEGER		iextnd (4)
	LOGICAL 	respnd
C------------------------------------------------------------------------
	iret = 0
C
C*	Write information to users terminal.
C
	WRITE ( 6, 1000 ) grltln, arenam, gltln (3), gltln (4)
1000	FORMAT (/ 20X, 'OBJECTIVE ANALYSIS PARAMETERS' //
     +           1X,  'GRIDAREA',6X, 4F10.2, 2X, '(', A, F6.2,F8.2,')' )
C*
	WRITE ( 6, 1001 ) deltan, deltax, deltay
1001	FORMAT ( 1X, 'DELTAN', 12X, F7.3 / 1X, 'DELTAX', 12X, F7.3 /
     +           1X, 'DELTAY', 12X, F7.3 / )
C*
	WRITE ( 6, 1002 ) kx, iextnd(1), iextnd(2), ky, iextnd(3),
     +                    iextnd(4)
1002	FORMAT ( 1X, 'GRID POINTS', 5X, 'X : ', I3, '  (', I2, ',', 
     +           I2, ' )',
     +           8X, 'Y : ',I3, '  (', I2, ',', I2, ' )' )
C*
	WRITE ( 6, 1003 ) ELTLN, DLTLN
1003	FORMAT ( 1X, 'EXTENDAREA', 6X, 4F10.2 / 
     +           1X, 'DATAAREA', 8X, 4F10.2 / )
C
C*	Check to see if computations have been done.
C
	IF  ( cmpflg ) THEN
C*
	    WRITE ( 6, 1010 )
1010	    FORMAT ( / ' CALCULATIONS: ' )
	    WRITE ( 6, 1011 ) FILE (1:62), SRC, AREA, NGSTN
1011	    FORMAT ( 3X, 'FILE', 6X, A / 1X, 'SOURCE', 4X, A, 4F10.2,
     +               7X, 'NSTN ', I4 )
	    WRITE ( 6, 1012 ) PARM, RLEVEL, DATE, COMPDS, UNIFDS,
     +                        CDELTN,
     +				CDELTX, CDELTY
1012	    FORMAT ( 3X, 'PARM', 6X, A, 8X, 'LEVEL ', F6.1,
     +               8X, 'DATTIM ',
     +               6X, A / /
     +               5X, 'COMPUTED DATA SPACING',10X, F6.3 /
     +               5X, 'UNIFORM  DATA SPACING',10X, F6.3 /
     +               5X, 'RECOMMENDED   DELTAN = ',F6.3,
     +               3X, 'DELTAX = ',
     +                   F6.3, 3X, 'DELTAY = ', F6.3 / )
	END IF
C
C*	Prompt user.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
	RETURN
	END
                    
