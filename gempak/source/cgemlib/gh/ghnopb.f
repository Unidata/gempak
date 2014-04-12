	SUBROUTINE GH_NOPB ( advnm, stype, sname, iret)
C************************************************************************
C* GH_NOPB								*
C*									*
C* This subroutine plots the 'no wind forecast probability' graphics.   *
C*									*
C* GH_NOPB (ADVNM, STYPE, SNAME, IRET) 					*
C*									*
C* Input parameters:							*
C*      ADVNM		CHAR*		Advisory number of current storm*
C*	STYPE		CHAR*		Storm type			*
C*	SNAME		CHAR*		Storm name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Created					*
C* A. Hardy/GSC		 5/01	Modified text string to have name/adv # *
C* A. Hardy/GSC		 6/01   Cleaned up prolog			*
C* A. Hardy/NCEP	 9/02   Added Subtropical Depression		*
C* S. Jacobs/NCEP	 3/13	Added Post-Tropical Cyclone		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	advnm, stype, sname
C
        CHARACTER       prbstr(3)*80, noprob*158 , type*24
        REAL            xlin(5), ylin(5)
C*
C------------------------------------------------------------------------
        iret = 0
C
	CALL GSCOLR  ( 1, ier )
	CALL GSLINE  ( 1, 2, 3, 2, ier )
        CALL GSSMTH ( 0, 1., ier )
        CALL GSTEXT ( 22, 2, 1.0, 1, 111, 1, 2, ier )
C
C*	Set the the border and text information.
C
        xlin(1) = .15
        ylin(1) = .12
        xlin(2) = .15
        ylin(2) = .63
        xlin(3) = .81
        ylin(3) = .63
        xlin(4) = .81
        ylin(4) = .12
        xlin(5) = .15
        ylin(5) = .12
        CALL GLINE ('N', 5, xlin, ylin, ier )
C
        CALL ST_LSTR ( stype, lent, ier )
	IF ( stype .eq. 'TS' ) THEN
            type = 'TROPICAL STORM'
	  ELSE IF ( stype .eq. 'HU' ) THEN
            type = 'HURRICANE'
	  ELSE IF ( stype .eq. 'TD' ) THEN
            type = 'TROPICAL DEPRESSION'
	  ELSE IF ( stype .eq. 'SD' ) THEN
            type = 'SUBTROPICAL DEPRESSION'
	  ELSE IF ( stype .eq. 'SS' ) THEN
            type = 'SUBTROPICAL STORM'
	  ELSE IF ( stype .eq. 'PT' ) THEN
            type = 'POST-TROPICAL CYCLONE'
        END IF
C
        ixoff = 0
        iyoff = 0
        CALL ST_LSTR ( advnm, lena, ier)
        CALL ST_LSTR ( type, lent, ier )
        CALL ST_LSTR ( sname, lenn, ier )
        prbstr (1) = 'NO WIND SPEED PROBABLILITIES'
	prbstr (2) = 'FOR ' // type(:lent) // ' ' // sname(:lenn)
	prbstr (3) = 'FOR ADVISORY' 
        CALL ST_LSTR ( prbstr(1), leno, ier )
        CALL ST_LSTR ( prbstr(2), lent, ier )
        CALL ST_LSTR ( prbstr(3), lenh, ier )
        noprob = prbstr(1) (:leno) // CHCR // CHCR // 
     +               prbstr(2) (:lent)// CHCR // CHCR //
     +               prbstr(3) (:lenh) // ' ' //advnm(:lena)
        CALL ST_LSTR ( noprob, lens, ier )
        CALL GTEXT  ( 'N',.47, .40 ,
     +                    noprob(:lens), 0.0, ixoff, iyoff, ier)
C*
        RETURN
	END
