	SUBROUTINE IMRAD2 ( tilt, radprm, ier)

	INCLUDE		'imcmn.cmn'

	CHARACTER	tilt*(*), radprm*(*)
	INTEGER		ier

	im_sweep_num = tilt(1:32)
	im_radar_field = radprm(1:32)

	ier = 0


	RETURN
	END



	SUBROUTINE IMRAD2G ( tilt, radprm, ier)

	CHARACTER       tilt*(*), radprm*(*)

	INTEGER         ier

	INCLUDE		'imcmn.cmn'

	tilt = im_sweep_num
	radprm = im_radar_field
	
	ier = 0

	RETURN
	END

	SUBROUTINE IMCSET ( cparm, cval, ier)
	CHARACTER	cparm*(*), cval*(*)

	INCLUDE		'IMGDEF.CMN'

	IF ( cparm .eq. 'cmstyp' ) THEN
	   cmstyp = cval
	ELSE IF ( cparm .eq. 'cmtype' ) THEN
	   cmtype = cval
	   write(*,*) 'setting cmtype to ',cval
	ELSE IF ( cparm .eq. 'cmbunt' ) THEN
	   cmbunt = cval
	ELSE
	   write(*,*) 'unknown common setting ',cval
	ENDIF

	ier = 0

	RETURN
	END

	SUBROUTINE IMDSET ( cparm, ival, ier)
	CHARACTER	cparm*(*)

	INCLUDE		'IMGDEF.CMN'

	IF ( cparm .eq. 'imdoff' ) THEN
	   imdoff = ival
	ELSE IF ( cparm .eq. 'imcalbar' ) THEN
	   imcalbar = ival
	ELSE IF ( cparm .eq. 'imndlv' ) THEN
	   imndlv = ival
	ELSE IF ( cparm .eq. 'imnchl' ) THEN
	   imnchl = ival
	ELSE IF ( cparm .eq. 'rmbelv' ) THEN
	   rmbelv = float(ival)
	ELSE
	   write(*,*) 'unknown common setting ',cparm
	ENDIF

	ier = 0

	RETURN
	END

