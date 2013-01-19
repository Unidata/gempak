	SUBROUTINE SFCOPN(fname,iflno,isorc,iperr)

	INCLUDE         'GEMPRM.PRM'


	CHARACTER*(*)	fname
	INTEGER		iflno
	INTEGER		isorc,nparm
	CHARACTER	parms(MMPARM)*4
	CHARACTER	filnam*256


	iflno = 0

	CALL FL_MFIL ( fname, ' ', filnam, iret )
	CALL SF_OPNR ( filnam, iflno, isorc, 
     +                 nparm, parms, iperr)

	RETURN
	END
