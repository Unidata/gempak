	SUBROUTINE	SETCPARM(CPARM,NPARM,JPARMS)


	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	cparm(*)
	INTEGER		nparm, jparms(*)

	CHARACTER       cprms (MMPARM)*4
	PARAMETER       ( NUMPRM = 8 )
	PARAMETER       ( NUMEXT = MMPARM - NUMPRM )
	DATA            cprms / 'TSEC', 'MSEC', 'SGNL', 'MULT',
     +                          'SMAJ', 'ECNT', 'ANGL', 'CHI2',
     +                          NUMEXT * ' ' /

	do i=1,numprm
	   jparms(i) = -1
	   CALL ST_FIND(cprms(i),cparm,nparm,jparms(i),iret)
	end do


	RETURN
	END
