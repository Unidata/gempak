	SUBROUTINE nldninit()

	include 'GEMPRM.PRM'
	include 'nldn.prm'

	ipos = 0
        first = .true.

	RETURN
	END


	SUBROUTINE nldnflush(iflno,iret)

	iret = 0

	CALL DMENDF(iflno,iret)
	CALL NLDNINIT()

	RETURN
	END
