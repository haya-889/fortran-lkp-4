      PROGRAM MOMEN_BALOK
      IMPLICIT NONE
      REAL*8 L, P, PV, A, RAD, VA, VB, X, DX, MX

*	INPUT     
      WRITE(*,'(A,\)') ' panjang balok (L):                  '
      READ(*,*) L
      WRITE(*,'(A,\)') ' posisi gaya dari tumpuan kiri (A):  '
      READ(*,*) A
      WRITE(*,'(A,\)') ' besar gaya (P):                     '
      READ(*,*) P
      WRITE(*,*) ' sudut gaya terhadap horizontal'
      WRITE(*,'(A,\)') '( dalam derajat):                    '
      READ(*,*) RAD
      WRITE(*,'(A,\)') ' interval perhitungan (DX):          '
      READ(*,*) DX

*	KONVERSI SUDUT KE RADIAN
      RAD = RAD * 3.14159265 / 180.0

      PV = P * SIN(RAD)


      VB = (PV * A) / L
      VA = PV - VB

*	MENENTUKAN KONTROL ATAU TIDAK
      IF ((VA + VB) .EQ. PV) THEN
         WRITE(*,*) 'KONTROL'
      ELSE
         WRITE(*,*) 'TIDAK KONTROL'
	STOP  ! PROGRAM BERHENTI JIKA TIDAK KONTROL
      
      ENDIF

      WRITE(*,*) '-------------------------------------'
      WRITE(*,*) '  X (m)    |      Mx (Nm)     | '
      WRITE(*,*) '-------------------------------------'

      X = 0.0
      DO WHILE (X <= L)
         IF (X < A) THEN
            MX = VA * X
         ELSE
            MX = VA * X - PV * (X - A)
         ENDIF
      WRITE(*, '(1X, F8.2, 3X, "|", 3X, F12.2, 3X, "|")') X, MX         
	X = X + DX
      END DO

      WRITE(*,*) '-------------------------------------'

      END PROGRAM
