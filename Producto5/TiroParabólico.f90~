!Programa para Tiro Parabolico
Program Tiro_Parabolico
	implicit none
	real, parameter :: pi = 4.0*atan(1.0)
	real, parameter :: g = 9.81
	real :: v, a, t, h, d, a_grados
	real :: x(3000),y(3000)
	integer :: i
	write(*,*) 'Proporcione un ángulo inicial en grados'
	read *, a_grados
	write(*,*)'Proporcione una velocidad inicial para el proyectil en m/s'
	read *, v
	a = a_grados*pi/180.0
	t = 2*v*sin(a)*(1/g)
	h = v*v*sin(a)*sin(a)*(1/(2*g))

	IF (a_grados == 90 ) THEN
		d = 0
               ELSE
  		d = v*cos(a)*t
	ENDIF
      

	print * , 'Tiempo de vuelo=' , t, 's'
	print * , 'Altura maxima=' , h, 'm'
	print * , 'Distancia maxima=' , d, 'm'

	open (1, file = 'Proyectil.dat')
	open (2, file = 'Tiempos.dat')

	do i = 1,3000
		t = (float(i)*0.01)
		x(i) = v*cos(a)*t
		y(i) = v*sin(a)*t - 0.5*g*t*t
		write(1,*) x(i), y(i)
		write (2,*) t
		if (y(i)<0) exit
	end do
        close(1)
End Program Tiro_Parabolico
