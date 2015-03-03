! Volumen . f90 : Calcular el volumen
! −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
Program Volumen_circulo ! Begin main program
  Implicit None ! Declare all variables
  Real *8 :: radius , height , volume ! Declare Reals
  Real *8 :: PI = 4.0 * atan(1.0) ! Declare , assign Real
  Integer :: model_n = 1 ! Declare , assign Ints
  print * , 'Enter a radius:' ! Talk to user
  read * , radius ! Read into radius
  print * , 'Enter a height :' ! Talk to user
  read * , height ! Read into height
  volume = 0.3333 * PI * height * height * (3 * radius - height)  ! Calc volume
  print * , 'Program number =' , model_n ! Print program number
  print * , 'Radius =' , radius ! Print radius
  print * , 'height =' , height ! Print height
  print * , 'volume =' , volume ! Print volume
End Program Volumen_circulo ! End main program code
