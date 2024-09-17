module utils
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)

contains

  !Subrutina para generar un número aleatorio entero
  function random_number_int(min, max) result(number)
    implicit none

    integer :: min, max
    integer :: number
    real(dp) :: rand

    call random_number(rand)

    number = min + int(rand * (max - min + 1))
  end function random_number_int

  !Subrutina para generar un número aleatorio real
  function random_number_real(min, max) result (number)
    implicit none

    real(dp) :: min, max
    real(dp) :: number
    real(dp) :: rand

    call random_number(rand)

    number = min + rand * (max - min)
  end function random_number_real

  !Subrutina para calcular el promedio y error estándar de un arreglo
  subroutine mean_error(array, n, mean, std_err)
    implicit none
    real(dp), intent(in) :: array(:)
    integer, intent(in) :: n
    real(dp), intent(out) :: mean, std_err
    real(dp) :: suma, suma_cuadrados, var
    integer :: i

    if (n  == size(array)) then 

      ! Inicializar variables
      suma = 0.0
      suma_cuadrados = 0.0
      mean = 0.0
      std_err = 0.0

      ! Calcular el promedio
      do i = 1, n
          suma = suma + array(i)
      end do
      mean = suma / real(n,dp)

      ! Calcular la varianza
      do i = 1, n
          suma_cuadrados = suma_cuadrados + (array(i) - mean)**2
      end do
      var = suma_cuadrados / real(n - 1,dp)

      ! Calcular el error estándar
      std_err = sqrt(var / real(n,dp))

    else
      print *, "Error: La longitud de 'array' no coincide con 'n' en mean_error."
      print *, "array:",size(array)," n:",n
      stop
    end if

  end subroutine mean_error

  !Subrutina para el calculo de la acción
  function S_E(x,n,lambda) result(sum)
    implicit none

    real(dp) :: x(:)
    real(dp) :: a,lambda
    real(dp) :: sum
    integer :: n
    integer :: i

    if (n == size(x)) then
      sum = 0.0
      a = 10.0/real(n,dp) !L/N
      

      do i = 1, n
        if (i == n) then
          sum = sum + (0.5*((x(1)-x(i))/a)**2 + 0.5*x(i)**2 + lambda*x(i)**4) !Condición periodica x[N+1] = x[0]
        else
          sum = sum + (0.5*((x(i+1)-x(i))/a)**2 + 0.5*x(i)**2 + lambda*x(i)**4)
        end if
      end do

      sum = a*sum

    else
      print *, "Error: La longitud de 'array' no coincide con 'n' en S_ E"
      print *, "array:",size(x)," n:",n
      stop
    end if

  end function S_E

  !Subrutina para el calculo del cambio de acción
  function delta_S(xl, xi, xr, xp, n, lambda) result(sum)
    implicit none

    real(dp) :: xl, xi, xr, xp,a, lambda, sum
    integer :: n
    a = 10.0/real(n,dp) !L/N

    sum = 0.0
    sum = sum + ( (xr - xp)**2 - (xr - xi)**2 + (xp - xl)**2 - (xi - xl)**2 ) / (2*a**2)
    sum = sum + 0.5*(xp**2-xi**2) + lambda*(xp**4-xi**4)
    sum = a*sum

  end function delta_S

  !Subrutina para el algoritmo de Metropolis
  subroutine sweep()
    implicit none

  end subroutine sweep

  !Subrutina para el calculo de la energia del estado base
  subroutine Energy()
    implicit none

  end subroutine Energy

end module utils

subroutine S_E_test(n,lambda)
  use utils
  implicit none  

  !Declarar variables
  real(dp) , intent(in):: lambda
  real(dp) :: sc1,sr,a
  real(dp), allocatable:: x1(:), x2(:)
  integer, intent(in) :: n
  integer :: i

  !Asignar valores iniciales
  a = 10.0/real(n,dp)

  allocate(x1(n))
  allocate(x2(n))

  do i = 1, n
    x1(i) = real(i-1)
  end do
  x2 = x1
  x2(6) = 6.0
 
  sc1 = S_E(x2, n,lambda) - S_E(x1, n,lambda)
  sr = delta_S(x1(5),x1(6),x1(7),x2(6),n,lambda)

  write(*,'(A,30F6.1)') "Array 1: ",x1
  write(*,'(A,30F6.1)') "Array 2: ",x2
  print *, "S array 1: ",S_E(x1, n,lambda)
  print *, "S array 2: ",S_E(x2, n,lambda)
  print *, "Delta S fórmula completa: ",sc1
  print *, "Delta S fórmula reducida: ",sr
  deallocate(x1)
  deallocate(x2)


end subroutine S_E_test

!Programa principal
program main
  use utils
  implicit none
  

  real(dp) :: lambda
  integer :: n

  n = 500
  lambda = 1.0
  call S_E_test(n,lambda)
end program main