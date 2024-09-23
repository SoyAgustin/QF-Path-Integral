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
      suma = 0.0d0
      suma_cuadrados = 0.0d0
      mean = 0.0d0
      std_err = 0.0d0

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

  !Subrutina para inicializar el arreglo, cold o hot start
  subroutine initialize_array(x, n, start) 
    implicit none

    real(dp), intent(inout) :: x(:)
    integer, intent(in) :: n
    integer, intent(in) :: start
    integer :: i

    if (size(x) == n) then

      if (start == 1) then !Hot start

        do i = 1, n
          x(i) = random_number_real(-1.0d0, 1.0d0)
        end do

      else if (start == 0) then !Cold start

        do i = 1, n
          x(i) = 0.0
        end do

      end if

    else 
      print *, "Error: La longitud de 'array' no coincide con 'n' en initialize_array."
      print *, "x:",size(x)," n:",n
      stop
    end if

    end subroutine initialize_array

  !Subrutina para el calculo de la acción
  function S_E(x,n,lambda) result(sum)
    implicit none

    real(dp) :: x(:)
    real(dp) :: a,lambda
    real(dp) :: sum
    integer :: n
    integer :: i

    if (n == size(x)) then
      sum = 0.0d0
      a = 10.0d0/real(n,dp) !L/N
      

      do i = 1, n
        if (i == n) then
          sum = sum + (0.5d0*((x(1)-x(i))/a)**2 + 0.5d0*x(i)**2 + lambda*x(i)**4) !Condición periodica x[N+1] = x[0]
        else
          sum = sum + (0.5d0*((x(i+1)-x(i))/a)**2 + 0.5d0*x(i)**2 + lambda*x(i)**4)
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
    a = 10.0d0/real(n,dp) !L/N

    sum = 0.0d0
    sum = sum + ( (xr - xp)**2 - (xr - xi)**2 + (xp - xl)**2 - (xi - xl)**2 ) / (2*a**2)
    sum = sum + 0.5d0*(xp**2-xi**2) + lambda*(xp**4-xi**4)
    sum = a*sum

  end function delta_S

  !Subrutina para el algoritmo de Metropolis
  subroutine sweep(x, n, epsilon, lambda, acc_rate)
    implicit none

    !Declarar variables
    real(dp), intent(inout) :: x(:)
    real(dp), intent(in) :: epsilon, lambda
    real(dp), intent(out) :: acc_rate
    integer, intent(in) :: n
    real(dp) :: a, rho, ds, p, r
    real(dp) :: xl, xr, xp
    integer :: i
    
    if ( n == size(x) ) then
      a = 10.0d0/real(n,dp) !L/N

      acc_rate = 0.0d0
      do i = 1, n

        rho = random_number_real(-epsilon, epsilon)
        xp = x(i) + rho
        xl = x(mod(i-1,n))
        xr = x(mod(i+1,n))

        ds = delta_S(xl, x(i), xr, xp, n, lambda)

        if (ds <= 0.0d0) then
          acc_rate = acc_rate + 1.0d0
          x(i) = xp
        else
          p = exp(-ds)
          r = random_number_real(0.0d0, 1.0d0)
          if (r < p) then
            acc_rate = acc_rate + 1.0d0
            x(i) = xp
          end if

        end if
        
      end do

      acc_rate = acc_rate / real(n,dp)
    
    else
      print *, "Error: La longitud de 'array' no coincide con 'n' en sweep"
      print *, "array:",size(x)," n:",n
      stop
    end if

  end subroutine sweep

  subroutine sweep2(x, n, epsilon, lambda, acc_rate)
    implicit none

    !Declarar variables
    real(dp), intent(inout) :: x(:)
    real(dp), intent(in) :: epsilon, lambda
    real(dp), intent(out) :: acc_rate
    integer, intent(in) :: n
    real(dp) :: a, rho, ds, ds0, ds1, p, r
    integer :: i
    
    if ( n == size(x) ) then
      a = 10.0d0/real(n,dp) !L/N

      acc_rate = 0.0d0
      do i = 1, n

        ds0 = S_E(x, n, lambda)

        rho = random_number_real(-epsilon, epsilon)

        x(i) = x(i) + rho !Hacemos un cambio

        ds1 = S_E(x, n, lambda)
        

        ds = ds1 - ds0

        if (ds <= 0.0d0) then
          acc_rate = acc_rate + 1.0d0 !Se acepta el cambio
        else
          p = exp(-ds)
          r = random_number_real(0.0d0, 1.0d0)
          if (r > p) then
             
            x(i) = x(i) - rho!Se rechaza el cambio

          else
            acc_rate = acc_rate + 1.0d0 !Se acepta el cambio
          end if

        end if
        
      end do

      acc_rate = acc_rate / real(n,dp)
    
    else
      print *, "Error: La longitud de 'array' no coincide con 'n' en sweep"
      print *, "array:",size(x)," n:",n
      stop
    end if

  end subroutine sweep2

  !Subrutina para el calculo de la energia del estado base
  subroutine ground_state_calculation(x_i, measures, lambda, energy, std_error) 
    implicit none

    real(dp), intent(in) :: x_i(:)
    real(dp), intent(in) :: lambda
    integer, intent(in) :: measures
    real(dp), intent(out) :: energy, std_error
    real(dp), allocatable :: x2_arr(:), x4_arr(:)
    real(dp) x2_mean, x4_mean, x2_std, x4_std

    if ( measures == size(x_i) ) then

      allocate(x2_arr(measures))
      allocate(x4_arr(measures))

      x2_arr = x_i**2.0d0
      x4_arr = x_i**4.0d0
      
      call mean_error(x2_arr, measures, x2_mean, x2_std)
      deallocate(x2_arr)
      call mean_error(x4_arr, measures, x4_mean, x4_std)
      deallocate(x4_arr)

      energy = x2_mean + 3*lambda*x4_mean
      std_error = x2_std + 3*lambda*x4_std

    else
      print *, "Error: La longitud de 'array' no coincide con 'n' en ground_state"
      print *, "array:",size(x_i)," n:",measures
      stop
    end if

  end subroutine ground_state_calculation

end module utils

subroutine acc_rates(n,start, sweeps, termalization , steps, epsilon, lambda)
  use utils
  implicit none

  !Declarar variables
  real(dp), allocatable :: x(:), acc_rate(:)
  real(dp), intent(in) :: epsilon, lambda
  integer, intent(in) :: n,start,sweeps, termalization, steps
  real(dp) :: a, acc, mean_acc, error_acc
  integer :: i, j, measures

  a = 10.0d0/real(n,dp)

  measures = int( (sweeps - termalization)/steps )

  allocate(x(n))
  allocate(acc_rate(measures))
  call initialize_array(x,n,start)

  j = 1
  do i = 1, sweeps
    call sweep(x, n, epsilon, lambda, acc)

    if (i> termalization .and. mod(i,steps) == 0) then

      acc_rate(j) = acc
      j = j + 1

    end if

  end do

  if(j-1 /= measures) then !verificacion del tamaño del array 'acc_rate'
    print *, "Error: La longitud de 'acc_rate' no coincide con 'measures' en acc_rate_sub"
    print *, "measures:",measures," length of acc_rate:",j
    stop
  else
    call mean_error(acc_rate, measures, mean_acc, error_acc)
    write(*,'(A,F10.4,A,F10.4)') "mean: ", mean_acc, " error: ", error_acc
  end if
  
  
end subroutine acc_rates

subroutine ground_state_full(n,start, sweeps, termalization , steps, epsilon, lambda)
  use utils
  implicit none

  !Declarar variables
  real(dp), allocatable :: x(:), acc_rate(:), x_i(:)
  real(dp), intent(in) :: epsilon, lambda
  integer, intent(in) :: n,start,sweeps, termalization, steps
  real(dp) :: a, acc, mean_acc, error_acc, energy, std_error
  integer :: i, j, measures

  a = 10.0d0/real(n,dp)

  measures = int( (sweeps - termalization)/steps )

  allocate(x(n))
  allocate(acc_rate(measures))
  allocate(x_i(measures))
  

  call initialize_array(x,n,start)

  j = 1
  do i = 1, sweeps
    call sweep2(x, n, epsilon, lambda, acc)

    if (i> termalization .and. mod(i,steps) == 0) then

      acc_rate(j) = acc
      x_i(j) = x(1)

      j = j + 1

    end if

  end do

  if(j-1 /= measures) then !verificacion del tamaño del array 'acc_rate'
    print *, "Error: La longitud de 'acc_rate' no coincide con 'measures' en acc_rate_sub"
    print *, "measures:",measures," length of acc_rate:",j
    stop
  else
    call mean_error(acc_rate, measures, mean_acc, error_acc)
    call ground_state_calculation(x_i, measures, lambda, energy, std_error)
    
    write(*,'(A,F10.4,A,F10.4)') "mean: ", mean_acc, " error: ", error_acc
    write(*,'(A,F10.4,A,F10.4)') "energy: ", energy, " error: ", std_error

    deallocate(acc_rate)
    deallocate(x_i)
    deallocate(x)
  end if
  
end subroutine ground_state_full

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
  a = 10.0d0/real(n,dp)

  allocate(x1(n))
  allocate(x2(n))

  do i = 1, n
    x1(i) = real(i-1)
  end do
  x2 = x1
  x2(6) = 6.0d0
 
  sc1 = S_E(x2, n,lambda) - S_E(x1, n,lambda)
  sr = delta_S(x1(5),x1(6),x1(7),x2(6),n,lambda)

  write(*,'(A,500F10.1)') "Array 1: ",x1
  write(*,'(A,500F10.1)') "Array 2: ",x2
  print *, "S array 1: ",S_E(x1, n,lambda)
  print *, "S array 2: ",S_E(x2, n,lambda)
  print *, "Delta S fórmula completa: ",sc1
  print *, "Delta S fórmula reducida: ",sr
  deallocate(x1)
  deallocate(x2)


end subroutine S_E_test

subroutine x_mean_test(n,lambda)
  use utils
  implicit none

  !Declarar variables
  integer, intent(in) :: n
  real(dp), intent(in) :: lambda
  integer :: start,sweeps,termalization, steps,measures
  real(dp) :: epsilon, a 
  

  real(dp), allocatable :: x(:)
  real(dp) ::  mean, error,acc, total_mean
  integer :: i


  start = 1 !0 = cold start, 1 = hot start
  sweeps = 101000
  termalization = 1000
  steps = 10
  measures = int( (sweeps - termalization)/steps )

  
  epsilon = 0.75d0
  a = 10.0d0/real(n,dp)


  total_mean = 0.0d0
  
  
  allocate(x(n))
  call initialize_array(x,n,start)

  write (*,"(A,A,A)")  "mean",",", "error"


  do i  = 1, sweeps

    call sweep(x,n,epsilon, lambda, acc)

    if (i> termalization .and. mod(i,steps) == 0) then

    call mean_error(x, n, mean, error)
    write (*,"(F10.5,A,F10.5)")  mean,",", error

    total_mean = total_mean + mean
   
    end if  

  end do

  deallocate(x)

  
  total_mean = total_mean/real(measures,dp)
  print *, "<x>_mean: ", total_mean
  

end subroutine x_mean_test

subroutine x_i_mean_test(component,n,lambda)
  use utils
  implicit none

  !Declarar variables
  integer, intent(in) :: n,component
  real(dp), intent(in) :: lambda
  integer :: start,sweeps,termalization, steps,measures
  real(dp) :: epsilon, a 
  

  real(dp), allocatable :: x(:), x_i(:) 
  real(dp) ::  mean, error,acc, total_mean
  integer :: i,j


  start = 1 !0 = cold start, 1 = hot start
  sweeps = 101000
  termalization = 1000
  steps = 10
  measures = int( (sweeps - termalization)/steps )

  
  epsilon = 0.75d0
  a = 10.0d0/real(n,dp)


  total_mean = 0.0d0
  
  
  allocate(x(n))
  allocate(x_i(measures))
  call initialize_array(x,n,start)

  j=1
  do i  = 1, sweeps

    call sweep(x,n,epsilon, lambda, acc)

    if (i> termalization .and. mod(i,steps) == 0) then

      x_i(j) = x(component)
      j = j + 1

    end if  

  end do

  deallocate(x)

  
  call mean_error(x_i, measures, mean, error)
  write(*,"(A,I0,A,F10.5,A,F10.5)") "<x_", component, "> mean: ", mean, " \pm", error

end subroutine x_i_mean_test

subroutine x_i_squared_mean(component,n,epsilon)
  use utils
  implicit none

  !Declarar variables
  integer, intent(in) :: n,component
  real(dp), intent(in) :: epsilon
  real(dp) :: lambda,a
  integer :: start,sweeps,termalization, steps,measures
  

  real(dp), allocatable :: x(:), x_i(:) 
  real(dp) ::  mean, error,acc, total_mean
  integer :: i,j


  start = 1 !0 = cold start, 1 = hot start
  sweeps = 1010000
  termalization = 10000
  steps = 10
  measures = int( (sweeps - termalization)/steps )


  lambda = 0.0d0
  a = 10.0d0/real(n,dp)

  total_mean = 0.0d0
  
  allocate(x(n))
  allocate(x_i(measures))
  call initialize_array(x,n,start)

  j=1
  do i  = 1, sweeps

    call sweep2(x,n,epsilon, lambda, acc)

    if (i> termalization .and. mod(i,steps) == 0) then

      x_i(j) = x(component)**2
      j = j + 1

    end if  

  end do

  deallocate(x)
  
  call mean_error(x_i, measures, mean, error)
  deallocate(x_i)

  write(*,"(A,I0,A,F10.5,A,F10.5)") "<x_", component, "^2> mean: ", mean, " \pm", error

end subroutine x_i_squared_mean

!Programa principal
program main
  use utils
  implicit none

  !Energia del estado base para lambda 0 <x_i^2> 
  real(dp) :: epsilon
  integer :: component,n,i
  
  n = 20
  epsilon = 0.6d0
  component  = 1
  !do component = 1, n

    call x_i_squared_mean(component, n, epsilon)

  !end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! !Test del promedio de la i-esima componente <x_i>(debe ser 0)
  ! real(dp) :: lambda
  ! integer :: component,n,i
  
  ! n = 10

  ! do component = 1, n

  !   lambda = 0.0
  !   call x_i_mean_test(component, n, lambda)

  ! end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
  ! !Test del promedio de la trayectoria completa: <x> (debe ser 0)
  ! real(dp) :: lambda
  ! integer :: n

  ! n = 100
  ! lambda = 0.0
  ! call x_mean_test(n,lambda)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! !Test del cálculo de acción y cambio de acción
  ! real(dp) :: lambda
  ! integer :: n

  ! n = 100
  ! lambda = 1.0
  ! call S_E_test(n,lambda)

end program main