program metrooscilador

integer, parameter :: dp = selected_real_kind(15, 307)
real(dp) , allocatable :: x(:),xi_squared_arr(:),xi_quart_arr(:), E_array(:)
real(dp) :: a,epsilon,rand,extrand,SE,SE1,rho,deltaSE,prob,E_cont,E_cont_2
integer :: i,j,k,acc,measures_cont,n,sweeps,measures,steps
real(dp) :: lambda, E_mean, E_error
real(dp) :: acc_rate
integer :: n_arr(9), M
real(dp) :: epsilon_arr(9)


n_arr = (/10, 20, 30, 40, 50, 70, 100, 150,200/)
epsilon_arr = (/0.75, 0.65, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25,0.2/) !lambda = 0
!epsilon_arr = (/0.65, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25,0.2/) !lambda = 0.5
!epsilon_arr = (/0.6, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25,0.2/) !lambda = 1

write(*,*) "E_0, err, acc_rate"

do k = 1, 7

	n = n_arr(k)
	epsilon = real(epsilon_arr(k),dp)
	!n = 100
	!epsilon = 0.35
	
	lambda = 0.0d0
	
	a = 10.0d0/real(n,dp)

	acc=0
	E_cont = 0.0d0
	E_cont_2 = 0.0d0
	E_error = 0.0d0

	measures_cont=1

	sweeps = 100000000
	termalization = 100000
	steps = 1000
	measures = int( (sweeps - termalization)/steps )

	allocate(x(n))
	!allocate(E_array(measures))
	
	!Hot start
	do i=1,n
		call Random_Number(rand)
		extrand=2.0d0*rand-1.0d0
		x(i)=extrand
	end do

	do i=1, sweeps

		call sweeep3(x)

		if(i .ge. termalization .and. mod(i,steps) .eq. 0) then

			!!E_array(measures_cont) = x(1)**2.0d0 + 3.0d0 * lambda * x(1)**4.0d0
			E_cont = E_cont + x(1)**2.0d0 + 3.0d0 * lambda * x(1)**4.0d0
			E_cont_2 = E_cont_2 + (x(1)**2.0d0 + 3.0d0 * lambda * x(1)**4.0d0)**2.0d0

			!print *, x(1)**2.0d0 + 3.0d0 * lambda * x(1)**4.0d0
			
			
			measures_cont=measures_cont+1
		
		end if
		
	end do

	 M = measures_cont
	!call mean_error(E_array, measures,E_mean,E_error)
	!deallocate(E_array)

	acc_rate = acc /(real(n,dp)*sweeps)

	E_mean = E_cont / M

	E_error = (E_cont_2 - M*(E_mean)**2.0d0)/(M-1.0d0)
	E_error = sqrt(E_error/(M))

	write(*,"(F10.5,A,F10.5,A,F10.5)") E_mean,",", E_error,",", acc_rate
	deallocate(x)

end do 


contains

function S_E(arreglo) result(resultado)
    real(dp) :: arreglo(n)
    real(dp) :: resultado
    integer :: p

    resultado = 0.0d0
    
    do p = 1, (n-1)
        resultado = resultado + 0.5d0*((arreglo(p+1) - arreglo(p))/a)**2.0d0 + 0.5d0*arreglo(p)**2.0d0 + lambda*arreglo(p)**4.0d0
    end do

    ! Sumar el término adicional x_f^2
    resultado = resultado + 0.5d0*((arreglo(1) - arreglo(n))/a)**2.0d0 + 0.5d0*arreglo(n)**2.0d0 + lambda*arreglo(n) ** 4.0d0

    ! Multiplicar por a
    resultado = a * resultado

end function S_E

subroutine sweeep(x)

	real(dp), intent(inout) :: x(:)
	do j=1,n
		SE=S_E(x)
		
		call Random_Number(rand)

		rho=epsilon*(2.0*rand-1)

		x(j)=x(j)+rho
		
		SE1=S_E(x)
		
		deltaSE=SE1-SE

		if(deltaSE .le. 0 ) then
		
			acc = acc + 1 !Se acepta el cambio

		else if(deltaSE .gt. 0) then

			prob=exp(-deltaSE)
			call Random_Number(rand)

			if(rand .lt. prob ) then
			
				acc = acc + 1 !Se acepta el cambio	

			else 

				x(j)=x(j)-rho !Se rechaza el cambio

			end if

		end if


	end do
end subroutine sweeep

  !Subrutina para el calculo del cambio de acción
function delta_S(xl, xi, xr, xp) result(sum)
    implicit none

    real(dp):: xl, xi, xr, xp, sum
    
    sum = 0.0
    sum = sum + ( (xr - xp)**2 - (xr - xi)**2 + (xp - xl)**2 - (xi - xl)**2 ) / (2*a**2)
    sum = sum + 0.5*(xp**2-xi**2) + lambda*(xp**4-xi**4)
    sum = a*sum

  end function delta_S

subroutine sweeep2(x)

	real(dp), intent(inout) :: x(:)
	real(dp) :: xl, xr, xp

	do j=1,n
		
		
		call Random_Number(rand)

		rho=epsilon*(2*rand-1)

		xp = x(j) + rho

        xl = x(mod(j-1,n))
        xr = x(mod(j+1,n))
		
		deltaSE= delta_S(xl, x(j), xr, xp)

		if(deltaSE .le. 0 ) then
	
			acc = acc + 1 !Se acepta el cambio
			x(j) = xp
	
		else if(deltaSE .gt. 0) then

			prob=exp(-deltaSE)
			call Random_Number(rand)

			if(rand .lt. prob ) then
		
				acc = acc + 1 !Se acepta el cambio
				x(j) = xp

			else 

			end if

		end if


	end do
end subroutine sweeep2

subroutine sweeep3(x)
    real(dp), intent(inout) :: x(:)
    real(dp) :: xl, xr, xp!,deltaSE, rho, prob, rand
    integer :: j

    do j = 1, n
        call Random_Number(rand)
        rho = epsilon * (2.0d0 * rand - 1.0d0)
        xp = x(j) + rho

        ! Manejo cíclico de índices
		if (j == 1) then
			xl = x(n)
			xr = x(j + 1)
		else if (j == n) then
			xl = x(j - 1)
			xr = x(1)
		else
			xl = x(j - 1)
			xr = x(j + 1)
		end if
        
        deltaSE = delta_S(xl, x(j), xr, xp)

        if (deltaSE <= 0) then
            acc = acc + 1 ! Se acepta el cambio
            x(j) = xp
        else
            prob = exp(-deltaSE)
            call Random_Number(rand)

            if (rand < prob) then
                acc = acc + 1 ! Se acepta el cambio
                x(j) = xp
            end if
        end if
    end do
end subroutine sweeep3


function mean(arreglo, longitud) result(promedio)
    real(dp) :: arreglo(:)
    integer :: longitud
    real(dp) :: promedio
    integer :: m
    real(dp) :: suma

    suma = 0.0d0
    do m = 1, longitud
        suma = suma + arreglo(m)
    end do

    promedio = suma / real(longitud,dp)

end function mean

function prom_squared(arreglo,longitud) result(average)
	real(dp) :: arreglo(:)
	integer :: longitud
	real(dp) :: average,acummulated

	average=0.0d0
	acummulated=0.0d0

	do l=1,longitud

		acummulated=acummulated+arreglo(l)**2

	end do

	average=acummulated/real(longitud,dp)


end function prom_squared

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
      var = suma_cuadrados / real(n-1,dp)

      ! Calcular el error estándar
      std_err = sqrt(var / real(n,dp))

    else
      print *, "Error: La longitud de 'array' no coincide con 'n' en mean_error."
      print *, "array:",size(array)," n:",n
      stop
    end if

  end subroutine mean_error

end
