program metrooscilador

real , allocatable :: x(:),xi_squared_arr(:),xi_quart_arr(:), E_array(:)
real :: a,epsilon,rand,extrand,SE,SE1,rho,deltaSE,prob
integer :: i,j,k,acc,measures_cont,n,sweeps,measures,steps
real :: lambda, E_mean, E_error
real :: acc_rate
integer :: n_arr(9)
real :: epsilon_arr(9)

n_arr = (/10, 20, 30, 40, 50, 70, 100, 150,200/)
epsilon_arr = (/0.75, 0.65, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25,0.2/) !lambda = 0
!epsilon_arr = (/0.65, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25,0.2/) !lambda = 0.5
!epsilon_arr = (/0.6, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25,0.2/) !lambda = 1

write(*,*) "E_0, err, acc_rate"

do k = 1, 9

	n = n_arr(k)
	epsilon = epsilon_arr(k)
	
	lambda = 0.0
	
	a = 10.0/real(n)

	acc=0
	measures_cont=1

	sweeps = 4010000
	termalization = 10000
	steps = 10
	measures = int( (sweeps - termalization)/steps )

	allocate(x(n))
	allocate(xi_squared_arr(measures))
	allocate(xi_quart_arr(measures))
	
	!Hot start
	do i=1,n
		call Random_Number(rand)
		extrand=2.0*rand-1.0
		x(i)=extrand
	end do

	do i=1, sweeps

		call sweeep3(x)

		if(i .ge. termalization .and. mod(i,steps) .eq. 0.0) then
			
			xi_squared_arr(measures_cont)=x(1) ** 2.0
			xi_quart_arr(measures_cont)=x(1)**4.0

			measures_cont=measures_cont+1
		
		end if
		
	end do

	E_array = xi_squared_arr + 3.0 * lambda*xi_quart_arr
	deallocate(xi_squared_arr)
	deallocate(xi_quart_arr)
	

	call mean_error(E_array, measures,E_mean,E_error)

	acc_rate = acc /(real(n)*sweeps)

	write(*,"(F10.5,A,F10.5,A,F10.5)") E_mean,",", E_error,",", acc_rate
	deallocate(x)

end do 


contains

function S_E(arreglo) result(resultado)
    real :: arreglo(n)
    real :: resultado
    integer :: p

    resultado = 0.0
    
    do p = 1, (n-1)
        resultado = resultado + 0.5*((arreglo(p+1) - arreglo(p))/a)**2 + 0.5*arreglo(p)**2 + lambda*arreglo(p)**4
    end do

    ! Sumar el término adicional x_f^2
    resultado = resultado + 0.5*((arreglo(1) - arreglo(n))/a)**2 + 0.5*arreglo(n)**2 + lambda*arreglo(n) ** 4

    ! Multiplicar por a
    resultado = a * resultado

end function S_E

subroutine sweeep(x)

	real, intent(inout) :: x(:)
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

    real:: xl, xi, xr, xp, sum
    
    sum = 0.0
    sum = sum + ( (xr - xp)**2 - (xr - xi)**2 + (xp - xl)**2 - (xi - xl)**2 ) / (2*a**2)
    sum = sum + 0.5*(xp**2-xi**2) + lambda*(xp**4-xi**4)
    sum = a*sum

  end function delta_S

subroutine sweeep2(x)

	real, intent(inout) :: x(:)
	real :: xl, xr, xp

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
    real, intent(inout) :: x(:)
    real :: xl, xr, xp!,deltaSE, rho, prob, rand
    integer :: j

    do j = 1, n
        call Random_Number(rand)
        rho = epsilon * (2.0 * rand - 1)
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
    real :: arreglo(:)
    integer :: longitud
    real :: promedio
    integer :: m
    real :: suma

    suma = 0.0
    do m = 1, longitud
        suma = suma + arreglo(m)
    end do

    promedio = suma / real(longitud)

end function mean

function prom_squared(arreglo,longitud) result(average)
	real :: arreglo(:)
	integer :: longitud
	real :: average,acummulated

	average=0.0
	acummulated=0.0

	do l=1,longitud

		acummulated=acummulated+arreglo(l)**2

	end do

	average=acummulated/real(longitud)


end function prom_squared

  !Subrutina para calcular el promedio y error estándar de un arreglo
subroutine mean_error(array, n, mean, std_err)
    implicit none
    real, intent(in) :: array(:)
    integer, intent(in) :: n
    real, intent(out) :: mean, std_err
    real :: suma, suma_cuadrados, var
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
      mean = suma / real(n)

      ! Calcular la varianza
      do i = 1, n
          suma_cuadrados = suma_cuadrados + (array(i) - mean)**2
      end do
      var = suma_cuadrados / real(n - 1)

      ! Calcular el error estándar
      std_err = sqrt(var / real(n))

    else
      print *, "Error: La longitud de 'array' no coincide con 'n' en mean_error."
      print *, "array:",size(array)," n:",n
      stop
    end if

  end subroutine mean_error

end
