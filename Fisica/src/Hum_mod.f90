program metrooscilador

real , allocatable :: x(:),xi_squared_arr(:),xi_quart_arr(:)
real :: a,epsilon,rand,extrand,SE,SE1,rho,deltaSE,prob
integer :: i,j,acc,measures_cont,n,sweeps,measures,steps
real :: final_xi_squared,lambda
real :: final_xi_quart

n = 20
lambda=0
epsilon = 0.6
a = 10.0/real(n)

acc=0
measures_cont=1


sweeps = 1010000
termalization = 10000
steps = 10
measures = int( (sweeps - termalization)/steps )

allocate(x(n))
allocate(xi_squared_arr(measures))
allocate(xi_quart_arr(measures))

do i=1,n
	call Random_Number(rand)
	extrand=2*rand-1
	x(i)=extrand
end do

do i=1, sweeps

	call sweeep(x)

	if(i .ge. termalization .and. mod(i,steps) .eq. 0) then
		
		xi_squared_arr(measures_cont)=x(1) ** 2
		xi_quart_arr(measures_cont)=x(1)**4
		measures_cont=measures_cont+1
	
	end if
	
end do

final_xi_squared=mean(xi_squared_arr,(measures_cont-1))
final_xi_quart=mean(xi_quart_arr,(measures_cont-1))

write(*,*) 'acc_rate: ', acc /(real(n)*sweeps)
write(*,*) 'E_0', final_xi_squared+3.*lambda*final_xi_quart


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

		rho=epsilon*(2*rand-1)

		x(j)=x(j)+rho
		
		SE1=S_E(x)
		
		deltaSE=SE1-SE

		if(deltaSE .le. 0 ) then
		
			if (i > 10000) then
				acc = acc + 1 !Se acepta el cambio
			end if

		else if(deltaSE .gt. 0) then

			prob=exp(-deltaSE)
			call Random_Number(rand)

			if(rand .lt. prob ) then
			
				if (i > 10000) then
					acc = acc + 1 !Se acepta el cambio
				end if

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
		
			if (i > 10000) then
				acc = acc + 1 !Se acepta el cambio
				x(j) = xp
			end if

		else if(deltaSE .gt. 0) then

			prob=exp(-deltaSE)
			call Random_Number(rand)

			if(rand .lt. prob ) then
			
				if (i > 10000) then
					acc = acc + 1 !Se acepta el cambio
					x(j) = xp
				end if

			else 

				x(j)=x(j)-rho !Se rechaza el cambio

			end if

		end if


	end do
end subroutine sweeep2

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

end
