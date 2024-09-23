program metrooscilador

real , allocatable :: x(:)
real :: a,epsilon,rand,extrand,SE,SE1,rho,deltaSE,prob
integer :: i,j,exitos,measures,n,sweeps
real :: final_xi_squared,xi_squared_arr(100000),lambda
real :: final_xi_quart,xi_quart_arr(100000)

n = 10
lambda=0
epsilon = 0.75
a = 10.0/real(n)

rand=0
extrand=0
SE=0
SE1=0
rho=0
exitos=0
prob=0
measures=1
final_xi_squared=0
final_xi_quart=0

sweeps = 1010000


allocate(x(n))	

do i=1,n
	call Random_Number(rand)
	extrand=2*rand-1
	x(i)=extrand
end do



do i=1, sweeps

	do j=1,n
		SE=S_E(x)
		
		call Random_Number(rand)

		rho=epsilon*(2*rand-1)

		x(j)=x(j)+rho
		
		SE1=S_E(x)
		
		deltaSE=SE1-SE

		if(deltaSE .le. 0 ) then
		
			if (i > 10000) then
				exitos = exitos + 1 !Se acepta el cambio
			end if

		else if(deltaSE .gt. 0) then

			prob=exp(-deltaSE)
			call Random_Number(rand)

			if(rand .lt. prob ) then
			
				if (i > 10000) then
					exitos = exitos + 1 !Se acepta el cambio
				end if

			else 

				x(j)=x(j)-rho !Se rechaza el cambio

			end if

		end if


	end do

	if(i .ge. 10000 .and. mod(i,10) .eq. 0) then
		
		xi_squared_arr(measures)=x(1) ** 2
		xi_quart_arr(measures)=x(1)**4
		measures=measures+1
	
	end if
	

end do

final_xi_squared=mean(xi_squared_arr,(measures-1))
final_xi_quart=mean(xi_quart_arr,(measures-1))

write(*,*) 'acc_rate: ', exitos /(real(n)*sweeps)
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
