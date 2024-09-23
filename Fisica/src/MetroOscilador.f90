program metrooscilador

real :: x(20),cold(20),a,b,c,rand,extrand,SE,SEC,SE1,SE2,rho,deltaSE,prob,arrSE(100000),SEf,SEf2,SEprom
integer :: i,j,k,exitos,contador,exitos2
character(len=30) :: filename   ! Nombre del archivo
integer :: unit                 ! Unidad del archivo
real :: delta2SE,arrCSE(100000),SECprom,acumSE,acumSEC,VarhSE,VarcSE,errorH,errorC
real :: average_xi_squared_arr(100000),average_xi_squared,final_xi_squared,xi_squared_arr(100000),lambda
real :: average_xi_quart_arr(100000),average_xi_quart,final_xi_quart,xi_quart_arr(100000)
real :: corrMatrix(10000,20)
integer :: auxcounter,temp

rand=0
extrand=0
SE=0
SE1=0
rho=0
exitos=0
prob=0
SEf=0
unit=10
contador=1
SEprom=0
SE2=0
SEf2=0
delta2SE=0
SEC=0
exitos2=0
SECprom=0
acumSE=0
acumSEC=0
VarcSE=0
VarhSE=0
errorH=0
errorC=0
final_xi_squared=0
average_xi_squared=0
final_xi_quart=0
average_xi_quart=0
lambda=0
temp=1
auxcounter=1
	
do i=1,20
	call Random_Number(rand)

extrand=2*rand-1
x(i)=extrand
end do

do i=1,20
	cold(i)=0
end do

do i=1,1010000
	do j=1,20
		SE=calcular_suma(x)
		SEC=calcular_suma(cold)
		call Random_Number(rand)
		rho=0.6*(2*rand-1)
		x(j)=x(j)+rho
		cold(j)=cold(j)+rho
		SE1=calcular_suma(x)
		SE2=calcular_suma(cold)
		deltaSE=SE1-SE
		delta2SE=SE2-SEC
		if(deltaSE .le. 0 ) then
		
		if (i > 10000) then
			exitos = exitos + 1
		end if
		else if(deltaSE .gt. 0) then

			prob=exp(-deltaSE)
			call Random_Number(rand)
				if(rand .lt. prob ) then
				
				! Se actualiza 'x' y se decide si se incrementa 'exitos'
			if (i > 10000) then
				exitos = exitos + 1
			end if
				else 
				x(j)=x(j)-rho
				end if
		end if

		if(delta2SE .le. 0 ) then
			
				if (i > 10000) then
			exitos2 = exitos2 + 1
		end if
		else if(delta2SE .gt. 0) then

			prob=exp(-delta2SE)
			call Random_Number(rand)
				if(rand .le. prob ) then
				
					if (i > 10000) then
			exitos2 = exitos2 + 1
		end if
				else 
				cold(j)=cold(j)-rho
				end if
		end if

	end do

	if(i .ge. 10000 .and. mod(i,10) .eq. 0) then
		SEf=calcular_suma(x)
		arrSE(contador)=SEf
		SEf2=calcular_suma(cold)
		arrCSE(contador)=SEf2
		xi_squared_arr(contador)=x(1) ** 2
		xi_quart_arr(contador)=x(1)**4
		contador=contador+1
		

	end if
	if(i .ge. 10000 .and. mod(i,5) .eq. 0) then
	corrMatrix(auxcounter,temp)=x(1)*x(temp)
	auxcounter=auxcounter+1
	if(auxcounter .ge. 10000) then
			auxcounter=1
			temp=temp+1
		end if
	end if 

end do
final_xi_squared=calcular_promedio(xi_squared_arr,50000)
final_xi_quart=calcular_promedio(xi_quart_arr,50000)
call calcular_promedios_y_escribir(corrMatrix,50000,20,'CorrRes.txt')


write(*,*) 'La cantidad de exitos para el hot start es: ', exitos
write(*,*) 'El porcentaje de exitos en el hot start es: ', (exitos*100) /10200000
write(*,*) 'La cantidad de exitos en el cold start es: ',exitos2
write(*,*) 'El porcentaje de exitos en el cold start es: ', (exitos2*100)/10200000
write(*,*) 'La energia de estado base E_0 es aproximadamente', final_xi_squared+3.*lambda*final_xi_quart



! Nombre del archivo
filename = 'ResMetroOscilador.txt'

! Abrir archivo para escritura
open(unit=unit, file=filename, status='replace', action='write')

! Escribir datos en el archivo
do k = 1, 50000
	write(unit, '(I5,1X, F10.6,1X,F10.6)') k-1, arrSE(k),arrCSE(k)  ! Formato: índice en la primera columna, dato en la segunda
end do

! Cerrar archivo
close(unit)

print *, 'Datos escritos en el archivo ', trim(filename)

SEprom=calcular_promedio(arrSE,50000)
SECprom=calcular_promedio(arrCSE,50000)

do j=1,50000
	acumSE=acumSE+arrSE(j)**2
	acumSEC=acumSEC+arrCSE(j)**2
end do

VarhSE=(1./49999.)*(acumSE-50000.*(SEprom**2))
VarcSE=(1./49999.)*(acumSEC-50000.*(SECprom**2))

errorH=sqrt(VarhSE/50000)
errorC=sqrt(VarcSE/50000)
write(*,*) 'El promedio para SE en el cold start es: ',SEprom
write(*,*) 'El error para el promedio de SE hot start es: ',errorH
write(*,*) 'El promedio para SE en el cold start es: ',SECprom
write(*,*) 'El error en SE para el cold start es: ',errorC



contains

function calcular_suma(arreglo) result(resultado)
    real :: arreglo(20)
    real :: resultado
    integer :: p
    

    resultado = 0.0
    

    do p = 1, 19
        resultado = resultado + 0.5*((arreglo(p+1) - arreglo(p))/0.5)**2 + 0.5*arreglo(p)**2 + 0*arreglo(p)**4
    end do

    ! Sumar el término adicional x_f^2
    resultado = resultado + 0.5*((arreglo(1) - arreglo(20))/0.5)**2 + 0.5*arreglo(20)**2 + 0*arreglo(20) ** 4

    ! Multiplicar por 1/2
    resultado = 0.5 * resultado

end function calcular_suma

function calcular_promedio(arreglo, longitud) result(promedio)
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

end function calcular_promedio

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

 subroutine calcular_promedios_y_escribir(matriz, n_filas, n_columnas, nombre_archivo)
        integer, intent(in) :: n_filas, n_columnas
        real, dimension(n_filas, n_columnas), intent(in) :: matriz
        character(len=*), intent(in) :: nombre_archivo
        real, dimension(n_columnas) :: promedio
        integer :: i, j
        integer :: unidad
        integer :: ios
        real, dimension(n_columnas) :: acummerrors,Var,errors


        ! Inicializamos el promedio a cero
        promedio = 0.0
        acummerrors =0.0
        Var=0.0
        errors=0.0

        ! Calcular los promedios de las columnas
        do j = 1, n_columnas
            do i = 1, n_filas
                promedio(j) = promedio(j) + matriz(i, j)
                acummerrors(j)=acummerrors(j)+ matriz(i, j)**2
            end do
            promedio(j) = promedio(j) / n_filas
            Var(j)=(1./49999.)*(acummerrors(j)-50000.*(promedio(j)**2))
            errors(j)=sqrt(abs(Var(j))/50000.)
        end do

        ! Abrir el archivo para escribir los resultados
        open(newunit=unidad, file=nombre_archivo, status='replace', iostat=ios)
        
        if (ios /= 0) then
            print *, "Error al abrir el archivo ", trim(nombre_archivo)
            return
        end if

        ! Escribir los índices y promedios en formato columna
        do j = 1, n_columnas
            write(unidad, '(I4, 1X, F10.6, 1X, F10.6)') j, promedio(j), errors(j)
        end do

        close(unidad)
        print *, "Promedios escritos en el archivo ", trim(nombre_archivo)

    end subroutine calcular_promedios_y_escribir



end