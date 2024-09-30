program metrooscilador

    real , allocatable :: x(:),corrMatrix(:,:), corrMatrix_temp(:), corrMatrix_mean(:,:)
    real :: a,epsilon,rand,extrand,SE,SE1,rho,deltaSE,prob
    integer :: i,j,d,acc,measures_cont,n,sweeps,measures,steps
    real :: lambda,corr_mean_temp, corr_error_temp
    real :: acc_rate
    
    ! n_arr = (/10, 20, 30, 40, 50, 70, 100, 150/)
    ! epsilon_arr = (/0.75, 0.65, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25/) !lambda = 0
    ! epsilon_arr = (/0.65, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25/) !lambda = 0.5
    ! epsilon_arr = (/0.6, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25/) !lambda = 1
    
    write(*,*) "corr, err"
    n = 10
    epsilon = 0.75
    lambda = 0.0
    
    a = 10.0/real(n)

    acc=0
    measures_cont=1

    sweeps = 101000
    termalization = 1000
    steps = 10
    measures = int( (sweeps - termalization)/steps )

    allocate(x(n))
    allocate(corrMatrix(n,measures))
    allocate(corrMatrix_temp(measures))
    allocate(corrMatrix_mean(n,2))

    !Hot start
    do i=1,n
        call Random_Number(rand)
        extrand=2*rand-1
        x(i)=extrand
    end do

    do i=1, sweeps

        call sweeep(x)

        if(i .ge. termalization .and. mod(i,steps) .eq. 0) then

            do d = 1,n
                corrMatrix(d,measures_cont) = x(1) * x(d)
            end do

            measures_cont=measures_cont+1
        
        end if
        
    end do

        do i = 1,n

        corrMatrix_temp = corrMatrix(i,:)
        call mean_error(corrMatrix_temp, measures, corr_mean_temp, corr_error_temp)
        corrMatrix_mean(i,1) = corr_mean_temp
        corrMatrix_mean(i,2) = corr_error_temp

    end do 
    
    deallocate(corrMatrix_temp)
    deallocate(corrMatrix)

    acc_rate = acc /(real(n)*sweeps)
        do i = 1, n
        write(*,"(F10.5,A,F10.5)") corrMatrix_mean(i,1),",", corrMatrix_mean(i,2)
        end do

    write(*,"(F10.5,A,F10.5)") corrMatrix_mean(1,1),",", corrMatrix_mean(1,2)
    
    deallocate(x)
    deallocate(corrMatrix_mean)

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
    