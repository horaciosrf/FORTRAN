program seriaexp
    implicit none
    !programa para testar laço do
    !problema: soma de uma série convergente: série de taylor
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 13/05/2021 
    real       ::x, soma


    write(*,*)'x?'
    read(*,*) x

    soma = serieexp(x)

    write(*,*)'Exponencial do x dado:',soma
    write(*,*)'funcao intriseca:', exp(x)
    !write(*,*)'N fatorial:',fatorial(N)


    contains

    integer function fatorial(N)
        implicit none
        integer, intent(in) :: N
        integer :: i
        fatorial = 1
        do i = 2, N
            fatorial = fatorial * i            
        end do
    end function fatorial

    real function serieexp(x)
        implicit none
        real, intent(in)    :: x
        real                :: eps, t
        integer             :: N
        eps = 1.e-6
        serieexp = 1.
        N = 0
        t = 1.
        do 
            N = N + 1
            t = t * x/N
       
           !if(serieexp+t == serieexp) exit
           serieexp = serieexp + t
            !write(*,*)N, t, soma

            if (abs(t) < eps) exit
        end do
    end function

end program seriaexp