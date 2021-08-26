program desvio
    implicit none
    !Programa para calcular desvio padrão, utilizando vetores alocados
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 01/06/2021
    real         :: m, dp
    integer      :: N, i
    real,allocatable::v(:)

    

    open(1,file='dados.dat', status='old', action='read')
    
    read(1,*)N    
    allocate( v(N) )

    do i = 1, N
        read(1,*) v(i)        
    end do

    close(1)



    call calcula_media_dp(N, v, m, dp)

    write(*,*)'Media:',m
    write(*,*)'Desvio padrao:',dp

    contains


    subroutine calcula_media_dp(N, x, m, dp)
        implicit none
        integer            :: i
        integer,intent(in) :: N
        real,intent(in) ::  x(N)
        real, intent(out) :: m, dp
        real     :: soma, soma_2

   
        do i = 1, N
            soma = soma + x(i)
            soma_2 = soma_2 + x(i)**2
        end do

        m = soma/N
        dp = sqrt(soma_2/N -(soma/N)**2)
   
   end subroutine calcula_media_dp

 


end program desvio