program desvio
    implicit none
    !Programa para calcular desvio padrão
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 27/05/2021
    real         :: x, m, soma, soma_2, dp
    integer      :: N, i

    

    open(1,file='dados.dat', status='old', action='read')
    
    !soma = 0.
    !read(1,*)N    
    !do i = 1, N
    !    read(1,*)x
    !    soma = soma + x
    !end do
    !m = soma/N

    !write(*,*)m
    !rewind(1)
    
    !read(1,*)N
    !soma = 0.
    !do i = 1, N
    !   read(1,*)x
    !    soma = soma + (x-m)**2
    ! end do
    ! dp = sqrt(soma/N)

    read(1,*)N    

    soma = 0.
    soma_2 = 0.

    do i = 1, N
        read(1,*)x
        soma = soma + x
        soma_2 = soma_2 + x**2
    end do

    m = soma/N
    dp = sqrt(soma_2/N -(soma/N)**2)
    write(*,*)'Media:',m
    write(*,*)'Desvio padrao:',dp

end program desvio