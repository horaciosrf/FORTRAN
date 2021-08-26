program arquivos
    implicit none

    !Programa para testar abertura de arquivos para entrada e saidas
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 25/05/2021
    real         :: x, m, soma
    integer      :: N, i

    soma = 0.

    open(1,file='dados.dat', status='old', action='read')
    

    read(1,*)N
    
    do i = 1, N
        read(1,*)x
        soma = soma + x
    end do

    close(1)
    
    m = soma/N

    write(*,*)m



end program arquivos