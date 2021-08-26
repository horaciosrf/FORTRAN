program dado
    implicit none
    !programa para testar a função dados, sem usar um if
    !problema: programa pra servir como um dado
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 11/05/2021 

    integer     :: face  
    integer     :: f(6)
    integer     :: i, N
    
    !call random_seed()


    !primeiro, fazer o teste da porcentagem de resultados, para não estar viciado o dado

    f = 0
    
    write(*,*)'quantos lancamentos?'
    read(*,*)N

    do i = 1, N
        face = dado6() 
        f(face)= f(face) + 1
    end do

    do i = 1, 6
        write(*,*)'Face',i, 100 * real(f(i))/N , '%'
    end do

    contains

    integer function dado6()
        implicit none
        real ::  x, dado
        
        call random_number(x)
        dado = 6.*x + 1.
        dado6 = INT(dado)

    end function dado6

end program dado