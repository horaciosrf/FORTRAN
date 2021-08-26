program dado
    implicit none
    ! programa para testar função intriseca e o laço 'do'
    ! problema: programa pra servir como um dado
    ! Programador: Horácio Santiago Ribeiro Ferreira
    ! Data: 11/05/2021 

    integer     :: face  
    integer     :: f1, f2, f3, f4, f5, f6
    integer     :: i, N
    
    !call random_seed()


    !primeiro, fazer o teste da porcentagem de resultados, para não estar viciado o dado

    f1 = 0
    f2 = 0
    f3 = 0
    f4 = 0
    f5 = 0
    f6 = 0
    
    write(*,*)'quantos lancamentos?'
    read(*,*)N

    do i = 1, N
        face = dado6() 
        if ( face == 1 ) then
            f1 = f1 + 1
        else if ( face == 2 ) then
            f2 = f2 + 1 
        else if ( face == 3 ) then
            f3 = f3 + 1 
        else if ( face == 4 ) then
            f4 = f4 + 1 
        else if ( face == 5 ) then
            f5 = f5 + 1 
        else
            f6 = f6 + 1
        end if
    end do

    

    

    write(*,*)'F1:', 100 * real(f1)/N , '%'
    write(*,*)'F2:', 100 * real(f2)/N , '%'
    write(*,*)'F3:', 100 * real(f3)/N , '%'
    write(*,*)'F4:', 100 * real(f4)/N , '%'
    write(*,*)'F5:', 100 * real(f5)/N , '%'
    write(*,*)'F6:', 100 * real(f6)/N , '%'





    contains

    integer function dado6()
        implicit none
        real ::  x, dado
        
        call random_number(x)
        dado = 6.*x + 1.
        dado6 = INT(dado)

    end function dado6

end program dado