real function sinc(x)
    !função feita para testar as condições da função sinc
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 29/04/2021     

    implicit none

    real, intent(in)   :: x

    if ( abs(x) > 1.e-6) then
        sinc = sin(x)/x
    else
        sinc = 1.
    end if


end function


program teste_sinc
    implicit none

    !programa para testar a função sinc
    
    real, external  :: sinc
    real            :: x

    write(*,*) 'x?'
    read(*,*) x
    write(*,*) 'f(x) =', sinc(x)   
end program teste_sinc