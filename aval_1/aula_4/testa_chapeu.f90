real function chapeu (x, a)
    !função feita para testar as condições da função chapéu
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 29/04/2021 
    implicit none
    real, intent(in)  ::x, a
    
    if (x <= -a) then !estrutura de condicionamento
        chapeu = 0.
    else if (x <= 0.) then
        chapeu = (a+x)/a
    else if (x <= a) then
        chapeu = (a-x)/a
    else
        chapeu = 0.
    end if


end function

program teste
    !programa para testar a função chapeu


    implicit none
    real, external  :: chapeu
    real            :: x,a

    a=1.
    write(*,*) 'x?'
    read(*,*) x
    write(*,*) 'f(x) =', chapeu(x, a)



end program teste