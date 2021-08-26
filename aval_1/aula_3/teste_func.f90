



!Real function exp_cos (x)
   ! implicit none
    !função coloca externamente ao programa, mas dentro do mesmo arquivo

    !real, intent(in) ::x  !declarar a variavel de entrada

    !exp_cos = exp(-x**2) * cos(10. * x) !função desejada

!end function exp_cos

program testafunc
    !Programa para usar uma função construida
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 27/04/2021   


    implicit none
    real ::a
   ! real, external :: exp_cos !variável externa da função, quando a função está externa
    write(*,*)'valor de teste:'
    read(*,*) a

    write(*,*)'Funcao:', exp_cos(a)

    contains !essencial para quando há funçãos e subrotinas internas
    Real function exp_cos (x)
        implicit none
        !função interna ao programa
        
        real, intent(in) ::x

        exp_cos = exp(-x**2) * cos(10. * x) 

    end function exp_cos



end program testafunc
