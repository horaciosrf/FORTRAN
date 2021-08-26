Real function exp_cos (x)
    implicit none

    !função feita em arquivo diferente do programa
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 27/04/2021 
    
    real, intent(in) ::x

    exp_cos = exp(-x**2) * cos(10. * x) 

end function exp_cos