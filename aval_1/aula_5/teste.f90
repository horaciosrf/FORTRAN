




program teste
    !programa com função feita para testar a variavel do tipo character
    !problema: transformar nota em conceito
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 04/05/2021 
    implicit none
    !character(3), external    :: conceito
    character(3)               :: con
    real                      :: nota

    do 
        write(*,*) 'Qual a nota?'
        read(*,*) nota

        con = conceito(nota)

        if (con /='ERR') then
            write(*,*)'Conceito:', con
            exit
        else
            write(*,*)'Nota invalida. Digite nota entre 0 e 10'
        end if

    end do

    contains

    character(3) function conceito(nota)
        !função feita para testar a variavel do tipo character
        !problema: transformar nota em conceito
        !Programador: Horácio Santiago Ribeiro Ferreira
        !Data: 04/05/2021 
        implicit none
        real, intent(in)   :: nota

        if (nota < 0.) then
            conceito='ERR'    
        else if (nota < 5.) then
            conceito = 'INS'
        else if (nota < 7.) then
            conceito = 'REG'
        else if (nota < 9.) then
            conceito = 'BOM'
        else if (nota <=10.) then
            conceito = 'EXC'
        else 
            conceito = 'ERR'
        end if

end function



end program teste