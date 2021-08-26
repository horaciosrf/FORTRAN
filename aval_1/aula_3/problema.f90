program problema_de_casa
    implicit none
    ! Problema deixado para fazer em casa e trazer na próxima aula
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 27/04/2021 
    !Problema: função para calcular a area de um triangulo, dado somente os lados a,b e c
    real ::a,b,c

    write(*,*)'entre com os lados a,b e c do triangulo:'
    read(*,*) a, b, c

    write(*,*)'Area do triangulo:', area_tri(a,b,c)





    contains
    Real function area_tri (a1, b1, c1)
        implicit none
        !função da área de um triangulo dado somente os lados.
        !como nãoo há nenhuma informação dos angulos e da altura, utilizaremos a formula de heron
        !nela, calculamos um semiperimetro p=(a+b+c)/2 e colocamos na forumla de heron:
        !A=sqrt(p(p-a)(p-b)(p-c))

        real, intent(in) :: a1,b1,c1
        real             :: p 

        p=(a1+b1+c1)/2.

        area_tri = sqrt(p*(p-a)*(p-b)*(p-c))

    end function area_tri


end program problema_de_casa