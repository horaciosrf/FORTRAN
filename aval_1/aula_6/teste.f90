program quad
    implicit none
    !programa para testar a subroutine
    !problema: raizes de uma função quadrática
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 06/05/2021 

    real         ::a,b,c
    real         ::r1, r2, r1i, r2i
    integer      :: Nr
    character(3) :: resp

    write(*,*)'Calculo das raizes reais da equacao ax^2 + bx + c = 0.'
    write(*,*)'Entre com a,b,c:'
    read(*,*) a, b, c
    write(*,*)'Deseja somente raizes reais? Digite sim ou nao'
    read(*,*) resp
    if (resp == 'sim') then
        call quadratic(a,b,c,Nr,r1,r2)

        if (Nr == 2) then
            write(*,*)'Duas raizes reais:'
            write(*,*)'r1:',r1
            write(*,*)'r2:',r2
        else if (Nr == 1) then
            write(*,*)'Uma raiz real:'
            write(*,*)'r:',r1
        else if (Nr == 0) then
            write(*,*)'Nao tem raizes reais'
        end if 
    else if ( resp == 'nao' ) then
         call quadratic_complex(a,b,c,Nr,r1,r2,r1i,r2i)

         if (Nr == 2) then
           write(*,*)'Tem duas raizes reais:'
           write(*,*)'r1:',r1
           write(*,*)'r2:',r2
         else if (Nr == 1) then
           write(*,*)'Uma raiz real:'
           write(*,*)'r:',r1
        else if (Nr == 0) then
           write(*,*)'Tem duas raizes complexas'
           write(*,*)'Re(r1):',r1,'Im(r1):',r1i
           write(*,*)'Re(r2):',r2,'Im(r2):',r2i
        end if  
    end if


    contains

    subroutine quadratic(a,b,c,Nr,r1,r2)
        !calculo das raizes reais da equação ax^2 + bx + c = 0.
        !calculo só de raizes reais e se possue uma, duas ou nenhuma raiz real
        implicit none
        real, intent(in)       :: a, b, c
        integer, intent(out)   :: Nr
        real, intent(out)      :: r1, r2
        real                   :: delta

        delta = (b**2) - (4.*a*c)

        if ( delta > 0. ) then
            Nr = 2
            r1 = (-b + sqrt(delta))/(2.*a) 
            r2 = (-b - sqrt(delta))/(2.*a)
        else if ( delta < 0. ) then
            Nr = 0
        else
            Nr = 1
            r1 = -b/(2.*a)
            r2 = r1
        end if    
    end subroutine quadratic

    subroutine quadratic_complex(a,b,c,Nr,r1,r2,r1i,r2i)
        !calculo das raizes reais da equação ax^2 + bx + c = 0.
        !calculo só de raizes reais e se possue uma, duas ou nenhuma raiz real
        implicit none
        real, intent(in)       :: a, b, c
        integer, intent(out)   :: Nr
        real, intent(out)      :: r1, r2, r1i, r2i
        real                   :: delta, mod_delta

        delta = (b**2) - (4.*a*c)
        mod_delta = abs(delta)
        if ( delta > 0. ) then
            Nr = 2
            r1 = (-b + sqrt(delta))/(2.*a) 
            r1i = 0.
            r2 = (-b - sqrt(delta))/(2.*a)
            r2i = 0.
        else if ( delta < 0. ) then
            Nr = 0
            r1 = -b/(2.*a)
            r1i = + sqrt(mod_delta)/(2.*a)
            r2 = -b/(2.*a)
            r2i = - sqrt(mod_delta)/(2.*a)
        else
            Nr = 1
            r1 = -b/(2.*a)
            r1i = 0.
            r2i = 0.
            r2 = r1
        end if    
    end subroutine quadratic_complex
    

end program quad