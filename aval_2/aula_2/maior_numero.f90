program maiornumero
    !Programa para analisar qual o maior numero de um arquivo
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 27/05/2021
    implicit none

    real ::x, maior, maior2, maior3
    real ::maiora1,maiora2
    integer  :: i, linha, linha2, linha3,li1,li2
    integer       :: erro



    open(2, file='dados_2.dat', status='old',action='read', iostat=erro)

    read(2,*,iostat=erro) x
    if ( erro == 0 ) then
        maior = x
        maior2 = x
        maior3 = x
        linha = 1
        linha2 = 1
        linha3 = 1
        i = 1
        do 
            read(2,*,iostat=erro) x
            if ( erro ==0 ) then
                i = i + 1
                if ( x > maior ) then
                    maiora1 = maior
                    li1=linha
                    maior = x
                    linha = i
                    if ( maiora1 > maior2 ) then
                        maiora2 = maior2
                        li2 = linha2
                        maior2 = maiora1
                        linha2 = li1
                        if ( maiora2 > maior3 ) then
                            maior3 = maiora2
                            linha3 = li2
                        end if
                    end if
                else if ( x > maior2 ) then
                    maiora2 = maior2
                    maior2 = x
                    linha2 = i                    
                else if ( x > maior3 ) then
                    maior3 = x
                    linha3 = i 
                end if
            else
                exit                
            end if

        end do
        write(*,*)'O arquivo tem',i,'valores.'
        write(*,*)'O maior numero e',maior,'na linha',linha,'.'
        write(*,*)'O 2 maior numero e',maior2,'na linha',linha2,'.'
        write(*,*)'O 3 maior numero e',maior3,'na linha',linha3,'.'
    else
        write(*,*)'Erro na primeira leitura!!!!'
    end if



    
end program maiornumero