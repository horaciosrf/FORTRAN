program Fahrenheit

!Programa para converter a temperatura de fahrenheit pra celsius
!Programador: Horácio Santiago Ribeiro Ferreira
!Data: 15/04/2021
implicit none

Real :: F  !variavel fahrenheit
Real :: C  !variaval celsius

!Entrar com o valor da variável F, temperatura em graus Fahrenheit
Write(*,*) 'Escreva a temperatura em Fahrenheit:'
Read(*,*) F 

    C = 5. * (F-32.) /9.  !Equação de conversão entre Fahrenheit e Celsius

    Write(*,*)'Temperatura em Celsius:',C  !Escrever no terminal a temperatura em graus Celsius


    
    
end program Fahrenheit
