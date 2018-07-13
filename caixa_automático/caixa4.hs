module Main where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf
import System.Exit ( exitFailure )
import System.Environment (getArgs)
--descrição da Operação
--valor da Operação
--saldo após operação
type Estado = [ (Int, [(String, Double, Double)] ) ]--como as variaveis nao sao atualizaveis
--representação dos dados do problema em um dado momento
--saldo da conta

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Simulacao de caixa automatico"
          putStrLn ("Versao" ++ versao)
          --O estado inicial corresponde ao saldo inicial
          --da conta corrente quando a aplicação é executada: zero (0.0).
          menu [(2, [("Saldo", 0.0, 100.0), ("Deposito", 100.0, 100.0)])]
          return ()
          where versao :: String
                versao = "v0.1"
----a função menu recebe um estado (o estado atual) e
--resulta em uma ação de E/S que, quando executada, interage com o mundo e retorna um novo estado.
menu :: Estado -> IO Estado
menu sal = do putStrLn "============================"
              putStrLn "Banco Funcional"
              putStrLn "============================"
              putStrLn "Opcoes: "
              putStrLn "1. Abertura de Conta"
              putStrLn "2. Saldo "
              putStrLn "3. Extrato"
              putStrLn "4. Depósito"
              putStrLn "5. Saque"
              putStrLn "6. Fim"
              --tarefa 15--
              opcao <- prompt "Escolha uma opçao: "
              case opcao of
                1 -> do sal' <- abreConta sal
                        menu sal'
                2 -> do saldo sal
                        menu sal
                3->  do extrato sal
                        menu sal
                4 -> do sal' <- deposito sal
                        menu sal'
                5 -> do sal' <- saque sal
                        menu sal'
                6 -> do putStrLn "Encerramento"
                        return sal
                _ -> do putStrLn "Opção inválida"
                        menu sal


abreConta :: Estado -> IO Estado
abreConta (x:xs) = do num <- prompt "Informe o número de sua conta: "
                      putStrLn "Conta aberta"
                      let tupla = (num,[("Vazio", 0.0, 0.0)])
                      return  (tupla : x : xs)


saldo :: Estado -> IO Double
saldo sal = do num <- prompt "Informe o número de sua conta: "
               if lookup num sal == Nothing
                 then do putStrLn "Conta não existe"
                         return 0.0
                         else do let ((x,y):xs) = filter (\(iden, _ ) -> iden == num) sal
                                     (a,b,c) = head y
                                 putStrLn ("Seu saldo atual é de R$: " ++ show c ++ "00")
                                 return c

deposito :: Estado -> IO Estado
deposito sal = do num <- prompt "Informe o número de sua conta: "
                  if lookup num sal == Nothing
                    then do putStrLn "Conta não existe"
                            return sal
                            else do valor <- saldo sal
                                    depos <- prompt "Informe o valor a ser depositado: "
                                    if depos < 0
                                      then do putStrLn "Valor invalido"
                                              return sal
                                              else do let tupla = ("Deposito", depos, (valor+depos))
                                                          ((x,y):xs) = filter (\(iden, _ ) -> iden == num) sal
                                                          lista = (tupla:y)
                                                          sal' = (x,lista) : sal
                                                      return sal'


saque :: Estado -> IO Estado
saque sal = do num <- prompt "Informe o número de sua conta: "
               if lookup num sal == Nothing
                 then do putStrLn "Conta não existe"
                         return sal
                         else do valor <- saldo sal
                                 saque <- prompt "Informe o valor a ser sacado: "
                                 if saque < 0 || saque > valor
                                   then do putStrLn "Valor invalido"
                                           return sal
                                           else do let tupla = ("Saque", saque, (valor-saque))
                                                       ((x,y):xs) = filter (\(iden, _ ) -> iden == num) sal
                                                       lista = (tupla:y)
                                                       sal' = (x,lista) : sal
                                                   return sal'


extrato :: Estado -> IO Estado
extrato sal = do num <- prompt "Informe o número da sua conta: "
                 case lookup num sal of
                   Nothing -> do putStrLn "Conta não existe"
                                 return sal
                   Just op -> do printf "%+14s %+14s %+14s\n""Movimentaçao""Valor""Saldo"
                                 extrato' op
                                 return sal

extrato' :: [(String,Double,Double)] -> IO ()
extrato' [] = return ()
extrato' ((x,y,z):xs) = do printf "%+14s %14.2f %14.2f\n" x y z
                           extrato' xs




---a que recebe uma string e resulta em uma ação de E/S que, quando executada, exibe a string
--(normalmente uma mensagem solicitando
--um dado) e lê um dado informado pelo usuário, e retorna o valor lido
prompt :: Read a => String -> IO a
prompt mensage = do putStr mensage
                    readLn







--------------------------------
