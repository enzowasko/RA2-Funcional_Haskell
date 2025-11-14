carregarInventario :: IO Inventario
carregarInventario = do
    conteudo <- catch (readFile inventarioFile) handleMissingFile
    case readMaybe conteudo of
        Just inv -> return inv
        Nothing -> putStrLn "Aviso: Falha na desserialização do Inventario.dat. Iniciando com inventário vazio." >> return empty
  where
    handleMissingFile e
      | isDoesNotExistError e = putStrLn "Aviso: Inventario.dat nao encontrado. Iniciando com inventário vazio." >> return (show (empty :: Inventario))
      | otherwise = ioError e

carregarLogs :: IO [LogEntry]
carregarLogs = do
    conteudo <- catch (readFile auditoriaLogFile) handleMissingFile
    let logs_lines = lines conteudo
    let parsedLogs = List.foldl' (\acc line -> case readMaybe line :: Maybe LogEntry of
                                                    Just l -> acc ++ [l]
                                                    Nothing -> acc) [] logs_lines
    return parsedLogs
  where
    handleMissingFile e
      | isDoesNotExistError e = putStrLn "Aviso: Auditoria.log nao encontrado. Iniciando com log vazio." >> return ""
      | otherwise = ioError e

sincronizar :: Inventario -> LogEntry -> IO ()
sincronizar novoInv logEntry = do
    writeFile inventarioFile (show novoInv)
    appendFile auditoriaLogFile (show logEntry ++ "\n")  
    putStrLn "Operacao bem-sucedida. Estado salvo."

registrarFalha :: LogEntry -> IO ()
registrarFalha logEntry = do
    appendFile auditoriaLogFile (show logEntry ++ "\n")
    putStrLn $ "Operacao falhou. Log registrado: " ++ detalhes logEntry

executarEPersistir :: Inventario -> [LogEntry] -> (Inventario -> Either LogEntry (Inventario, LogEntry)) -> IO (Inventario, [LogEntry])
executarEPersistir inv logs operacaoPuraComHora =
    case operacaoPuraComHora inv of
        Right (novoInv, logEntry) -> sincronizar novoInv logEntry >> return (novoInv, logs ++ [logEntry])
        Left logEntry -> registrarFalha logEntry >> return (inv, logs ++ [logEntry])

processarComando :: String -> Inventario -> [LogEntry] -> IO (Inventario, [LogEntry])
processarComando input inv logs = do
    now <- getCurrentTime
    case words input of
        [] -> return (inv, logs)
        ["exit"] -> return (inv, logs)
        ["quit"] -> return (inv, logs)
        ["list"] -> putStrLn (listItems inv) >> return (inv, logs)
        ["report"] -> putStrLn (report logs) >> return (inv, logs)
        ["historico", id'] -> do
            let itemHistory = historicoPorItem id' logs
            putStrLn $ "\n- Historico de Log para Item ID: " ++ id' ++ " -\n"
            putStrLn $ unlines (List.map formatLogEntry itemHistory)
            return (inv, logs)

        ("add":id':name:qtdStr:cat:[]) ->
            case readMaybe qtdStr :: Maybe Int of
                Just qtd ->
                    let operacao = additem now id' name qtd cat
                    in executarEPersistir inv logs operacao
                Nothing -> putStrLn "Erro: Quantidade invalida. Use: add <id> <nome> <qtd> <cat>" >> return (inv, logs)

        ("remove":id':qtdStr:[]) ->
            case readMaybe qtdStr :: Maybe Int of
                Just qtd ->
                    let operacao = removeltem now id' qtd
                    in executarEPersistir inv logs operacao
                Nothing -> putStrLn "Erro: Quantidade invalida. Use: remove <id> <qtd>" >> return (inv, logs)

        ("update":id':qtdStr:[]) ->
            case readMaybe qtdStr :: Maybe Int of
                Just qtd ->
                    let operacao = updateQty now id' qtd
                    in executarEPersistir inv logs operacao
                Nothing -> putStrLn "Erro: Nova quantidade invalida. Use: update <id> <nova_qtd>" >> return (inv, logs)

        _ -> putStrLn "Comando invalido. Use: add <id> <nome> <qtd> <cat> | remove <id> <qtd> | update <id> <nova_qtd> | list | report | historico <id> | exit" >> return (inv, logs)

loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
    putStr "\n> "
    input <- getLine
    if input `elem` ["exit", "quit"]
        then putStrLn "Fechando o sistema de Inventario. Tchau!"
        else do
            (novoInv, novosLogs) <- processarComando input inv logs
            loop novoInv novosLogs

main :: IO ()
main = do
    putStrLn "Iniciando Sistema de Inventário Haskell (RA2)..."
    initialInv <- carregarInventario
    initialLogs <- carregarLogs
    putStrLn $"Inventário carregado com " ++ show (List.length $ toList initialInv) ++ " itens."
    putStrLn $ "Log de auditoria carregado com " ++ show (List.length initialLogs) ++ " entradas."
    putStrLn "Sistema pronto. Digite comandos. 'exit' para fechar."
    loop initialInv initialLogs