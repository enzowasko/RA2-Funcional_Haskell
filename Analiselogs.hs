logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\(LogEntry {status = s}) -> case s of {Falha _ -> True; _ -> False})

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId logs = filter (\(LogEntry {acao = a}) -> itemActionId a == itemId) logs
    where
        itemActionId (Add id_ _) = id_
        itemActionId (Remove id_ _) = id_
        itemActionId (Update id_ _) = id_
        itemActionId (QueryFail id_) = id_

report :: [LogEntry] -> String
report logs =
    " RELATORIO DE AUDITORIA DO LOG \n" ++
    "\n- Logs de Erro -\n" ++
    (if List.null errors then "Nenhuma falha registrada.\n" else unlines (map formatLogEntry errors)) ++
    "\n- Item Mais Movimentado (Top 3) -\n" ++
    (if List.null allCounts then "Nenhuma movimentacao registrada.\n" else unlines top3Report)
    where
        errors = logsDeErro logs
        isMovement (LogEntry {acao = Add _ _}) = True
        isMovement (LogEntry {acao = Remove _ _}) = True
        isMovement (LogEntry {acao = Update _ _}) = True
        isMovement _ = False
        itemActionId (LogEntry {acao = Add id_ _}) = id_
        itemActionId (LogEntry {acao = Remove id_ _}) = id_
        itemActionId (LogEntry {acao = Update id_ _}) = id_
        itemActionId _ = ""

        allCounts = List.sortBy (comparing (negate . snd)) . List.map (\l -> (List.head l, List.length l)) . List.groupBy (==) . List.sortBy (comparing id) . List.map itemActionId . List.filter isMovement $ logs
        
        top3Report = List.zipWith (\i (id', count) -> show i ++ "º: ID " ++ id' ++ " com " ++ show count ++ " operacoes.") [1..] (List.take 3 allCounts)


formatLogEntry :: LogEntry -> String
formatLogEntry logEntry =
    "[" ++ show (timestamp logEntry) ++ "] " ++ show (acao logEntry) ++ " | Status: " ++ show (status logEntry) ++ " | Detalhes: " ++ detalhes logEntry

