import Data.Map (Map)
import Data.Time (UTCTime)

data Item = Item
    { itemID     :: String
    , nome       :: String
    , quantidade :: Int
    , categoria  :: String
    } deriving (Show, Read)

type Inventario = Map String Item

data AcaoLog = Add String Int | Remove String Int | Update String Int | QueryFail String
    deriving (Show, Read)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read)

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao      :: AcaoLog
    , detalhes  :: String
    , status    :: StatusLog
    } deriving (Show, Read)

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

auditoriaLogFile :: FilePath
auditoriaLogFile = "Auditoria.log"
