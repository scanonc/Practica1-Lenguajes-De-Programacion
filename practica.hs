import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)


-- Main principal
main :: IO ()
main = do
    -- Cargar el registro desde el archivo
    admisiones <- cargarRegistro
    putStrLn "¡Bienvenido a Admisiones!"

    -- Ciclo principal del programa
    cicloPrincipal admisiones

-- Definición de datos para estudiantes
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read)

-- Función para el ciclo principal del programa (menu)
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal admisiones = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let admisionesActualizadas = registrarCheckIn idEst tiempoActual admisiones
            putStrLn $ "Estudiante con ID " ++ idEst ++ " registrado en la universidad."
            guardarRegistro admisionesActualizadas
            cicloPrincipal admisionesActualizadas

        "2" -> do
            putStrLn "Ingrese el ID del estudiante que sale:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let admisionesActualizadas = registrarCheckOut idEst tiempoActual admisiones
            putStrLn $ "Estudiante con ID " ++ idEst ++ " salió de la universidad."
            guardarRegistro admisionesActualizadas
            cicloPrincipal admisionesActualizadas

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEst <- getLine
            case buscarEstudiante idEst admisiones of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ idEst ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal admisiones

        "4" -> do
            listarEstudiantes admisiones
            cicloPrincipal admisiones

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal admisiones

-- Función para registrar el ingreso de un estudiante a la universidad (requisito funcional 1)
registrarCheckIn :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarCheckIn id tiempo admisiones =
    Estudiante id tiempo Nothing : admisiones

-- Función para buscar un estudiante por su ID (requisito funcional 2)
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id admisiones =
    find (\e -> id == idEstudiante e && isNothing (salida e)) admisiones
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo (requerimiento funcional 3)
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para listar los estudiantes (requisito funcional 4)
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función para registrar la salida de un estudiante de la universidad (requisito funcional 5)
registrarCheckOut :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarCheckOut id tiempo admisiones =
    map (\e -> if id == idEstudiante e then e { salida = Just tiempo } else e) admisiones

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarRegistro :: [Estudiante] -> IO ()
guardarRegistro admisiones = do
    withFile "University.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map show admisiones))
    putStrLn "Registro guardado en University.txt."


-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarRegistro :: IO [Estudiante]
cargarRegistro = do
    contenido <- withFile "University.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id entrada salida) =
    "Estudiante {id = \"" ++ id ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"