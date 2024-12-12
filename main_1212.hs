data Permission = ReadOnly | Editable deriving (Show, Eq)

data Sheet = Sheet {
    sheetName :: String,
    owner :: User,
    content :: [[Double]],
    permissionControl :: [(User, Permission)]
} deriving (Show)

data User = User {
    userName :: String
} deriving (Show, Eq)

createSheet :: String -> User -> Sheet
createSheet name owner =
    Sheet {
        sheetName = name,
        owner = owner,
        content = replicate 3 (replicate 3 0.0),
        permissionControl = [(owner, Editable)]
    }

createUser :: String -> User
createUser name = User { userName = name }

printSheet :: Sheet -> IO ()
printSheet sheet = do
    putStrLn $ "Sheet Name: " ++ sheetName sheet
    putStrLn $ "Owner: " ++ userName (owner sheet)
    putStrLn "Content: "
    mapM_ (putStrLn . unwords . map show) (content sheet)

updateCell :: Sheet -> Int -> Int -> Double -> Sheet
updateCell sheet row col value =
    let updatedContent = updateList (content sheet) row (updateList (content sheet !! row) col value)
    in sheet { content = updatedContent }

updateList :: [a] -> Int -> a -> [a]
updateList xs index newVal =
    take index xs ++ [newVal] ++ drop (index + 1) xs

hasPermission :: User -> Sheet -> Permission -> Bool
hasPermission user sheet requiredPermission =
    case lookup user (permissionControl sheet) of
        Just permission -> permission == requiredPermission || permission == Editable
        Nothing -> False

shareSheet :: User -> Sheet -> User -> Permission -> Sheet
shareSheet ownerUser sheet otherUser permission =
    if ownerUser == owner sheet
    then sheet { permissionControl = (otherUser, permission) : permissionControl sheet }
    else sheet

data Config = Config {
    enablePermission :: Bool,
    enableSharing :: Bool
}

updateCellWithConfig :: Config -> User -> Sheet -> Int -> Int -> Double -> Maybe Sheet
updateCellWithConfig config user sheet row col value
    | enablePermission config && not (hasPermission user sheet Editable) = Nothing
    | otherwise = Just (updateCell sheet row col value)

mainMenu :: Config -> User -> Sheet -> IO ()
mainMenu config user sheet = do
    putStrLn "\nMenu:"
    putStrLn "1. Print sheet"
    putStrLn "2. Update cell"
    putStrLn "3. Exit"
    putStr "Choose an option: "
    option <- getLine
    case option of
        "1" -> do
            printSheet sheet
            mainMenu config user sheet
        "2" -> do
            putStr "Enter row: "
            row <- readLn
            putStr "Enter column: "
            col <- readLn
            putStr "Enter new value: "
            value <- readLn
            case updateCellWithConfig config user sheet row col value of
                Just updatedSheet -> do
                    putStrLn "Cell updated successfully!"
                    mainMenu config user updatedSheet
                Nothing -> do
                    putStrLn "Permission denied!"
                    mainMenu config user sheet
        "3" -> putStrLn "Exiting..."
        _   -> do
            putStrLn "Invalid option!"
            mainMenu config user sheet

main :: IO ()
main = do
    let user = createUser "allen"
    let sheet = createSheet "SheetA" user
    let config = Config { enablePermission = True, enableSharing = True }
    mainMenu config user sheet
