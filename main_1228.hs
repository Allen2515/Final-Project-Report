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

-- 创建用户
createUser :: String -> User
createUser name = User { userName = name }

-- 创建工作表
createSheet :: String -> User -> Sheet
createSheet name owner =
    Sheet {
        sheetName = name,
        owner = owner,
        content = replicate 3 (replicate 3 0.0),
        permissionControl = [(owner, Editable)]
    }

-- 打印工作表内容
printSheet :: Sheet -> IO ()
printSheet sheet = do
    putStrLn $ "Sheet Name: " ++ sheetName sheet
    putStrLn $ "Owner: " ++ userName (owner sheet)
    putStrLn "Content: "
    mapM_ (putStrLn . unwords . map show) (content sheet)

-- 更新单元格的值
updateCell :: Sheet -> Int -> Int -> Double -> Sheet
updateCell sheet row col value =
    let updatedContent = updateList (content sheet) row (updateList (content sheet !! row) col value)
    in sheet { content = updatedContent }

-- 更新列表中的元素
updateList :: [a] -> Int -> a -> [a]
updateList xs index newVal =
    take index xs ++ [newVal] ++ drop (index + 1) xs

-- 检查用户权限
hasPermission :: User -> Sheet -> Permission -> Bool
hasPermission user sheet requiredPermission =
    case lookup user (permissionControl sheet) of
        Just permission -> permission == requiredPermission || permission == Editable
        Nothing -> False

-- 更改访问权限
changeAccessRight :: User -> Sheet -> User -> Permission -> Sheet
changeAccessRight ownerUser sheet otherUser newPermission =
    if ownerUser == owner sheet
    then sheet { permissionControl = (otherUser, newPermission) : filter ((/= otherUser) . fst) (permissionControl sheet) }
    else sheet

-- 菜单实现
mainMenu :: [User] -> [Sheet] -> IO ()
mainMenu users sheets = do
    putStrLn "\n---------------Menu---------------"
    putStrLn "1. Create a user"
    putStrLn "2. Create a sheet"
    putStrLn "3. Check a sheet"
    putStrLn "4. Change a value in a sheet"
    putStrLn "5. Change a sheet's access right"
    putStrLn "6. Collaborate with another user"
    putStrLn "7. Exit"
    putStrLn "choose an option"
    option <- getLine
    case option of
        "1" -> do
            putStr "Enter new user's name:\n"
            newName <- getLine
            let newUser = createUser newName
            putStrLn $ "User " ++ newName ++ " created!"
            mainMenu (newUser : users) sheets
        "2" -> do
            putStrLn "Available users:"
            mapM_ (putStrLn . userName) users
            putStr "Enter owner's name:\n"
            ownerName <- getLine
            case filter (\u -> userName u == ownerName) users of
                (owner:_) -> do
                    putStr "Enter sheet name:\n"
                    inputSheetName <- getLine
                    let newSheet = createSheet inputSheetName owner
                    putStrLn $ "Create a sheet named " ++ inputSheetName ++ " for " ++ ownerName
                    mainMenu users (newSheet : sheets)
                [] -> do
                    putStrLn "Owner not found!"
                    mainMenu users sheets
        "3" -> do
            putStrLn "Available sheets:"
            mapM_ (putStrLn . sheetName) sheets
            putStr "Enter sheet name to check: "
            inputSheetName <- getLine
            case filter (\s -> sheetName s == inputSheetName) sheets of
                (sheet:_) -> do
                    printSheet sheet
                    mainMenu users sheets
                [] -> do
                    putStrLn "Sheet not found!"
                    mainMenu users sheets
        "4" -> do
            putStrLn "Available sheets:"
            mapM_ (putStrLn . sheetName) sheets
            putStr "Enter sheet name: "
            inputSheetName <- getLine
            case filter (\s -> sheetName s == inputSheetName) sheets of
                (sheet:_) -> do
                    putStrLn "Enter user's name for authorization:"
                    inputUserName <- getLine
                    case filter (\u -> userName u == inputUserName) users of
                        (user:_) -> do
                            putStr "Enter row:\n"
                            row <- readLn
                            putStr "Enter column:\n"
                            col <- readLn
                            putStr "Enter new value:\n"
                            value <- readLn
                            if hasPermission user sheet Editable
                            then do
                                let updatedSheet = updateCell sheet row col value
                                putStrLn "Cell updated successfully!"
                                mainMenu users (updatedSheet : filter (\s -> sheetName s /= inputSheetName) sheets)
                            else do
                                putStrLn "Permission denied!"
                                mainMenu users sheets
                        [] -> do
                            putStrLn "User not found!"
                            mainMenu users sheets
                [] -> do
                    putStrLn "Sheet not found!"
                    mainMenu users sheets
        "5" -> do
            putStrLn "Available sheets:"
            mapM_ (putStrLn . sheetName) sheets
            putStr "Enter sheet name:\n"
            inputSheetName <- getLine
            case filter (\s -> sheetName s == inputSheetName) sheets of
                (sheet:_) -> do
                    putStrLn "Enter owner's name:"
                    ownerName <- getLine
                    case filter (\u -> userName u == ownerName) users of
                        (owner:_) -> do
                            putStrLn "Enter user's name to change permission:"
                            targetUserName <- getLine
                            case filter (\u -> userName u == targetUserName) users of
                                (targetUser:_) -> do
                                    putStrLn "Enter new permission (ReadOnly/Editable):"
                                    permission <- getLine
                                    let newPermission = if permission == "Editable" then Editable else ReadOnly
                                    let updatedSheet = changeAccessRight owner sheet targetUser newPermission
                                    putStrLn "Permission updated successfully!"
                                    mainMenu users (updatedSheet : filter (\s -> sheetName s /= inputSheetName) sheets)
                                [] -> do
                                    putStrLn "User not found!"
                                    mainMenu users sheets
                        [] -> do
                            putStrLn "Owner not found!"
                            mainMenu users sheets
                [] -> do
                    putStrLn "Sheet not found!"
                    mainMenu users sheets
        "6" -> do
            putStrLn "!"
            mainMenu users sheets
        "7" -> putStrLn "Exiting..."
        _   -> do
            putStrLn "Invalid option!"
            mainMenu users sheets

main :: IO ()
main = do
    mainMenu [] []

