import Text.Read
import Eval (eval)
import Control.Monad

data Permission = ReadOnly | Editable deriving (Show, Eq)

data Sheet = Sheet {
    sheetName :: String,
    creator :: User,
    owners :: [User],
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
createSheet name creator =
    Sheet {
        sheetName = name,
        creator = creator,
        owners = [creator],
        content = replicate 3 (replicate 3 0.0),
        permissionControl = [(creator, Editable)]
    }

-- 打印工作表内容
printSheet :: Sheet -> IO ()
printSheet sheet = do
    putStrLn $ "Sheet Name: " ++ sheetName sheet
    putStrLn $ "Creator: " ++ userName (creator sheet)
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

-- 檢查是否有可以分享的權限 i.e. 是否為 owner
ownsSheet :: User -> Sheet -> Bool
ownsSheet user sheet = user `elem` owners sheet

-- 更改访问权限
changeAccessRight :: User -> Sheet -> User -> Permission -> Maybe Sheet
changeAccessRight changer sheet otherUser newPermission =
    if changer `ownsSheet` sheet
    then Just sheet { permissionControl = (otherUser, newPermission) : filter ((/= otherUser) . fst) (permissionControl sheet) }
    else Nothing

-- 分享 sheet i.e. 將 user 設為 owner
shareSheet :: User -> User -> Sheet -> Maybe Sheet
shareSheet sharer target sheet = if sharer == creator sheet
    then Just sheet { owners = target : owners sheet }
    else Nothing

-- menu item 1
promptCreateUser :: IO User
promptCreateUser = do
    putStr "Enter new user's name:\n"
    newName <- getLine
    let newUser = createUser newName
    putStrLn $ "User " ++ newName ++ " created!"
    return newUser

-- menu item 2
promptCreateSheet :: [User] -> IO (Maybe Sheet)
promptCreateSheet users = do
    putStrLn "Available users:"
    mapM_ (putStrLn . userName) users
    putStr "Enter creator's name:\n"
    creatorName <- getLine
    case filter (\u -> userName u == creatorName) users of
        (creator:_) -> do
            putStr "Enter sheet name:\n"
            inputSheetName <- getLine
            let newSheet = createSheet inputSheetName creator
            putStrLn $ "Create a sheet named " ++ inputSheetName ++ " for " ++ creatorName
            return (Just newSheet)
        [] -> do
            putStrLn "Creator not found!"
            return Nothing

-- menu item 3
promptCheckSheet :: [User] -> [Sheet] -> IO ()
promptCheckSheet users sheets = do
    putStrLn "Available sheets:"
    mapM_ (putStrLn . sheetName) sheets
    putStr "Enter sheet name to check: "
    inputSheetName <- getLine
    case filter (\s -> sheetName s == inputSheetName) sheets of
        (sheet:_) -> printSheet sheet
        [] -> putStrLn "Sheet not found!"

-- menu item 4
promptUpdateSheet :: Bool -> [User] -> [Sheet] -> IO [Sheet]
promptUpdateSheet enablePermission users sheets = do
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
                    value <- getLine
                    case eval value of
                        Just newVal ->
                            if not enablePermission || hasPermission user sheet Editable
                            then do
                                let updatedSheet = updateCell sheet row col newVal
                                putStrLn "Cell updated successfully!"
                                return (updatedSheet : filter (\s -> sheetName s /= inputSheetName) sheets)
                            else do
                                putStrLn "Permission denied!"
                                return sheets
                        Nothing -> do
                            putStrLn "Can't evaluate expression!"
                            return sheets
                [] -> do
                    putStrLn "User not found!"
                    return sheets
        [] -> do
            putStrLn "Sheet not found!"
            return sheets            
                    
-- 菜单实现
mainMenu :: Bool -> [User] -> [Sheet] -> IO ()
-- permission and share feature enabled
mainMenu True users sheets = do
    putStrLn "\n---------------Menu---------------"
    putStrLn "1. Create a user"
    putStrLn "2. Create a sheet"
    putStrLn "3. Check a sheet"
    putStrLn "4. Change a value in a sheet"
    putStrLn "5. Change a sheet's access right"
    putStrLn "6. Collaborate with another user"
    putStrLn "7. Exit"
    putStrLn "*. Turn off permission and share feature"
    putStrLn "choose an option"
    option <- getLine
    case option of
        "1" -> do
            newUser <- promptCreateUser
            mainMenu True (newUser : users) sheets
        "2" -> do
            newSheet <- promptCreateSheet users
            case newSheet of 
                Just sheet -> mainMenu True users (sheet : sheets)
                Nothing -> mainMenu True users sheets
        "3" -> do
            promptCheckSheet users sheets
            mainMenu True users sheets        
        "4" -> do
            updatedSheet <- promptUpdateSheet True users sheets
            mainMenu True users updatedSheet
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
                                    case changeAccessRight owner sheet targetUser newPermission of
                                        Just updatedSheet -> do
                                            putStrLn "Permission updated successfully!"
                                            mainMenu True users (updatedSheet : filter (\s -> sheetName s /= inputSheetName) sheets)
                                        Nothing -> do
                                            putStrLn ("User " ++ userName owner ++ " doesn't own " ++ sheetName sheet)
                                            mainMenu True users sheets
                                [] -> do
                                    putStrLn "User not found!"
                                    mainMenu True users sheets
                        [] -> do
                            putStrLn "Owner not found!"
                            mainMenu True users sheets
                [] -> do
                    putStrLn "Sheet not found!"
                    mainMenu True users sheets
        "6" -> do
            putStrLn "Available sheets:"
            mapM_ (putStrLn . sheetName) sheets
            putStr "Enter sheet name: "
            inputSheetName <- getLine
            case filter (\s -> sheetName s == inputSheetName) sheets of
                (sheet:_) -> do
                    putStrLn "Enter creator's name:"
                    creatorName <- getLine
                    case filter (\u -> userName u == creatorName) users of
                        (creator:_) -> do
                            putStrLn "Enter user's name to share:"
                            targetUserName <- getLine
                            case filter (\u -> userName u == targetUserName) users of
                                (targetUser:_) -> do
                                    case shareSheet creator targetUser sheet of
                                        Just newSheet -> do
                                            putStrLn (userName creator ++ " shares sheet " ++ sheetName sheet ++ " with " ++ userName targetUser)
                                            mainMenu True users (newSheet : filter (\s -> sheetName s /= inputSheetName) sheets)
                                        Nothing -> do
                                            putStrLn ("User " ++ userName creator ++ " isn't the creator of " ++ sheetName sheet)
                                            mainMenu True users sheets
                                [] -> do
                                    putStrLn "User not found!"
                                    mainMenu True users sheets
                        [] -> do
                            putStrLn "Creator not found!"
                            mainMenu True users sheets
                [] -> do
                    putStrLn "Sheet not found!"
                    mainMenu True users sheets
        "7" -> putStrLn "Exiting..."
        "*" -> mainMenu False users sheets
        _   -> do
            putStrLn "Invalid option!"
            mainMenu True users sheets

mainMenu False users sheets = do
    putStrLn "\n---------------Menu---------------"
    putStrLn "1. Create a user"
    putStrLn "2. Create a sheet"
    putStrLn "3. Check a sheet"
    putStrLn "4. Change a value in a sheet"
    putStrLn "5. Exit"
    putStrLn "*. Turn on permission and share feature"
    putStrLn "choose an option"
    option <- getLine
    case option of
        "1" -> do
            newUser <- promptCreateUser
            mainMenu False (newUser : users) sheets
        "2" -> do
            newSheet <- promptCreateSheet users
            case newSheet of 
                Just sheet -> mainMenu False users (sheet : sheets)
                Nothing -> mainMenu False users sheets
        "3" -> do
            promptCheckSheet users sheets
            mainMenu False users sheets        
        "4" -> do
            updatedSheet <- promptUpdateSheet False users sheets
            mainMenu False users updatedSheet
        "5" -> putStrLn "Exiting..."
        "*" -> mainMenu True users sheets
        _   -> do
            putStrLn "Invalid option!"
            mainMenu False users sheets
            
main :: IO ()
main = do
    mainMenu True [] []

