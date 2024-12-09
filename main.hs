data Permission = ReadOnly | Editable deriving(Show, Eq)

data Sheet = Sheet {
    sheetName :: String,
    owner :: User,
    content :: [[Double]],
    permissionControl :: [(User, Permission)]
} deriving(Show)

data User = User {
    userName :: String
} deriving(Show, Eq)

    
createSheet :: String -> User -> Sheet
createSheet name owner =
    Sheet {
        sheetName = name,
        owner = owner,
        content = replicate 3 (replicate 3 0.0),
        permissionControl = [(owner, Editable)]
    }
    
createUser :: String -> User
createUser name =
    User {
        userName = name
    }

main :: IO()
main = do
    let user = createUser "allen"
    let sheet = createSheet "SheetA" user 
    print user
    print sheet