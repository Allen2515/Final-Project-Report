# Final-Project-Report

欠缺功能:
1. change value need to support rational arithmetic operations (OK, +-*/ no space)
2. collaborate with antoher user (OK)
3. 權限(是否需要增加密碼功能) (感覺不用)
4. 錯誤輸入的反饋或提醒（現在看起來挺完整的，我看每個 branch 都有訊息）

新增：
permission 與 collaborate 的開關（dynamically），由作業說明中 `Furthermore, please make the requirements 5. & 6. can be easily switched on/off.` 要求。

## Dependency
- parsec package: https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec.html

## Vocabulary
以下對一些作業說明中不清楚或者我們自己使用的名詞進行定義：
- creator：sheet 的一個屬性。表示這個 sheet 最開始是由哪個 user 創的。
- owners：sheet 的一個 list 屬性。在 list 內的 user 可以改變這個 sheet 對任意 user 的讀寫權限。在權限功能關閉時無法執行此操作。注意 creator 一定在 owners 中。
- 讀寫權限：指名一個 user 對於一個 sheet 是否可寫。注意讀寫權限與 owners 並無關聯（即使是 sheet owner 也可能是只讀）。在權限功能關閉時，所有 user 都能寫入任意 sheet。
- collaborate：作業說明中稱為 share。Collaborate 指的是將指定的 user 加入到指定 sheet 的 owners 之中。只有 sheet 的 creator 能夠操作。collaborate 功能關閉時，無法執行此操作。


測試方式:
---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
1
Enter new user's name:
allen
User allen created!

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
2
Available users:
allen
Enter owner's name:
allen
Enter sheet name:
A
Create a sheet named A for allen

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
3
Available sheets:
A
A 
Enter sheet name to check: Sheet Name: A
Owner: allen
Content: 
0.0 0.0 0.0
0.0 0.0 0.0
0.0 0.0 0.0

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
4
Available sheets:
A
A
Enter sheet name: Enter user's name for authorization:
allen
Enter row:
1
Enter column:
1
Enter new value:
5.5
Cell updated successfully!

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
3
Available sheets:
A
A
Enter sheet name to check: Sheet Name: A
Owner: allen
Content: 
0.0 0.0 0.0
0.0 5.5 0.0
0.0 0.0 0.0

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
5
Available sheets:
A
Enter sheet name:
ii
Sheet not found!

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
1
Enter new user's name:
alice
User alice created!

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
5
Available sheets:
A
Enter sheet name:
A
Enter owner's name:
allen
Enter user's name to change permission:
alice
Enter new permission (ReadOnly/Editable):
ReadOnly
Permission updated successfully!

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
4
Available sheets:
A
A
Enter sheet name: Enter user's name for authorization:
alice
Enter row:
1
Enter column:
2
Enter new value:
9.9
Permission denied!

---------------Menu---------------
1. Create a user
2. Create a sheet
3. Check a sheet
4. Change a value in a sheet
5. Change a sheet's access right
6. Collaborate with another user
7. Exit
choose an option
7
Exiting...
