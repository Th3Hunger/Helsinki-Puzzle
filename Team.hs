import System.Random
import System.IO.Unsafe

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

deletesub lst = filter (`notElem` lst)

users= ["user1", "user2", "user3", "user4"]
items = ["item1", "item2", "item3", "item4","item5", "item6"]
purchasesHistory = [("user1", [["item1", "item2", "item3"] , ["item1", "item2", "item4"]]) , ("user2", [["item2", "item5"], ["item4", "item5"]]), ("user3", [["item3", "item2"]]), ("user4", [])]
 
------------------------------------------------------------------------------------------
createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = (x,[]):createEmptyFreqList xs
-------------------------------------------------------------------------------------------
rem0 str l1 = rem_h str l1 []
rem_h str [] acc = acc
rem_h str (h:t) acc = if (str ==h) then rem_h str t acc else rem_h str  t (acc ++ [h])
 
count list  = ( (fst list) ,(uniq []   (count_h ( snd list) (snd list) 0 [] (snd list)) ))
count_h  list [] counter acc tmp = acc
count_h  [] (h2:t2) counter acc tmp = count_h tmp t2 0 ((h2,counter) : acc) tmp
count_h (h:t) (h2:t2) counter acc tmp = if(h2==h) then count_h t (h2:t2) (counter+1) acc tmp else count_h t (h2:t2) counter acc tmp

	
uniq :: Eq a => [a] -> [a] -> [a]
uniq x [] = x 
uniq [] (a:xs) = uniq [a] xs
uniq x (a:xs) = if a `elem` x then uniq x xs else uniq (a:x) xs

 


getAllUsersStats list  = all_h  (createEmptyFreqList items) list [] (createEmptyFreqList items) []
all_h  l1 [] acc tmp fl = (reverse fl)
all_h   []  (h2:t2) acc tmp fl = all_h tmp t2  [] tmp ([(fst h2,(reverse acc))] ++ fl)
all_h  (h:t) (h2:t2) acc tmp  fl =   all_h t (h2:t2)  ((count (cancount (fst h) (snd h2))): acc) tmp fl
 


cancount str list = (str,rem0 str (cancount_h str list []) )
cancount_h str [] acc = acc
cancount_h str (h:t) acc = if (elem str h) then cancount_h str t (acc ++ h) else cancount_h str  t acc

-------------------------------------------------------------------------------------------
nth ind list = nthh ind list 0
nthh ind  [] c = error "index bound of exception"
nthh ind (x:xs) c = if (ind == c) then  x else nthh ind xs (c+1)
empty [] = True 
empty list = False 

flatList l1 = concat (flatlisth l1)
flatlisth l1 = flatlist_h l1 []
 where
flatlist_h [] acc = acc
flatlist_h (x:xs) acc  = flatlist_h xs  (acc ++ [snd x])


countitems list =  (cth items list [] [] list)
 where
cth [] list  final acc  temp = final
cth (x1:xs1) [] final acc  temp = cth xs1 temp  ((x1,sum(acc)) : final) [] temp
cth (x1:xs1) (x2:xs2) final acc  temp = if(x1 == (fst x2 )) then cth (x1:xs1) xs2  final ( acc ++[snd x2] )  temp else cth (x1:xs1) xs2 final acc  temp

removezero list = removezeroh list [] 
removezeroh [] acc = acc
removezeroh (x:xs) acc = if (snd x == 0) then removezeroh xs acc else removezeroh xs ([(fst x, snd x)] ++ acc)

freqListItems str  = freqList_H str (getAllUsersStats purchasesHistory) 
freqList_H str [] = []
freqList_H str (x:xs)  = if (str == fst x) then removezero (countitems (flatList (snd x))) else freqList_H str xs 
 
tester str list = testerh str list list []
testerh [] listt tmp acc = acc
testerh (h:t) [] tmp acc = testerh (h:t) tmp tmp acc 
testerh (h:t) (x:xs) tmp acc =  if ( h == fst x ) then  testerh t  xs tmp ( (snd x) : acc) else testerh (h:t) xs tmp acc

carthelper str list = removezero (countitems (concat (tester str list)))


freqListCart user str = fcarth user str (getAllUsersStats purchasesHistory) 
fcarth user str [] = []
fcarth user str (x:xs) = if (user == fst x) then carthelper str (snd x) else fcarth user str xs

freqListCartAndItems str list = removezero ( countitems (freqListItems str ++  freqListCart str list))
 
  
------------------------------------------------- freqListUsers should be----------------------------------------------------
freqListUsers str = freqListUsers_helper (freqpur str)
freqListUsers_helper list = removezero (countitems ( flatList (concat list ) ) ) 

freqpur str = freqpur_h str (getAllUsersStats purchasesHistory) (getAllUsersStats purchasesHistory)
freqpur_h str [] tmp = [] 
freqpur_h str (h:t) tmp = if (str == (fst h)) then  (purchaesIntersection (snd h) (deletesub [h] tmp )) else freqpur_h str t tmp
-----------------------------------------------------------------------------------------------

tuple list = tupleh list [] 
tupleh  [] acc = acc
tupleh (x:xs) acc = tupleh xs ( acc ++ [replicate (fromIntegral ((snd (x)))) ([fst (x)])]  ) 

----------------------------------------------------------------------------------------------------------------------------------------------
rec_empty_helper  str = rec_empty_helper1 (freqListItems str)
rec_empty_helper1 list = concat (concat (tuple list))

recommendEmptyCart user =  recommendEmptyCart_helper  (rec_empty_helper user) (length (rec_empty_helper user))
recommendEmptyCart_helper list len = if (empty list) then "" else (nth (randomZeroToX (fromIntegral (len -1) ) ) list)
----------------------------------------------------------------------------------------------------------------------------------------------
rec_IT str list =  rec_IT_helper (freqListCartAndItems str list)
rec_IT_helper list = concat (concat (tuple list))

recommendBasedOnItemsInCart user list = recommendBasedOnItemsInCart_helper ( rec_IT user list) (length (rec_IT user list))
recommendBasedOnItemsInCart_helper list len = if (empty list) then "" else(nth (randomZeroToX (fromIntegral (len -1) ) ) list)
----------------------------------------------------------------------------------------------------------------------------------------------

rec_use str = rec_use_helper (freqListUsers str)
rec_use_helper list = concat (concat (tuple list))	

recommendBasedOnUsers user = recommendEmptyCart_helper  (rec_use  user) (length (rec_use  user))
recommendBasedOnUsers_helper list len =if (empty list) then "" else (nth (randomZeroToX (fromIntegral (len -1) ) ) list)

----------------------------------------------------------------------------------------------------------------------------------------------

recommend str list = if (empty (list) ==True) then (recEmptyHe items) else last_recommend_helper str list 2

last_recommend_helper str list len = (nth (randomZeroToX (fromIntegral (len -1) ) ) (recohelper str list) )

recohelper str list  =  ([recommendBasedOnItemsInCart str list ] ++ [recommendBasedOnUsers str] )


recEmptyHe list  = recEmptyHe_helper (list) (length items)
recEmptyHe_helper list len = (nth (randomZeroToX (fromIntegral (len -1) ) ) list)
-----------------------------------------------------------------------------------------------------------------------------------------------
 
-------------------------------------------Purchase Intersection -----------------------------------------------
intersect1 tuple1 tuple2 =  removezero (countitems ((snd tuple1) ++ (snd tuple2))) 

intersect2 l1 l2 = intersect2_Helper l1 (snd l2) []
intersect2_Helper   [] [] acc = acc
intersect2_Helper (h:t) (h2:t2) acc =  if ( empty (snd h) == True || empty (snd h2)==True ) then intersect2_Helper t t2 acc
                                      else intersect2_Helper t t2  (acc ++ [(fst (h),(intersect1 h h2))] )


purchaesIntersection l1 l2 = intersect3_Helper l1 l2 []
intersect3_Helper l1 [] acc =acc 
intersect3_Helper l1 (h2:t2) acc = intersect3_Helper l1 t2 (acc ++ [(intersect2 l1 h2)])

--------------------------------------------------------------------------------------------------------------------------------------------------------- 

























