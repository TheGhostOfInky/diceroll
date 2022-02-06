import System.Environment (getArgs)   
import Data.List (sort)
computelist :: (Int, [Int],[[Int]]) -> IO () -- Creates list out of list of positions 
computelist (x,l,m) = do 
    let list = [1,2,3,4,5,6] -- List of possible combinations
    let newa = [list!!(l!!0),list!!(l!!1),list!!(l!!2),list!!(l!!3),list!!(l!!4),list!!(l!!5),list!!(l!!6),list!!(l!!7)] -- Creates list from list combinations and positions
    if sum newa == x -- Checks if sum of elements of list matches x
        then print newa -- Prints output if it does
        else return () -- Returns if it doesn't
    if sum l == 8*(length list -1) -- Checks if list has reached the end of the possibilities
        then return() -- Returns if it does
        else do
            let newlist = nextList(l,m,length list -1,7) -- Creates a new list if possibilities are available
            computelist(x,newlist,addM(newlist,m)) -- Recurses function with new list and adds the list of the list of lists

lastN :: Int -> [a] -> [a] -- Returns last n elements of list
lastN n xs = drop (length xs - n) xs

incrementX :: ([Int],Int) -> [Int] -- Increments i element of list by 1
incrementX (l,i) = take i l ++ [l!!i + 1] ++ lastN (7-i) l

resetM :: ([Int],Int) -> [Int] -- Sets the i element of list to 0
resetM (l,i) = take i l ++ [0] ++ lastN (7-i) l

addM :: ([Int], [[Int]]) -> [[Int]] -- Adds list of integers to list of lists of integers
addM (l,m) = m ++ [l]

checkRep :: ([Int],[[Int]]) -> Bool -- Checks if list is in the list of lists
checkRep (l,m) = do
    if elem l m 
        then False 
        else True

incrementM :: ([Int], Int, Int) -> [Int] -- Returns an incremented matrix 
incrementM (l,i,len) = do
    if l!!i < len -- Checks if element i can be incremented
        then incrementX(l,i) -- Increments if it can
        else if i > 0 --checks if i is grester than zero
            then incrementM(resetM(l,i),i-1,len) -- Recurses to increment matrix on the i value above if it is
            else [len,len,len,len,len,len,len,len] -- Else returns the list of the final values


nextList :: ([Int], [[Int]], Int, Int) -> [Int] --Returns a new unique matrix
nextList (l,m,len,n) = do 
    let new = incrementM(l,n,len) -- Creates a new list that has an incremented value
    if checkRep(sort(new),m) -- Checks if list is already in list of lists
        then new -- Returns the matrix if it is unique 
        else nextList(new,m,len,n) -- Recurses fucntion to generate new unique list

main :: IO ()
main = do
    let basem = [0,0,0,0,0,0,0,0] -- Sets base list as all 0's
    args <- getArgs -- Fetches command line arguments
    let len = if length args> 0 then read(args !! 0) :: Int else 20 -- Sets desired lenght to the first argument or defaults to 20
    computelist(len, basem,[basem]) -- Computes list with the input parameters