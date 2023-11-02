module DataStructure where

-- Data Structures for Intepreter
-- Arrays of Integer type

type Array = [Int]


-- Declararing an Array variable
-- input:  Dimension, we start counting from 0 like any programing languge
-- output: Array variable with the given dimension
declareArray ::  Int -> Array
declareArray 0 = []
declareArray dim = 0 : declareArray (dim-1)  


-- Returning an element from the given array, 
-- input:  Target array and target index position
-- output: Element from array in specific position
readElemArray :: Array -> Int -> Int
readElemArray [] _ = error "empty array,index out of range"
readElemArray arr i = arr !! i 


-- Insert an int element in the array
-- input:  Target array and element to be add to the array
-- output: Updated array variable
-- if index = 0, element is inserted in first position,
-- if index < 0, returning error
-- if index > 0, function executed in recursion       
insertElemArray :: Array -> Int -> Int -> Array
insertElemArray (a:arr) 0 el = el : arr
insertElemArray (a:arr) i el | i < 0 = error "negative index,index out of range"
                             | otherwise = a : insertElemArray arr (i-1) el       


