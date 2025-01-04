import System.Environment
import Data.List 

main = do 
    progName <- getProgName    
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c
	    putStrLn progName  
            main  
        else do
		putChar '\n'
		return ()  