import System.IO
import LogParser
       
main = do input <- getContents
          case parseLog input "input" of
            Left err -> print err
            Right value -> print value
          
          
