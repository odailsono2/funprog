removeNonUppercase :: [Char]->[Char]

removeNonUppercase st = [c | c<-st, c `elem` ['A' .. 'Z']]

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe 6 = "Six!"
sayMe x = "Not between 1 and 6"



