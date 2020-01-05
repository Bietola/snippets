uSplitLines = doIt . snd . break isLineTerminator 
  where isLineTerminator c = c == '\n' || c == '\r'

        doIt ('')
