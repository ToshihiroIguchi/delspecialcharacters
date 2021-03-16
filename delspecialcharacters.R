#caretのformulaで特殊文字を入力すると不具合が生じるので削除する関数
delspecialcharacters <- function(vec, o.num = TRUE){
  
  #vecは置換対象のベクトル
  #先頭に数字が来たら"O(オー)"をつける。
  
  #xのベクトルで、hexではじまるunicodeをvecの文字に変換
  conv.hex.chr <- function(x, hex, vec){
    
    #https://qiita.com/hoxo_m/items/82ad2493f73d20648b1c
    #十六進数を数値に変換
    first.hex.num <- strtoi(hex, base = 16)
    
    #数値のベクトルを作成
    hex.num <- first.hex.num + c(0 : (length(vec) - 1))
    
    #16進数で再生
    hex.vec <- as.character(as.hexmode(hex.num))
    
    #unicodeなので先頭に付け足す
    hex.vec <- paste0("0x", hex.vec)
    
    #戻り値に初期値を入れておく
    ret <- x
    
    #unicodeで順に変換していく
    for(i in 1:length(vec)){
      #https://stackoverflow.com/questions/66639163/convert-from-hexadecimal-to-unicode-in-r-language/66639415#66639415
      ret <- gsub(pattern = intToUtf8(hex.vec[i]) , replacement = vec[i], x = ret)
    }
    
    #戻り値
    return(ret)
    
  }
  
  #hexではじまるunicodeをn個の文字に変換
  make.hex.chr <- function(hex, n = 1){
    
    #https://qiita.com/hoxo_m/items/82ad2493f73d20648b1c
    #十六進数を数値に変換
    first.hex.num <- strtoi(hex, base = 16)
    
    #数値のベクトルを作成
    hex.num <- first.hex.num + c(0 : (n - 1))
    
    #16進数で再生
    hex.vec <- as.character(as.hexmode(hex.num))
    
    #unicodeなので先頭に付け足す
    hex.vec <- paste0("0x", hex.vec)
    
    #戻り値
    ret <- rep(NA, n)
    
    #それぞれ戻り値に変換
    for(i in 1:length(hex.vec)){
      #UTF8に変換
      ret[i] <- intToUtf8(hex.vec[i])
    }
    
    
    #戻り値
    return(ret)
    
  }

  
  #置換用の関数
  gsub2 <- function(x, pattern, replacement = ""){
    return(gsub(pattern, replacement, x))
  }
  
  #戻り値を定義
  ret <- vec
  
  #特殊文字を削除
  ret <- gsub2(ret, "\r\n|\n|\r")
  
  #特殊文字を置換
  ret <- gsub2(ret, "@", "at")
  ret <- gsub2(ret, "\uFF20", "at")
  ret <- gsub2(ret, "+")
  ret <- gsub2(ret, "-")
  
  #全角数字を変換
  ret <- conv.hex.chr(ret, hex = "FF10", c(0:9))
  
  #丸の中の数字を変換(1-20)
  ret <- conv.hex.chr(ret, hex = "2460", c(1:20))
  
  
  #上付き数字を変換
  #https://unicode-table.com/jp/sets/superscript-and-subscript-letters/
  ret <- conv.hex.chr(ret, hex = "2070", c(0:9))
  
  #添え字の数字を変換
  #https://unicode-table.com/jp/sets/superscript-and-subscript-letters/
  ret <- conv.hex.chr(ret, hex = "2080", c(0:9))


  #ギリシャ文字を変換
  ret <- gsub2(ret, "\u03B1", "a")
  ret <- gsub2(ret, "\u03B2", "beta")
  ret <- gsub2(ret, "\u03B3", "gamma")
  ret <- gsub2(ret, "\u03B4", "delta")
  ret <- gsub2(ret, "\u03B5", "epsilon")
  ret <- gsub2(ret, "\u03B6", "zeta")
  ret <- gsub2(ret, "\u03B7", "eta")
  ret <- gsub2(ret, "\u03B8", "theta")
  ret <- gsub2(ret, "\u03B9", "iota")
  ret <- gsub2(ret, "\u03BA", "kappa")
  ret <- gsub2(ret, "\u03BB", "lambda")
  ret <- gsub2(ret, "\u03BC", "u")
  ret <- gsub2(ret, "\u03BD", "V")
  ret <- gsub2(ret, "\u03BE", "xi")
  ret <- gsub2(ret, "\u03BF", "o")
  ret <- gsub2(ret, "\u03C1", "pi")
  ret <- gsub2(ret, "\u03C2", "rho")
  ret <- gsub2(ret, "\u03C3", "sigma")
  ret <- gsub2(ret, "\u03C4", "tau")
  ret <- gsub2(ret, "\u03C5", "u")
  ret <- gsub2(ret, "\u03C6", "phi")
  ret <- gsub2(ret, "\u03C7", "x")
  ret <- gsub2(ret, "\u03c8", "psi")
  ret <- gsub2(ret, "\u03C9", "omega")
  
  ret <- gsub2(ret, "\u0391", "A")
  ret <- gsub2(ret, "\u0392", "B")
  ret <- gsub2(ret, "\u0393", "Gamma")
  ret <- gsub2(ret, "\u0394", "Delta")
  ret <- gsub2(ret, "\u0395", "E")
  ret <- gsub2(ret, "\u0396", "Z")
  ret <- gsub2(ret, "\u0397", "H")
  ret <- gsub2(ret, "\u0398", "Theta")
  ret <- gsub2(ret, "\u0399", "I")
  ret <- gsub2(ret, "\u039A", "K")
  ret <- gsub2(ret, "\u039B", "Lambda")
  ret <- gsub2(ret, "\u039C", "M")
  ret <- gsub2(ret, "\u039D", "N")
  ret <- gsub2(ret, "\u039E", "Xi")
  ret <- gsub2(ret, "\u039F", "O")
  ret <- gsub2(ret, "\u03A1", "Pi")
  ret <- gsub2(ret, "\u03A2", "P")
  ret <- gsub2(ret, "\u03A3", "Sigma")
  ret <- gsub2(ret, "\u03A4", "T")
  ret <- gsub2(ret, "\u03A5", "Y")
  ret <- gsub2(ret, "\u03C6", "Phi")
  ret <- gsub2(ret, "\u03C7", "X")
  ret <- gsub2(ret, "\u03c8", "Psi")
  ret <- gsub2(ret, "\u03C9", "Omega")
  
  
  #特殊文字
  ret <- gsub2(ret, "\u2103", "C")
  ret <- gsub2(ret, "\u2116", "No")
  
  #ローマ数字を変換
  ret <- conv.hex.chr(ret, hex = "2160", c(1:12))
  
  #カッコつき漢数字を変換
  ret <- conv.hex.chr(ret, hex = "3220", make.hex.chr("4E00", n = 10))
  
  
  #漢字、ひらがな、カタカナ、数字、アルファベット大文字小文字以外を削除
  #https://qiita.com/grrrr/items/0b35b5c1c98eebfa5128
  #https://so-zou.jp/software/tech/programming/tech/regular-expression/meta-character/variable-width-encoding.htm#no1
  #サロゲートペアは含めない
  ret <- gsub2(ret, "[^\u3041-\u3096\u30A1-\u30FA\u3400-\u9FFF\uF900-\uFAFF0-9a-zA-Z\u30FC]", "")
  
  #先頭に数字がくる場合の処理
  if(o.num){

    #先頭に数字がくるベクトル中の位置
    num.pos <- grep("^[0-9]", ret)
    
    #先頭に数字がくる場合、"O"(オー)"をつける。
    ret[num.pos] <- paste0("O", ret[num.pos])
    
  }
  
  #戻り値
  return(ret)
}

