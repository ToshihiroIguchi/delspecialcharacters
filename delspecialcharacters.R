#特殊文字を削除
#ただし、漢字は直接使用しない

#caretのformulaで特殊文字を入力すると不具合が生じるので削除する関数
delspecialcharacters <- function(vec){
  
  #vecは置換対象のベクトル
  #先頭に数字が来たら"O(オー)"をつける。
  
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
  
  #漢字、ひらがな、カタカナ、数字、アルファベット大文字小文字以外を削除
  #https://qiita.com/grrrr/items/0b35b5c1c98eebfa5128
  #https://so-zou.jp/software/tech/programming/tech/regular-expression/meta-character/variable-width-encoding.htm#no1
  #サロゲートペアは含めない
  ret <- gsub2(ret, "[^\u3041-\u3096\u30A1-\u30FA\u3400-\u9FFF\uF900-\uFAFF0-9a-zA-Z\u30FC]", "")
  
  #先頭に数字がくるベクトル中の位置
  num.pos <- grep("^[0-9]", ret)
  
  #先頭に数字がくる場合、"O"(オー)"をつける。
  ret[num.pos] <- paste0("O", ret[num.pos])
  
  #戻り値
  return(ret)
}



