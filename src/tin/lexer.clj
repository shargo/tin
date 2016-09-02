(ns tin.lexer
  (:require
   [clojure.string :as str]
   [tin.common :as common])
  (:gen-class))

(defn token-regex
  [regex]
  (re-pattern (str #"^" "(" regex ")")))

(def token-regexes
  [
   ["NUMBER" (token-regex
              (re-pattern (str #"(?:0[xX][0-9a-fA-F]+)|"
                               #"(?:[0-9]+(\.[0-9]+)?(?:e[+-]?[0-9]+)?)")))]
   ["STRING" (token-regex #"\"[^\"\\]*(?:\\.[^\"\\]*)*\"")]
   ["CODE_STRING" (token-regex #"`[^`\\]*(?:\\.[^`\\]*)*`")]
   ["KEYWORD" (token-regex #"[\p{L}]+:")]
   ["SYMBOL" (token-regex #"[\p{L}][-+=!%&*<>_|\p{L}\p{N}]*")]
   ["OPERATOR" (token-regex #"[-+=!%&*<>_][-+=!%&*<>_|\p{L}\p{N}]*")]
   ["DOT" (token-regex #"\.")]
   ["COMMA" (token-regex #",")]
   ["LPAREN" (token-regex #"\(")]
   ["RPAREN" (token-regex #"\)")]
   ["LBRACKET" (token-regex #"\[")]
   ["RBRACKET" (token-regex #"\]")]
   ["LBRACE" (token-regex #"\{")]
   ["RBRACE" (token-regex #"\}")]
   ["TILDE_AT" (token-regex #"~@")]
   ["TILDE" (token-regex #"~")]
   ["POUND" (token-regex #"#")]
   ["CARAT" (token-regex #"\^")]
   ["COLON" (token-regex #"\:")]
   ["LINE_START" #"^(?s)(?:\n(?:[ ]|/\*.*?\*/|//[^\n]*)*)*\n([ ])*"]
   ["WS" #"^(?s)([ ]|/\*.*?\*/|//[^\n]*)+"]
   ])

(defn result-for-match
  "Returns a token object for the provided regex match object."
  [token-name match]
  (cond
    (some #{token-name} #{"SYMBOL" "KEYWORD" "OPERATOR" "NUMBER" "STRING"
                          "CODE_STRING"})
    {:token (str token-name "(" (second match) ")")}

    (= "LINE_START" token-name)
    {:token "LINE_START" :indent (count (second match))}

    :else
    {:token token-name}))

(defn token-match
  "Returns a function which accepts a [token regex] pair and returns a [token
  remainder] pair if the regex matches 'string'."
  [string]
  (fn [[name regex]]
    (when-let [match (re-find regex string)]
      (assoc (result-for-match name match)
             :rest
             (subs string (count (first match)))))))

(defn next-token
  "Returns a vector pair consisting of a token string representing the first
   token present in 'string' and the remainder of the string"
  [string]
  (if-let [next (some (token-match string) token-regexes)]
    next
    (common/failure
     :TokenError
     (str "Unexpected character '" (first string)))))

(defn make-indent-token
  "Returns a pair consisting of an indentation token to insert (or nil) and the
  new current indentation based on the provided line indentation and current
  indentation"
  [line-indent current-indent]
  (cond
    (or (nil? line-indent) (= line-indent current-indent))
    [nil current-indent]
    (< line-indent current-indent)
    ["DEDENT" line-indent]
    (> line-indent current-indent)
    ["INDENT" line-indent]))

(defn new-balanced-delimiters
  [balanced-delimiters {token-string :token}]
  (case token-string
    "LPAREN" (update-in balanced-delimiters [:PAREN] inc)
    "RPAREN" (update-in balanced-delimiters [:PAREN] dec)
    "LBRACE" (update-in balanced-delimiters [:BRACE] inc)
    "RBRACE" (update-in balanced-delimiters [:BRACE] dec)
    "LBRACKET" (update-in balanced-delimiters [:BRACKET] inc)
    "RBRACKET" (update-in balanced-delimiters [:BRACKET] dec)
    balanced-delimiters))

(defn currently-balanced?
  [balanced-delimiters]
  (and (= 0 (:PAREN balanced-delimiters))
       (= 0 (:BRACE balanced-delimiters))
       (= 0 (:BRACKET balanced-delimiters))))

(defn negative-balanced?
  [balanced-delimiters]
  (or (neg? (:PAREN balanced-delimiters))
      (neg? (:BRACE balanced-delimiters))
      (neg? (:BRACKET balanced-delimiters))))

(defn make-new-tokens
  [{token-string :token line-indent :indent} current-indent balanced-delimiters]
  (if (negative-balanced? balanced-delimiters)
    (common/failure :TokenError
                    (str "Unexpected close delimiter: " balanced-delimiters))
    (cond (nil? line-indent)
          {:tokens [token-string] :indent current-indent}

          (currently-balanced? balanced-delimiters)
          (cond
            (= line-indent current-indent)
            {:tokens [token-string] :indent current-indent}
            (< line-indent current-indent)
            {:tokens ["DEDENT" token-string] :indent line-indent}
            (> line-indent current-indent)
            {:tokens ["INDENT" token-string] :indent line-indent})

          :else
          {:tokens ["WS"] :indent current-indent})))

(defn process-token
  [{rest :rest :as token}
   result
   current-indent
   balanced-delimiters]
  (let [new-balanced (new-balanced-delimiters balanced-delimiters token)
        new-tokens-result (make-new-tokens token current-indent new-balanced)]
    (if (common/failure? new-tokens-result)
      new-tokens-result
      {:rest rest
       :result (apply conj result (:tokens new-tokens-result))
       :current-indent (:indent new-tokens-result)
       :balanced-delimiters new-balanced})))

(defn tokenize-string
  "Performs lexical analysis on string, returning a string containing the
   tokenized representation of the string or a LexicalError value."
  [string]
  (loop [rest (str "\n" string "\n")
         result []
         current-indent 0
         balanced-delimiters {:PAREN 0 :BRACE 0 :BRACKET 0}]
    (if (empty? rest)
      (if (currently-balanced? balanced-delimiters)
        result
        (common/failure :TokenError
                        (str "Unclosed Delimiters! " balanced-delimiters)))
      (let [next-token (next-token rest)]
        (if (common/failure? next-token)
          next-token
          (let [new-args (process-token next-token
                                        result
                                        current-indent
                                        balanced-delimiters)]
            (if (common/failure? new-args)
              new-args
              (recur (:rest new-args)
                     (:result new-args)
                     (:current-indent new-args)
                     (:balanced-delimiters new-args)))))))))
