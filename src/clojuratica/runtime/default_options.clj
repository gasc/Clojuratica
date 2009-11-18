(ns clojuratica.runtime.default-options)

(def *default-options*
   {#{:vectors :seqs}           :vectors
    #{:parallel :serial}        :serial
    #{:parse :no-parse}         :parse
    #{:evaluate :no-evaluate}   :evaluate
    #{:convert :no-convert}     :convert
    #{:hash-maps :no-hash-maps} :hash-maps
    #{:functions :no-functions} :functions
    #{:aliases :no-aliases}     :aliases
    #{:verbose :no-verbose}     :no-verbose
    #{:as-function
      :as-expression}           :as-expression
    #{:full-form
      :clojure-form}            :clojure-form
    #{:restore-defaults
      :no-restore-defaults}     :no-restore-defaults
    :alias-list                 :clojure-aliases
    :poll-interval              20 ;ms
    :clojure-aliases            {'list 'List
                                 'do   'CompoundExpression
                                 '=    'Set
                                 ':=   'SetDelayed
                                 '=.   'Unset
                                 '->   'Rule
                                 ':>   'RuleDelayed
                                 '==   'Equals
                                 '===  'SameQ
                                 '<    'Less
                                 '>    'Greater
                                 '<=   'LessEqual
                                 '>=   'GreaterEqual
                                 '+=   'AddTo
                                 '-=   'SubtractFrom
                                 '+    'Plus
                                 '-    'Subtract
                                 '*    'Times
                                 '.    'Dot
                                 '/    'Divide
                                 '<>   'StringJoin
                                 '&&   'And
                                 (symbol "||")   'Or  ;'|| annoys IntelliJ's syntax-checking
                                 '!    'Not}})
