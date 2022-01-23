(ns fruit-economy.language
  (:import [squidpony FakeLanguageGen NaturalLanguageCipher]))


(defn make-lang []
  (let [language (FakeLanguageGen/randomLanguage
                   (bit-xor
                     (long (* (- (rand) 0.5) 0x10000000000000))
                     (long (* (- (rand) 0.5) 2 0x8000000000000000))))]
    language))

(defn make-word
  ([lang] (make-word lang true))
  ([lang capitalise?]
   (.word lang capitalise?)))


(comment
  (let [language (FakeLanguageGen/randomLanguage
                   (bit-xor
                     (long (* (- (rand) 0.5) 0x10000000000000))
                     (long (* (- (rand) 0.5) 2 0x8000000000000000))))]
    #_(.word language true)
    language)

  (let [lang FakeLanguageGen/DEEP_SPEECH]
    (.word lang true))

  (let [translator (NaturalLanguageCipher. FakeLanguageGen/DEEP_SPEECH)]
    (.cipher translator "Civ %+7" #_"This is a Fruit Economy")
    #_(.cipherMarkup translator "This [?]is[?] a Fruit Economy")
    #_(clojure.reflect/reflect translator))

  (clojure.reflect/reflect FakeLanguageGen/DEEP_SPEECH)
  (clojure.reflect/reflect NaturalLanguageCipher))