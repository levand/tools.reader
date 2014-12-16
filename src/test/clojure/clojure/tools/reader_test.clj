(ns clojure.tools.reader-test
  (:refer-clojure :exclude [read-string *default-data-reader-fn* *features*])
  (:use [clojure.tools.reader :only [read-string *default-data-reader-fn* *features*]]
        [clojure.test :only [deftest is are]])
  (:import clojure.lang.BigInt))

(load "common_tests")

(deftest read-keyword
  (is (= :foo-bar (read-string ":foo-bar")))
  (is (= :foo/bar (read-string ":foo/bar")))
  (is (= :user/foo-bar (binding [*ns* (the-ns 'user)]
                         (read-string "::foo-bar"))))
  (is (= :clojure.core/foo-bar
         (do (alias 'core 'clojure.core)
             (read-string "::core/foo-bar"))))
  (is (= :*+!-_? (read-string ":*+!-_?")))
  (is (= :abc:def:ghi (read-string ":abc:def:ghi")))
  (is (= :abc.def/ghi (read-string ":abc.def/ghi")))
  (is (= :abc/def.ghi (read-string ":abc/def.ghi")))
  (is (= :abc:def/ghi:jkl.mno (read-string ":abc:def/ghi:jkl.mno")))
  (is (instance? clojure.lang.Keyword (read-string ":alphabet"))) )

(deftest read-regex
  (is (= (str #"\[\]?(\")\\")
         (str (read-string "#\"\\[\\]?(\\\")\\\\\"")))))

(deftest read-quote
  (is (= ''foo (read-string "'foo"))))

(deftest read-syntax-quote
  (is (= '`user/foo (binding [*ns* (the-ns 'user)]
                      (read-string "`foo"))))
  (is (= () (eval (read-string "`(~@[])"))))
  (is (= '`+ (read-string "`+")))
  (is (= '`foo/bar (read-string "`foo/bar")))
  (is (= '`1 (read-string "`1")))
  (is (= `(1 (~2 ~@'(3))) (eval (read-string "`(1 (~2 ~@'(3)))")))))

(deftest read-deref
  (is (= '@foo (read-string "@foo"))))

(deftest read-var
  (is (= '(var foo) (read-string "#'foo"))))

(deftest read-fn
  (is (= '(fn* [] (foo bar baz)) (read-string "#(foo bar baz)"))))

(deftest read-arg
  (is (= 14 ((eval (read-string "#(apply + % %1 %3 %&)")) 1 2 3 4 5)))
  (is (= 4 ((eval (read-string "#(last %&)")) 1 2 3 4))))

(deftest read-eval
  (is (= 3 (read-string "#=(+ 1 2)"))))

(deftest read-tagged
  ;; (is (= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst \"2010-11-12T13:14:15.666\"")))
  ;; (is (= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst\"2010-11-12T13:14:15.666\"")))
  ;; (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  ;; (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (java.util.UUID/fromString "550e8400-e29b-41d4-a716-446655440000")
         (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (java.util.UUID/fromString "550e8400-e29b-41d4-a716-446655440000")
                  (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (when *default-data-reader-fn*
    (let [my-unknown (fn [tag val] {:unknown-tag tag :value val})]
      (is (= {:unknown-tag 'foo :value 'bar}
             (binding [*default-data-reader-fn* my-unknown]
               (read-string "#foo bar")))))))

(defrecord foo [])
(defrecord bar [baz buz])

(deftest read-record
  (is (= (foo.) (read-string "#clojure.tools.reader_test.foo[]")))
  (is (= (foo.) (read-string "#clojure.tools.reader_test.foo []"))) ;; not valid in clojure
  (is (= (foo.) (read-string "#clojure.tools.reader_test.foo{}")))
  (is (= (assoc (foo.) :foo 'bar) (read-string "#clojure.tools.reader_test.foo{:foo bar}")))

  (is (= (map->bar {}) (read-string "#clojure.tools.reader_test.bar{}")))
  (is (= (bar. 1 nil) (read-string "#clojure.tools.reader_test.bar{:baz 1}")))
  (is (= (bar. 1 nil) (read-string "#clojure.tools.reader_test.bar[1 nil]")))
  (is (= (bar. 1 2) (read-string "#clojure.tools.reader_test.bar[1 2]"))))

(deftest read-ctor
  (is (= "foo" (read-string "#java.lang.String[\"foo\"]"))))

(deftest read-features
  (binding [*features* #{:clj}]
    (are [out s] (= out (read-string s))

         ["x"] "[#+clj \"x\"]"
         ["a" "x"] "[\"a\" #+clj \"x\"]"
         [] "[#+cljs \"x\"]"

         ["x"] "[#+(and) \"x\"]"
         ["x"] "[#+(and clj) \"x\"]"
         ["x"] "[#+(and clj clj) \"x\"]"
         [] "[#+(and clj cljs) \"x\"]"

         [] "[#+(or) \"x\"]"
         ["x"] "[#+(or clj) \"x\"]"
         ["x"] "[#+(or clj cljs) \"x\"]"
         []   "[#+(or cljs) \"x\"]"

         ["x"] "[#+(not cljs) \"x\"]"

         nil "#+cljs #js {} nil"
         :foo "#+cljs #js {} :foo"

         "x" "#+cljs [:foo #bar 123 :baz] #+clj \"x\""

         :foo/bar "#-clj :baz :foo/bar"
         :baz "#-cljs :baz"
         :x "#-(not clj) :x"

         [] "[#-(and) \"x\"]"
         [] "[#-(and clj) \"x\"]"
         [] "[#-(and clj clj) \"x\"]"
         ["x"] "[#-(and clj cljs) \"x\"]"

         ["x"] "[#-(or) \"x\"]"
         [] "[#-(or clj) \"x\"]"
         [] "[#-(or clj cljs) \"x\"]"
         ["x"] "[#-(or cljs) \"x\"]"

         "foo"         "#+cljs \"bar\" \"foo\""
         "foo"         "#+cljs #js \"bar\" \"foo\""
         ["foo"]       "[ #+cljs \"bar\" \"foo\"]"
         []            "[ #+cljs \"bar\"]")))
