(defmodule loise-opts-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest triggers?-not
  (is-not (loise-opts:triggers? #m()))
  (is-not (loise-opts:triggers? #m(foo bar))))

(deftest triggers?
  (is (loise-opts:triggers? #m(width 100)))
  (is (loise-opts:triggers? #m(foo bar
                               width 100)))
  (is (loise-opts:triggers? #m(width 100
                               height 200)))
  (is (loise-opts:triggers? #m(grades-count 5
                               ascii-map '()
                               colors '()))))

(deftest trigger-update?-empty
  (is-not (loise-opts:trigger-update? #m()))
  (is-not (loise-opts:trigger-update? '())))

(deftest trigger-update?-no-triggers
  (is-not (loise-opts:trigger-update? #m(foo bar))))

(deftest trigger-update?-with-triggers
  (is (loise-opts:trigger-update? #m(width 100)))
  (is (loise-opts:trigger-update? #m(foo bar
                                     width 100)))
  (is (loise-opts:trigger-update? #m(width 100
                                     height 200)))
  (is (loise-opts:trigger-update? #m(grades-count 5
                                     ascii-map '()
                                     colors '()))))

(deftest trigger-update?-non-empty
  (is-not (loise-opts:trigger-update? '(a random list))))

(deftest triggers-intersection
  (is-equal '()
            (sets:to_list (loise-opts:triggers-intersection #m())))
  (is-equal '()
            (sets:to_list (loise-opts:triggers-intersection #m(foo bar))))
  (is-equal '(width)
            (sets:to_list (loise-opts:triggers-intersection #m(width 100))))
  (is-equal '(width)
            (sets:to_list
             (loise-opts:triggers-intersection #m(foo bar
                                                  width 100))))
  (is-equal '(height width)
            (sets:to_list
             (loise-opts:triggers-intersection #m(width 100
                                                  height 200))))
  (is-equal '(ascii-map colors grades-count)
            (sets:to_list
             (loise-opts:triggers-intersection #m(grades-count 5
                                                  ascii-map '()
                                                  colors '())))))

(deftest empty-triggers?
  (is-equal 'true
            (loise-opts:empty-triggers? #m()))
  (is-equal 'true
            (loise-opts:empty-triggers? #m(foo bar)))
  (is-equal 'false
            (loise-opts:empty-triggers? #m(width 100)))
  (is-equal 'false
            (loise-opts:empty-triggers? #m(foo bar
                                                  width 100)))
  (is-equal 'false
            (loise-opts:empty-triggers? #m(width 100
                                                  height 200)))
  (is-equal 'false
            (loise-opts:empty-triggers? #m(grades-count 5
                                                  ascii-map '()
                                                  colors '()))))
