(ns caution.test-runner
  (:require [caution.diff-test]
            [caution.session-test]
            [clojure.test :as t]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'caution.diff-test 'caution.session-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
