(module user.util)

(defn chunks [n t]
  (let [out []]
    (for [i 1 (length t) n]
      (let [chunk []]
        (for [j i (+ i (- n 1)) 1]
          (table.insert chunk (. t j)))
        (table.insert out chunk)))
    out))
