(module user.util)

(defn chunks [n t]
  "Splits a sequential table `t` into a new table of `n`-length chunks"
  (let [out []]
    (for [i 1 (length t) n]
      (let [chunk []]
        (for [j i (+ i (- n 1)) 1]
          (table.insert chunk (. t j)))
        (table.insert out chunk)))
    out))

(defn sanitize [path]
  "Simple function that replaces backslashes in paths to help stay compatible on Windows"
  (pick-values 1 (string.gsub path "\\" "/")))

