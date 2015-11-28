(ns gridom.utils)

(defn hsv->rgb [h s v]
  (let [c (* v s)
        xp (- (mod (/ h 60) 2) 1)
        x (* c (- 1 (max xp (- xp))))
        m (- v c)
        [rp gp bp] (cond (< h  60) [c, x, 0]
                         (< h 120) [x, c, 0]
                         (< h 180) [0, c, x]
                         (< h 240) [0, x, c]
                         (< h 300) [x, 0, c]
                         (< h 360) [c, 0, x])
        f (fn [i] (* 255 (+ i m)))]
    (map f [rp gp bp])))

(defn digit->hex [d]
  (nth "0123456789abcdef" d))

(defn byte->hex [i]
  (let [a (int (/ i 16))
        b (mod i 16)]
    (str (digit->hex a) (digit->hex b))))

(defn rgb->hexstr [r g b]
  (str "#" (apply str (map byte->hex [r g b]))))
