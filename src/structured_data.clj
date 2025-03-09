(ns structured-data)

(require 'clojure.set)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))



(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1] [x2]] rectangle ]
       (- x2 x1)))

(defn height [rectangle]
  (let [[p1 p2] rectangle]
     (- (get p2 1) (get p1 1))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [xp yp] point]
      (and (<= x1 xp x2) (<= y1 yp y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
    ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authorVector (get book :authors)]
    (assoc book :authors (conj authorVector new-author))))

(defn alive? [author]
 (not (contains? author :death-year)) )

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn[x] (get x 1))]
    (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
 (or (apply <= a-seq) (apply >= a-seq)) )

(defn stars [n]
  (apply str (repeat n "*") ))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
 (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (fn [birth death] (if birth (str " (" birth " - " (if death death "") ")") ""))] 
    (str name (years (:birth-year author) (:death-year author)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [ booklist (fn [books] (apply str (interpose ". " (map book->string books))))]
    (cond
      (empty? books) "No books."
      (== (count books) 1) (str "1 book. " (booklist books) ".")
      :else (str (count books) " books. " (booklist books) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [matchName? (fn [x] (= name (:name x)))]
     (first (filter matchName? authors)))
 )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
 (not (empty? (living-authors (:authors book)))) )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
