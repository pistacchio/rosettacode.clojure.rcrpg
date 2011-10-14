(ns rosettacode.rcrpg.clojure)
(use '[clojure.string :only (join split)])

(defn split-keys
  "returns map m (with string keys) creating multiple entries for keys separated by regex splitter."
  [m splitter]
  (apply hash-map (flatten
    (map #(let [splitted (split (first %) splitter)]
            (if (> (count splitted) 1)
                (map vector splitted (repeat (second %)))
                %)) m))))

(def *translation-map*
  (split-keys {"drop" "drop-item"
               "get|take" "take-item"
               "i|inv" "inventory"
               "n|north" "move north"
               "w|west" "move west"
               "s|south" "move south"
               "e|east" "move east"
               "u|up" "move up"
               "d|down" "move down"
               "alias" "alias-command"} #"\|"))

;; room = (x y z [on-the-ground])
(def *world-state* {:maze {[0 0 0] #{:sledge}
                           [1 1 5] #{:lots-of-gold}}
                    :inventory #{}
                    :current-room [0 0 0]
                    :equipped nil
                    :aliases *translation-map*})

(def *directions* [:north :west :south :east :up :down])

(def *coords*
  (zipmap *directions*  [[0  1  0 ]
                         [-1 0  0 ]
                         [0 -1  0 ]
                         [1  0  0 ]
                         [0  0  1 ]
                         [0  0  -1]]))

(defn translate
  "if source is a key of target, returns the value as a sequence (split by spaces), else returns source."
  [source target]
  (split (if-let [t (get target source)] t source) #" "))

;; ** utilities ** ;;
(defn callable?
  "checks if string s corresponds to the name of a function"
  [s]
  (resolve (symbol s)))

(defn coord-at
  "given a coordinate (eg. [1 1 1]) and a direction (eg. :north) returns the coordinate for the direction (eg. [1 2 1])"
  [current direction]
  (vec (map + (direction *coords*) current)))

(defn current-room
  "returns the room the player is in, eg. [[1 2 3] #{:gold}]"
  [world]
  [(:current-room world) ((world :maze) (world :current-room ))])

(defn room-position
  "returns the coordinate for room (eg. [1 2 1])"
  [room]
  (first room))

(defn room-ground
  "returns the content of room (eg. #{gold})"
  [room]
  (second room))

(defn in?
  "returns true if k is in coll"
  [coll k]
  (some #{k} coll))

(defn in-inv?
  "returns true if the player has item in her inventory"
  [world item]
  (in? (world :inventory) item))

(defn new-room-at
  "returns a new room relative to the player's current position, towards direction"
  [world direction]
  [(coord-at (world :current-room) direction) (hash-set (rand-nth [:gold :sledge :ladder]))])

(defn if-valid-direction
  "fn if direction is valid, otherwise prints an error message, else fnelse"
  [direction fn fnelse]
  (if-not (in? *directions* direction)
    (do
      (println "Where?!")
      fnelse)
    fn))

;; ** describing rooms ** ;;

;; find adjacent exits
(defn find-exits
  "returns a list of directions with the exit directions for room, eg. (:north :east)"
  [room world]
  (let [calc-neighbour #(map + (room-position room) (second %))
        maze (world :maze)]
    (->> *coords* (filter #(contains? maze (calc-neighbour %))) (map key))))

(defn exit?
  "returns true if room has an exit towards direction"
  [room world direction]
  (in? (find-exits room world) direction))

(defn describe-exits
  "returns string describing the exits in room"
  [room world]
  (let [exits-to-string (fn [exits]
          (cond
          (= (count exits) 1) (str " There is an exit " (name (first exits)) "ward")
          (not-empty exits) (str " There are exits at " (->> exits (map name) (join ", ")) ".")
          :else ""))]
    (exits-to-string (find-exits room world))))

(defn describe-items
  "returns a description of items"
  [items]
  (let [i (reduce #(conj %1
            (case %2
              :sledge "a sledge"
              :gold "some gold coins"
              :ladder "a ladder lying down"
              :lots-of-gold "LOTS of gold!"))
            [] items)]
     (str
       (join ", " (drop-last 2 i))
       (when (> (count i) 2) ", ")
       (join " and " (take-last 2 i)))))

(defn describe-ground
  "returns string describing the exits in room"
  [room]
  (let [room-content (room-ground room)]
  (if  (not-empty room-content)
     (str
       " On the ground you can see: " (describe-items room-content) "."))))

(defn describe
  "prints a description of room"
  [room world]
  (println (str "You are at " (join " "  (room-position room)) "." (describe-ground room) (describe-exits room world))))

;; ** actions ** ;;
(defn look [world]
  "describes the room the player is in"
  (describe (current-room world) world)
  world)

(defn dig
  "digs a new room towards direction if there's not a room already and the player has a sledge"
  ([world direction]
    (let [dir (keyword direction)]
      (if-valid-direction dir
        (if (exit? (current-room world) world dir)
          (do
            (println "There is already a room!")
            world)
          (if (= (world :equipped) :sledge)
            (do
              (println (str "You dig a new room " direction "ward."))
              (assoc world :maze (merge (world :maze) (new-room-at world dir))))
            (do
              (println "You need to equip a sledge in order to dig the wall!")
              world)))
        world)))
  ([world]
    (println "Where do you want to dig?")
    world))

(defn move
  "moves the player to an adjacent room and describes it"
  ([world direction]
    (let [dir (keyword direction)
          target-coord (coord-at (room-position (current-room world)) dir)]
      (if-valid-direction dir
        (if (exit? (current-room world) world dir)
          (if (and (= dir :up) (not ((room-ground (current-room world)) :ladder)))
            (do
              (println "You cannot go up if there's no ladder in the room.")
              world)
            (let [updated-world (assoc world :current-room target-coord)]
              (describe (current-room updated-world) updated-world)
              updated-world))
          (do
            (println "There's no exit in that direction!")
            world))
        world)))
  ([world]
    (println "Where do you want to go?")
    world))

(defn equip
  "equips an item if specified and if the player has it in her inventory"
  ([world item]
    (let [i (keyword item)]
      (if (in-inv? world i)
        (do
          (println "Equipped!")
          (assoc world :equipped i))
        (do
          (println "You haven't such an item")
          world))))
  ([world]
    (println "What do you want to equip?")
    world))

(defn drop-item
  "drops an item in the inventory or all of them leaving it in the room"
  ([world item]
    (let [i (keyword item)
          current-position (get world :current-room)
          current-ground (room-ground (current-room world))
          current-maze (world :maze)
          current-inventory (world :inventory)
          update-room (partial assoc current-maze current-position)
          equipped-item (world :equipped)]
      (if (= i :all)
        (do
          (println "Everything dropped!")
          (assoc world
            :equipped nil
            :inventory #{}
            :maze (update-room (clojure.set/union current-ground current-inventory))))
        (do
          (if (in-inv? world i)
            (do
              (println "Item dropped!")
              (assoc world
                :equipped (if (= equipped-item i) nil equipped-item)
                :inventory (disj current-inventory i)
                :maze (update-room (conj current-ground i))))
            (do
              (println "You haven't such an item")
              world))))))
  ([world]
    (println "What do you want to drop?")
    world))

(defn take-item
  "picks up an item from the ground or all of them putting them into the inventory"
  ([world item]
    (let [i (keyword item)
          current-position (get world :current-room)
          current-ground (room-ground (current-room world))
          current-maze (:maze world)
          current-inventory (world :inventory)
          update-room (partial assoc current-maze current-position)
          equipped-item (world :equipped)]
      (if (= i :all)
        (do
          (println "Everything taken!")
          (assoc world
            :inventory (clojure.set/union current-inventory current-ground)
            :maze (update-room #{})))
        (do
          (if (in? current-ground i)
            (do
              (println "Item taken!")
              (assoc world
                :inventory (current-inventory i)
                :maze (update-room (disj current-ground i))))
            (do
              (println "There is not such item on the ground!")
              world))))))
  ([world]
    (println "What do you want to pick up?")
    world))

(defn inventory
  "prints what the player is carrying"
  [world]
  (let [inv (world :inventory)]
    (println (if (empty? inv)
               "You are not carrying anything"
               (str "You are carrying: " (describe-items inv)))))
    world)

(defn equip
  ([world item]
    (let [i (keyword item)]
      (if (in-inv? world i)
        (do
          (println "Item equipped.")
          (assoc world :equipped i))
        (do
          (println "You haven't such item")
          world))))
  ([world]
    (do
      (println "What do you want to equip?")
      world)))

(defn alias-command
  "aliases command to alias"
  ([world alias & commands]
    (let [command (join " " commands)
          current-aliases (world :aliases)]
      (do
        (println (str "Alias created for the command " command))
        (assoc world :aliases (assoc current-aliases alias command)))))
  ([world]
    (do
      (println "Alias what?")
      world))
  ([world alias]
    (do
      (println (str "Alias '" alias "' to what?"))
      world)))

;; ** user input ** ;;

(defn sanitize-input
  "lowercases input, splits it into words and trims the tokens"
  [input]
  (remove empty? (-> input .toLowerCase .trim (split #" "))))

(defn parse-input
  "interprets user input. returns an updated world state if the game has to continue, nil if the user inputs 'exit'"
  [input world]
  (if (not-empty input)
    (let [sanitized-input (sanitize-input input)
          command (translate (first sanitized-input) (world :aliases))
          i (concat command (rest sanitized-input))
          action (first i)
          args (rest i)]
      (cond
        (= (first i) "exit") nil
        (callable? action) (try
                             (apply (resolve (symbol action)) (conj args world ))
                             (catch IllegalArgumentException e (println "Invalid arguments") world))
        :else (do (println "What do you mean?") world)))
    (do
      (println "Hm?!")
      world)))

;; main loop
(defn run
  "the main game loop"
  []
  (loop [input (read-line)
         world *world-state*]
    (if-let [w (parse-input input world)]
            (recur (read-line) w)
            (println "See you next time!"))))

;;(drop-item (drop-item *world-state* "all") "all")

(run)