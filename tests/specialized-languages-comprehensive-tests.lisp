;; tests/specialized-languages-comprehensive-tests.lisp
;; Comprehensive test suites for GOAL, ZIL, Fountain, and Burgermistress
;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :eightbol)

;;; =============================================================================
;;; GOAL LANGUAGE TESTS
;;; =============================================================================

(def-suite :goal-comprehensive
  :description "Comprehensive GOAL language form tests")

(in-suite :goal-comprehensive)

;;; DEFUN Tests (Function Definition)
(test goal-defun-simple
  "Test simple DEFUN definition."
  (let* ((source "(defun add (a b) (+ a b))")
         (result (goal-parse source)))
    (is (listp result))
    (is (eq (car result) :form))))

(test goal-defun-with-multiple-params
  "Test DEFUN with multiple parameters."
  (let* ((source "(defun calculate-health (base damage armor) (- (+ base armor) damage))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-defun-recursive
  "Test recursive DEFUN."
  (let* ((source "(defun factorial (n acc) (if (<= n 1) acc (factorial (- n 1) (* n acc))))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-defun-with-local-scope
  "Test DEFUN that uses LET for local variables."
  (let* ((source "(defun test () (let ((x 10)) (+ x 5)))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-defun-with-numeric-types
  "Test DEFUN using different numeric types."
  (let* ((source "(defun test-types (dec hex bin oct) (+ dec hex bin oct))")
         (result (goal-parse source)))
    (is (listp result))))

;;; LET Tests (Local Binding)
(test goal-let-simple-binding
  "Test simple LET with one binding."
  (let* ((source "(let ((x 10)) x)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-let-multiple-bindings
  "Test LET with multiple parallel bindings."
  (let* ((source "(let ((x 10) (y 20) (z 30)) (+ x y z))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-let-with-expressions
  "Test LET with expression initializers."
  (let* ((source "(let ((x (+ 5 5)) (y (* 3 7))) (+ x y))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-let-nested
  "Test nested LET expressions."
  (let* ((source "(let ((x 10)) (let ((y 20)) (+ x y)))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-let-numeric-types
  "Test LET with various numeric types."
  (let* ((source "(let ((dec 100) (hex #xFF) (bin #b1010) (oct #o77)) (+ dec hex bin oct))")
         (result (goal-parse source)))
    (is (listp result))))

;;; IF Tests (Conditional)
(test goal-if-simple
  "Test simple IF with then branch."
  (let* ((source "(if (> x 0) 1)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-if-with-else
  "Test IF with else branch."
  (let* ((source "(if (> x 0) 1 0)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-if-nested
  "Test nested IF expressions."
  (let* ((source "(if (> x 0) (if (< x 100) \"ok\" \"big\") \"negative\")")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-if-with-complex-test
  "Test IF with complex test expression."
  (let* ((source "(if (and (> x 50) (< x 100) (= status 'active)) 1 0)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-if-numeric-type-comparison
  "Test IF comparing different numeric types."
  (let* ((source "(if (< #xFF 256) #b1010 #o77)")
         (result (goal-parse source)))
    (is (listp result))))

;;; COND Tests (Multi-Branch Conditional)
(test goal-cond-simple
  "Test simple COND with multiple branches."
  (let* ((source "(cond ((< health 30) \"critical\") ((< health 60) \"wounded\") (t \"healthy\"))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-cond-with-numeric-types
  "Test COND with different numeric type tests."
  (let* ((source "(cond ((= code #xFF) \"error\") ((= code #x00) \"ok\") ((= code #b1010) \"pending\") (t \"unknown\"))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-cond-complex-conditions
  "Test COND with complex test expressions."
  (let* ((source "(cond ((and (> level 10) (< xp 5000)) \"fast\") ((and (> level 5) (< xp 1000)) \"normal\") (t \"slow\"))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-cond-nested
  "Test nested COND expressions."
  (let* ((source "(cond ((= state 'menu) (cond ((= event 'start) 'playing) (t 'menu))) ((= state 'playing) 'playing) (t state))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-cond-with-side-effects
  "Test COND with expression bodies."
  (let* ((source "(cond ((> x 100) (print \"big\") 1) ((> x 50) (print \"medium\") 2) (t (print \"small\") 3))")
         (result (goal-parse source)))
    (is (listp result))))

;;; LOOP Tests (Iteration)
(test goal-loop-simple
  "Test simple LOOP."
  (let* ((source "(loop (update-game) (render))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-loop-with-break
  "Test LOOP with BREAK control."
  (let* ((source "(loop (update) (if (game-should-quit?) (break)))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-loop-with-continue
  "Test LOOP with CONTINUE control."
  (let* ((source "(let ((i 0)) (loop (if (= (mod i 2) 0) (continue)) (print i) (set! i (+ i 1)) (if (>= i 10) (break))))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-loop-counter-pattern
  "Test LOOP with counter accumulation pattern."
  (let* ((source "(let ((c 0)) (loop (print c) (set! c (+ c 1)) (if (>= c 255) (break))))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-loop-with-numeric-literals
  "Test LOOP with various numeric types."
  (let* ((source "(loop (set! counter (+ counter #b1)) (if (>= counter #xFF) (break)))")
         (result (goal-parse source)))
    (is (listp result))))

;;; SETQ Tests (Variable Assignment)
(test goal-setq-simple
  "Test simple SETQ assignment."
  (let* ((source "(setq health 100)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-setq-expression
  "Test SETQ with expression value."
  (let* ((source "(setq damage (+ base-damage weapon-bonus))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-setq-multiple
  "Test multiple SETQ assignments."
  (let* ((source "(progn (setq x 10) (setq y 20) (setq z (+ x y)))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-setq-numeric-types
  "Test SETQ with different numeric types."
  (let* ((source "(progn (setq flags #b1010) (setq count 0) (setq max-val #xFF))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-setq-in-conditional
  "Test SETQ within conditional."
  (let* ((source "(if (< health 0) (setq health 0))")
         (result (goal-parse source)))
    (is (listp result))))

;;; LAMBDA Tests (Anonymous Function)
(test goal-lambda-simple
  "Test simple lambda expression."
  (let* ((source "(lambda (x) (+ x 1))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-lambda-multiple-params
  "Test lambda with multiple parameters."
  (let* ((source "(lambda (x y) (* x y))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-lambda-with-closure
  "Test lambda capturing outer scope."
  (let* ((source "(let ((multiplier 10)) (lambda (x) (* x multiplier)))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-lambda-numeric-operations
  "Test lambda with numeric type operations."
  (let* ((source "(lambda (bonus) (+ base-damage #x20 bonus #b0001))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-lambda-with-conditional
  "Test lambda containing conditional logic."
  (let* ((source "(lambda (x) (if (> x 50) \"high\" \"low\"))")
         (result (goal-parse source)))
    (is (listp result))))

;;; QUOTE Tests (Quotation)
(test goal-quote-symbol
  "Test quoting a symbol."
  (let* ((source "'player-died")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-quote-list
  "Test quoting a list."
  (let* ((source "'(north south east west)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-quoted-data-structure
  "Test quoted data in let binding."
  (let* ((source "(let ((dirs '(n s e w))) (car dirs))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-quoted-symbols-in-cond
  "Test quoted symbols in COND."
  (let* ((source "(cond ((= state 'menu) 'menu) ((= state 'playing) 'playing) (t 'quit))")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-quote-mixed-literal
  "Test quote with mixed content."
  (let* ((source "`(location ,name (north tavern))")
         (result (goal-parse source)))
    (is (listp result))))

;;; PROGN Tests (Sequence)
(test goal-progn-simple
  "Test simple PROGN sequence."
  (let* ((source "(progn (print \"start\") (update) 42)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-progn-in-conditional
  "Test PROGN in IF branch."
  (let* ((source "(if (> damage 50) (progn (play-sound 'boom) (shake-screen) 1) 0)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-progn-nested
  "Test nested PROGN expressions."
  (let* ((source "(progn (print \"outer\") (progn (print \"inner\") 100) 50)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-progn-with-assignments
  "Test PROGN with multiple assignments."
  (let* ((source "(progn (setq x 10) (setq y 20) (setq z (+ x y)) z)")
         (result (goal-parse source)))
    (is (listp result))))

(test goal-progn-side-effects
  "Test PROGN for orchestrating side effects."
  (let* ((source "(progn (load-assets) (setup-entities) (start-music) 42)")
         (result (goal-parse source)))
    (is (listp result))))

;;; =============================================================================
;;; ZIL LANGUAGE TESTS
;;; =============================================================================

(def-suite :zil-comprehensive
  :description "Comprehensive ZIL language form tests")

(in-suite :zil-comprehensive)

;;; OBJECT Tests
(test zil-object-simple
  "Test simple object definition."
  (let* ((source "<OBJECT SWORD (DESCRIPTION \"a rusty sword\") (NOUNS \"sword\")>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-object-with-properties
  "Test object with multiple properties."
  (let* ((source "<OBJECT TORCH (DESCRIPTION \"burning torch\") (SIZE 5) (WEIGHT 20) (VALUE 15)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-object-with-flags-binary
  "Test object with binary flag properties."
  (let* ((source "<OBJECT TORCH (FLAGS 0B1010) (VALUE 15)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-object-container
  "Test container object with contents."
  (let* ((source "<OBJECT BACKPACK (DESCRIPTION \"leather pack\") (CAPACITY 100) (HAS ROPE KEY POTION)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-object-with-custom-property
  "Test object with custom property."
  (let* ((source "<OBJECT DOOR (DESCRIPTION \"wooden door\") (SIZE 200) (PROPERTY strength 50)>")
         (result (zil-parse source)))
    (is (listp result))))

;;; ROOM Tests
(test zil-room-simple
  "Test simple room definition."
  (let* ((source "<ROOM TAVERN (DESCRIPTION \"You are in a tavern.\") (SHORT \"Tavern\") (NORTH HALL)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-room-all-exits
  "Test room with all directional exits."
  (let* ((source "<ROOM CROSS (SHORT \"Crossroads\") (NORTH N) (SOUTH S) (EAST E) (WEST W) (UP U) (DOWN D)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-room-with-objects
  "Test room containing objects."
  (let* ((source "<ROOM BEDROOM (SHORT \"Bedroom\") (OBJECTS BED CHEST WARDROBE)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-room-with-properties
  "Test room with environmental properties."
  (let* ((source "<ROOM DUNGEON (SHORT \"Dark Cell\") (PROPERTY danger-level 100) (PROPERTY temperature 50)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-room-complex
  "Test complex room with all features."
  (let* ((source "<ROOM LAIR (SHORT \"Dragon's Lair\") (SOUTH MOUNTAINS) (OBJECTS DRAGON GOLD) (PROPERTY danger-level 100)>")
         (result (zil-parse source)))
    (is (listp result))))

;;; VERB Tests
(test zil-verb-simple
  "Test simple verb definition."
  (let* ((source "<VERB LOOK (SYNTAX \"LOOK\") (HANDLER V-LOOK)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-verb-multiple-syntax
  "Test verb with multiple syntax patterns."
  (let* ((source "<VERB TAKE (SYNTAX \"TAKE object\") (SYNTAX \"GET object\") (SYNTAX \"GRAB object\") (HANDLER V-TAKE)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-verb-combat
  "Test combat verb definition."
  (let* ((source "<VERB ATTACK (SYNTAX \"ATTACK enemy\") (SYNTAX \"FIGHT enemy\") (HANDLER V-ATTACK)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-verb-magic
  "Test magic verb with complex syntax."
  (let* ((source "<VERB CAST (SYNTAX \"CAST spell\") (SYNTAX \"CAST spell AT target\") (HANDLER V-CAST)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-verb-directional
  "Test directional movement verb."
  (let* ((source "<VERB GO (SYNTAX \"GO direction\") (SYNTAX \"direction\") (HANDLER V-GO)>")
         (result (zil-parse source)))
    (is (listp result))))

;;; DEFINE Tests
(test zil-define-simple
  "Test simple routine definition."
  (let* ((source "<DEFINE V-LOOK () (PRINT \"You look around.\")>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-define-with-locals
  "Test routine with local variables."
  (let* ((source "<DEFINE CALC (X) (LOCAL RESULT) (SET RESULT (+ X 10)) RESULT>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-define-recursive
  "Test recursive routine definition."
  (let* ((source "<DEFINE COUNT (N COUNT) (LOCAL NEXT) (IF N (COUNT (- N 1) (+ COUNT 1)) COUNT)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-define-with-parameters
  "Test routine with multiple parameters."
  (let* ((source "<DEFINE V-TAKE (OBJECT) (LOCAL WEIGHT) (MOVE OBJECT PLAYER) (PRINT \"Taken.\")>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-define-binary-operations
  "Test routine with binary numeric operations."
  (let* ((source "<DEFINE CHECK-FLAGS (FLAGS MASK) (LOCAL RESULT) (SET RESULT (BAND FLAGS MASK))>")
         (result (zil-parse source)))
    (is (listp result))))

;;; SET Tests
(test zil-set-simple
  "Test simple SET assignment."
  (let* ((source "(SET HEALTH 100)")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-set-expression
  "Test SET with expression."
  (let* ((source "(SET DAMAGE (+ BASE-DAMAGE BONUS))")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-set-numeric-types
  "Test SET with different numeric types."
  (let* ((source "(SET FLAGS 0B1010)")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-set-conditional
  "Test SET in conditional context."
  (let* ((source "(IF (> DAMAGE 50) (SET CRITICAL-HIT 1))")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-set-in-loop
  "Test SET in loop context."
  (let* ((source "(SET COUNTER 0) (LOOP (SET COUNTER (+ COUNTER 1)) (IF (>= COUNTER 10) (BREAK)))")
         (result (zil-parse source)))
    (is (listp result))))

;;; GET/PUT Tests
(test zil-get-property
  "Test GET property access."
  (let* ((source "(GET SWORD 'WEIGHT)")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-put-property
  "Test PUT property assignment."
  (let* ((source "(PUT PLAYER 'HEALTH 100)")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-get-in-calculation
  "Test GET in arithmetic expression."
  (let* ((source "(+ (GET WEAPON 'DAMAGE) 10)")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-put-in-conditional
  "Test PUT in conditional."
  (let* ((source "(IF (> DAMAGE 50) (PUT PLAYER 'HEALTH (- (GET PLAYER 'HEALTH) 25)))")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-property-modification-pattern
  "Test property access and modification pattern."
  (let* ((source "(LOCAL HP) (SET HP (GET PLAYER 'HEALTH)) (SET HP (- HP 25)) (PUT PLAYER 'HEALTH HP)")
         (result (zil-parse source)))
    (is (listp result))))

;;; IF Tests (ZIL)
(test zil-if-simple
  "Test simple IF in ZIL."
  (let* ((source "<IF (> PLAYER-HEALTH 0) (PRINT \"Alive.\")>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-if-with-else
  "Test IF with ELSE clause."
  (let* ((source "<IF (> PLAYER-HEALTH 0) (PRINT \"Alive.\") ELSE (PRINT \"Dead.\")>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-if-nested
  "Test nested IF."
  (let* ((source "<IF (PLAYER-HAS-WEAPON?) <IF (WEAPON-LOADED?) (FIRE) ELSE (RELOAD)> ELSE (PRINT \"Weapon needed.\")>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-if-complex-condition
  "Test IF with complex condition."
  (let* ((source "<IF (AND (> LEVEL 5) (< XP 1000) (= STATUS 'ACTIVE)) (LEVEL-UP) ELSE (PRINT \"Not ready.\")>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-if-numeric-comparison
  "Test IF with numeric type comparison."
  (let* ((source "<IF (EQUAL FLAGS 0B1010) (SET RESULT 255) ELSE (SET RESULT 0)>")
         (result (zil-parse source)))
    (is (listp result))))

;;; COND Tests (ZIL)
(test zil-cond-simple
  "Test simple COND in ZIL."
  (let* ((source "<COND ((= GAME-STATE 'PLAYING) (UPDATE-GAME)) ((= GAME-STATE 'PAUSED) (SHOW-MENU)) (ELSE (EXIT))>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-cond-numeric-tests
  "Test COND with numeric comparisons."
  (let* ((source "<COND ((< HEALTH 20) (PRINT \"Critical!\")) ((< HEALTH 50) (PRINT \"Wounded\")) (ELSE (PRINT \"Healthy\"))>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-cond-object-types
  "Test COND dispatching on object type."
  (let* ((source "<COND ((EQUAL OBJECT-TYPE 'WEAPON) (EQUIP-WEAPON)) ((EQUAL OBJECT-TYPE 'POTION) (DRINK-POTION)) (ELSE (DROP-ITEM))>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-cond-nested
  "Test nested COND."
  (let* ((source "<COND ((PLAYER-IN-COMBAT?) <COND ((HAVE-WEAPON?) (ATTACK)) (ELSE (RUN))>) ((PLAYER-IN-CHAT?) (DIALOGUE)) (ELSE (IDLE))>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-cond-with-side-effects
  "Test COND with multiple statements per branch."
  (let* ((source "<COND ((> X 100) (PRINT \"Big\") 1) ((> X 50) (PRINT \"Med\") 2) (ELSE (PRINT \"Small\") 3)>")
         (result (zil-parse source)))
    (is (listp result))))

;;; LOOP Tests (ZIL)
(test zil-loop-simple
  "Test simple LOOP."
  (let* ((source "<LOOP (UPDATE-ENTITIES) (RENDER-FRAME)>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-loop-with-break
  "Test LOOP with BREAK."
  (let* ((source "<LOOP (UPDATE) (IF (QUIT-REQUESTED?) (BREAK))>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-loop-counter
  "Test LOOP with counter."
  (let* ((source "(SET I 0) <LOOP (PRINT I) (SET I (+ I 1)) (IF (>= I 10) (BREAK))>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-loop-processing-list
  "Test LOOP processing list."
  (let* ((source "(SET CURRENT INVENTORY) <LOOP (IF (EMPTY? CURRENT) (BREAK)) (PROCESS-ITEM (CAR CURRENT)) (SET CURRENT (CDR CURRENT))>")
         (result (zil-parse source)))
    (is (listp result))))

(test zil-loop-main-game
  "Test main game loop pattern."
  (let* ((source "<LOOP (HANDLE-INPUT) (UPDATE-GAME) (RENDER) (IF (QUIT?) (BREAK))>")
         (result (zil-parse source)))
    (is (listp result))))

;;; =============================================================================
;;; FOUNTAIN LANGUAGE TESTS
;;; =============================================================================

(def-suite :fountain-comprehensive
  :description "Comprehensive Fountain language directive tests")

(in-suite :fountain-comprehensive)

;;; Scene Heading Tests
(test fountain-scene-int
  "Test interior scene heading."
  (let* ((source "INT TAVERN - AFTERNOON")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-scene-ext
  "Test exterior scene heading."
  (let* ((source "EXT FOREST CLEARING - MORNING")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-scene-blob
  "Test BLOB title card."
  (let* ((source "BLOB CHAPTER 1 - THE BEGINNING")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-scene-night
  "Test night scene heading."
  (let* ((source "INT CASTLE CHAMBER - NIGHT")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-scene-location-details
  "Test scene with detailed location."
  (let* ((source "INT UNDERGROUND TEMPLE - CANDLELIT, ANCIENT")
         (result (fountain-parse source)))
    (is (listp result))))

;;; Character Entry Tests
(test fountain-character-simple-entry
  "Test simple character entry."
  (let* ((source "Enter Hero")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-character-entry-pose
  "Test character entry with pose."
  (let* ((source "Enter Knight Looks brave Faces north")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-character-multiple-entries
  "Test multiple character entries."
  (let* ((source "Enter Guard\nEnter Guard\nEnter Captain Looks commanding")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-character-entry-emotion
  "Test character entry with emotion."
  (let* ((source "Enter Villain Looks menacing")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-staged-entrance
  "Test staged character entrance."
  (let* ((source "Enter Hero\nEnter Companion Looks worried Faces east")
         (result (fountain-parse source)))
    (is (listp result))))

;;; Action Tests
(test fountain-action-simple
  "Test simple action line."
  (let* ((source "ACTION Hero draws sword and points it at the dragon.")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-action-dramatic
  "Test dramatic action sequence."
  (let* ((source "ACTION The ground shakes. A massive cave opens.")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-action-sound
  "Test action with sound effects."
  (let* ((source "ACTION Door SLAMS open. Wind howls through the chamber.")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-action-environmental
  "Test environmental action."
  (let* ((source "ACTION Lightning flashes. Thunder echoes across the mountains.")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-action-character-movement
  "Test character action."
  (let* ((source "ACTION Villain laughs menacingly and raises staff.")
         (result (fountain-parse source)))
    (is (listp result))))

;;; Dialogue Tests
(test fountain-dialogue-simple
  "Test simple dialogue."
  (let* ((source "HERO\nI must stop the evil wizard!")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-dialogue-extended
  "Test extended dialogue."
  (let* ((source "WIZARD\nYou think you can defeat me? I have studied magic for centuries.")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-dialogue-question
  "Test question dialogue."
  (let* ((source "COMPANION\nWhere are we going now?")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-dialogue-exchange
  "Test dialogue exchange between characters."
  (let* ((source "HERO\nWhere is the treasure?\n\nGUIDE\nOver that ridge.")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-dialogue-emotional
  "Test emotional dialogue."
  (let* ((source "HERO\nNo! She cannot be dead.")
         (result (fountain-parse source)))
    (is (listp result))))

;;; Parenthetical Tests
(test fountain-parenthetical-simple
  "Test simple parenthetical."
  (let* ((source "HERO\n(angrily)\nYou will not get away with this!")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-parenthetical-action
  "Test action parenthetical."
  (let* ((source "WIZARD\n(raises staff)\nFeel the power!")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-parenthetical-emotional
  "Test emotional parenthetical."
  (let* ((source "COMPANION\n(sadly)\nI never thought it would end like this.")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-parenthetical-directed
  "Test directed parenthetical."
  (let* ((source "HERO\n(to the Wizard)\nYou know this is wrong!")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-parenthetical-timing
  "Test timing parenthetical."
  (let* ((source "GUARD\n(after a long pause)\nThe dungeons are that way.")
         (result (fountain-parse source)))
    (is (listp result))))

;;; Transition Tests
(test fountain-transition-fade
  "Test FADE transition."
  (let* ((source "FADE TO BLACK")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-transition-cut
  "Test CUT transition."
  (let* ((source "CUT TO")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-transition-dissolve
  "Test DISSOLVE transition."
  (let* ((source "DISSOLVE TO")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-transition-wipe
  "Test WIPE transition."
  (let* ((source "WIPE")
         (result (fountain-parse source)))
    (is (listp result))))

;;; Page Break Tests
(test fountain-page-break-explicit
  "Test explicit page break."
  (let* ((source "Hero enters.\n\nPAGE BREAK\n\nINT DUNGEON - LATER")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-page-break-dash
  "Test triple-dash page break."
  (let* ((source "Hero enters.\n\n---\n\nINT NEW SCENE")
         (result (fountain-parse source)))
    (is (listp result))))

;;; Note Tests
(test fountain-note-simple
  "Test simple note."
  (let* ((source "[[ This is the opening scene ]]")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-note-todo
  "Test TODO note."
  (let* ((source "[[ TODO: Add explosion sound ]]")
         (result (fountain-parse source)))
    (is (listp result))))

(test fountain-note-effect
  "Test effect note."
  (let* ((source "[[ Trigger dramatic lighting ]]")
         (result (fountain-parse source)))
    (is (listp result))))

;;; =============================================================================
;;; BURGERMISTRESS LANGUAGE TESTS
;;; =============================================================================

(def-suite :burgermistress-comprehensive
  :description "Comprehensive Burgermistress language statement tests")

(in-suite :burgermistress-comprehensive)

;;; Variable Declaration Tests
(test burgermistress-dim-simple
  "Test simple variable declaration."
  (let* ((source "DIM PlayerHealth")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-dim-with-type
  "Test DIM with type annotation."
  (let* ((source "DIM Damage AS INTEGER")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-dim-with-init
  "Test DIM with initialization."
  (let* ((source "DIM MaxHealth = 100")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-dim-numeric-types
  "Test DIM with different numeric types."
  (let* ((source "DIM Flags = 0B1010\nDIM Count = 0\nDIM MaxVal = 255")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-multiple-dim
  "Test multiple variable declarations."
  (let* ((source "DIM Experience = 0\nDIM Level = 1\nDIM Gold = 0")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; Assignment Tests
(test burgermistress-assignment-simple
  "Test simple assignment."
  (let* ((source "Health = 100")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-assignment-expression
  "Test assignment with expression."
  (let* ((source "TotalDamage = WeaponDamage + BonusDamage")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-assignment-numeric-types
  "Test assignment with different numeric types."
  (let* ((source "Flags = 0B1010")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-assignment-conditional
  "Test assignment in conditional."
  (let* ((source "IF Health > 0 THEN\nNewHealth = Health - 25\nEND IF")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-assignment-loop
  "Test assignment in loop."
  (let* ((source "Counter = 0\nDO WHILE Counter < 10\nCounter = Counter + 1\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; IF-THEN-ELSE Tests
(test burgermistress-if-simple
  "Test simple IF."
  (let* ((source "IF Health > 0 THEN\nPRINT \"You are alive\"\nEND IF")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-if-else
  "Test IF-ELSE."
  (let* ((source "IF Health > 0 THEN\nPRINT \"Alive\"\nELSE\nPRINT \"Dead\"\nEND IF")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-if-nested
  "Test nested IF."
  (let* ((source "IF PlayerAlive THEN\nIF HasWeapon THEN\nPRINT \"Ready\"\nEND IF\nEND IF")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-if-complex-condition
  "Test IF with complex condition."
  (let* ((source "IF Health > 50 AND Mana > 20 THEN\nCastSpell(\"Fireball\")\nEND IF")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-if-multiple-branches
  "Test IF-ELSE IF-ELSE chain."
  (let* ((source "IF Health < 25 THEN\nPRINT \"Critical\"\nELSE IF Health < 50 THEN\nPRINT \"Wounded\"\nELSE\nPRINT \"Healthy\"\nEND IF")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; DO-LOOP Tests
(test burgermistress-do-loop-simple
  "Test simple DO-LOOP."
  (let* ((source "DO\nUpdateGame()\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-do-while
  "Test DO WHILE loop."
  (let* ((source "Counter = 0\nDO WHILE Counter < 10\nPRINT Counter\nCounter = Counter + 1\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-do-until
  "Test DO UNTIL loop."
  (let* ((source "DO UNTIL PlayerDead\nUpdateEnemies()\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-do-loop-break
  "Test DO-LOOP with EXIT DO."
  (let* ((source "Counter = 0\nDO\nProcess(Counter)\nCounter = Counter + 1\nIF Counter >= 100 THEN\nEXIT DO\nEND IF\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-nested-loops
  "Test nested loops."
  (let* ((source "X = 0\nDO WHILE X < 10\nY = 0\nDO WHILE Y < 10\nProcessCell(X, Y)\nY = Y + 1\nLOOP\nX = X + 1\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; FOR Loop Tests
(test burgermistress-for-simple
  "Test simple FOR loop."
  (let* ((source "FOR I = 1 TO 10\nPRINT I\nNEXT")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-for-step
  "Test FOR with STEP."
  (let* ((source "FOR I = 0 TO 100 STEP 10\nProcess(I)\nNEXT I")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-for-backward
  "Test backward FOR loop."
  (let* ((source "FOR I = 10 TO 1 STEP -1\nPRINT I\nNEXT I")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-for-nested
  "Test nested FOR loops."
  (let* ((source "FOR X = 0 TO 9\nFOR Y = 0 TO 9\nGrid(X, Y) = 0\nNEXT Y\nNEXT X")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-for-numeric-types
  "Test FOR with different numeric types."
  (let* ((source "FOR I = 0 TO 255\nProcess(I)\nNEXT I")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; SUBROUTINE Tests
(test burgermistress-sub-simple
  "Test simple subroutine."
  (let* ((source "SUB PrintGreeting()\nPRINT \"Hello!\"\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-sub-parameters
  "Test subroutine with parameters."
  (let* ((source "SUB TakeDamage(Amount)\nHealth = Health - Amount\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-sub-multiple-params
  "Test subroutine with multiple parameters."
  (let* ((source "SUB CalculateDamage(Base, Modifier, Critical)\nTotal = Base + Modifier\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-sub-recursive
  "Test recursive subroutine."
  (let* ((source "SUB Factorial(N)\nIF N <= 1 THEN\nRETURN 1\nELSE\nRETURN N * Factorial(N - 1)\nEND IF\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-sub-local-vars
  "Test subroutine with local variables."
  (let* ((source "SUB ProcessEntity(Entity)\nLOCAL Health, MaxHealth, Damage\nHealth = GetHealth(Entity)\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; PRINT Tests
(test burgermistress-print-string
  "Test PRINT string literal."
  (let* ((source "PRINT \"Hello, World!\"")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-print-variable
  "Test PRINT variable."
  (let* ((source "PRINT Health")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-print-concatenation
  "Test PRINT with string concatenation."
  (let* ((source "PRINT \"Health: \" & Health & \" / \" & MaxHealth")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-print-expression
  "Test PRINT expression."
  (let* ((source "PRINT TotalDamage + BonusDamage")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-print-formatted
  "Test formatted PRINT."
  (let* ((source "PRINT \"Status: \" & Status & \" Level: \" & Level")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; INPUT Tests
(test burgermistress-input-simple
  "Test simple INPUT."
  (let* ((source "INPUT PlayerChoice")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-input-prompt
  "Test INPUT with prompt."
  (let* ((source "INPUT \"Enter your name: \"; PlayerName")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-input-numeric
  "Test numeric INPUT."
  (let* ((source "INPUT \"Choose a number: \"; SelectedNumber")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-input-multiple
  "Test multiple INPUT statements."
  (let* ((source "INPUT \"X: \"; X\nINPUT \"Y: \"; Y")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-input-in-loop
  "Test INPUT in loop."
  (let* ((source "DO\nINPUT \"Command: \"; Command\nIF Command = \"Q\" THEN\nEXIT DO\nEND IF\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; SELECT-CASE Tests
(test burgermistress-select-simple
  "Test simple SELECT."
  (let* ((source "SELECT GameState\nCASE PLAYING\nUpdateGame()\nCASE PAUSED\nShowMenu()\nEND SELECT")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-select-multiple-values
  "Test SELECT with multiple values per case."
  (let* ((source "SELECT ItemType\nCASE SWORD, AXE, MACE\nEquipWeapon(Item)\nCASE POTION, ELIXIR\nDrinkPotion(Item)\nEND SELECT")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-select-numeric
  "Test SELECT with numeric cases."
  (let* ((source "SELECT Difficulty\nCASE 1\nEnemyHealth = 50\nCASE 2\nEnemyHealth = 100\nEND SELECT")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-select-numeric-types
  "Test SELECT with binary numeric types."
  (let* ((source "SELECT Flags\nCASE 0B1010\nAction1()\nCASE 0B1111\nAction2()\nEND SELECT")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-select-else
  "Test SELECT with CASE ELSE."
  (let* ((source "SELECT EventType\nCASE PLAYER_DAMAGE\nProcessDamage()\nCASE ELSE\nLogEvent()\nEND SELECT")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; RETURN Tests
(test burgermistress-return-simple
  "Test simple RETURN."
  (let* ((source "SUB Test()\nIF Error THEN\nRETURN\nEND IF\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-return-value
  "Test RETURN with value."
  (let* ((source "SUB GetHealth()\nRETURN Health\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-return-conditional
  "Test RETURN in conditional."
  (let* ((source "SUB ValidatePlayer(Player)\nIF Player.Health <= 0 THEN\nRETURN 0\nELSE\nRETURN 1\nEND IF\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-return-multiple
  "Test multiple RETURNs."
  (let* ((source "SUB CalculateDamage(Weapon, Armor)\nIF Weapon = NULL THEN\nRETURN 0\nEND IF\nRETURN Weapon.Power - Armor\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-return-recursive
  "Test RETURN from recursive function."
  (let* ((source "SUB Sum(N)\nIF N <= 0 THEN\nRETURN 0\nELSE\nRETURN N + Sum(N - 1)\nEND IF\nEND SUB")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; EXIT Tests
(test burgermistress-exit-do
  "Test EXIT DO."
  (let* ((source "DO\nInput(Command)\nIF Command = \"QUIT\" THEN\nEXIT DO\nEND IF\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-exit-for
  "Test EXIT FOR."
  (let* ((source "FOR I = 0 TO 1000\nIF Found(I) THEN\nEXIT FOR\nEND IF\nNEXT")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-exit-nested-loops
  "Test EXIT in nested loops."
  (let* ((source "FOR X = 0 TO 9\nFOR Y = 0 TO 9\nIF Grid(X, Y) = TARGET THEN\nEXIT FOR\nEND IF\nNEXT Y\nNEXT X")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-exit-condition
  "Test EXIT with condition."
  (let* ((source "DO\nFrameCount = FrameCount + 1\nIF FrameCount > 6000 THEN\nEXIT DO\nEND IF\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-exit-search
  "Test EXIT in search loop."
  (let* ((source "FOR I = 0 TO MAX-ENTITIES\nIF Entity(I).Type = PLAYER THEN\nPlayerIndex = I\nEXIT FOR\nEND IF\nNEXT")
         (result (burgermistress-parse source)))
    (is (listp result))))

;;; CONTINUE Tests
(test burgermistress-continue-simple
  "Test simple CONTINUE."
  (let* ((source "FOR I = 0 TO 10\nIF I MOD 2 = 0 THEN\nCONTINUE\nEND IF\nPRINT I\nNEXT")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-continue-do
  "Test CONTINUE in DO loop."
  (let* ((source "DO\nIF SkipFrame THEN\nCONTINUE DO\nEND IF\nProcessFrame()\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-continue-nested
  "Test CONTINUE in nested loop."
  (let* ((source "FOR X = 0 TO 9\nFOR Y = 0 TO 9\nIF Grid(X, Y) = EMPTY THEN\nCONTINUE FOR\nEND IF\nProcessCell(X, Y)\nNEXT Y\nNEXT X")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-continue-filtering
  "Test CONTINUE for filtering."
  (let* ((source "FOR I = 0 TO ItemCount\nIF Item(I).Type = JUNK THEN\nCONTINUE FOR\nEND IF\nProcessItem(Item(I))\nNEXT I")
         (result (burgermistress-parse source)))
    (is (listp result))))

(test burgermistress-continue-skip-logic
  "Test CONTINUE skipping to next iteration."
  (let* ((source "DO WHILE Counter < 100\nCounter = Counter + 1\nIF Counter < 50 THEN\nCONTINUE DO\nEND IF\nProcessValue(Counter)\nLOOP")
         (result (burgermistress-parse source)))
    (is (listp result))))
