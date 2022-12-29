(ns synacor.challenge
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Helper functions

(defn error
  "Throw an exception"
  [fmt & args]
  (throw (Exception. (apply format fmt args))))

(defn bin-file
  "Load a binary file into little-endian 16-bit ints"
  [fname]
  (with-open [in (io/input-stream (io/file fname))]
    (let [buf (byte-array (* 2 32768))
          n (.read in buf)]
      (->> buf
           (partition 2)
           (map #(+ (bit-and 0xff (first %)) (* 0x100 (second %))))))))

;; Initial state of VM

(def init-vm
  {:running true
   :ip 0
   :mem (into (vector-of :int) (repeat 32768 0))
   :reg (into (vector-of :int) (repeat 8 0))
   :stack '()
   :input ""})

;; Get info from VM

(defn get-reg
  "Get value of a register"
  [vm reg]
  (if (<= 0 reg 7)
    (-> vm :reg (get reg))
    (error "Can't get invalid register: %s" reg)))

(defn get-num
  "Get value of a number that may be a reference to a register"
  [vm val]
  (if (< val 32768)
    val
    (get-reg vm (- val 32768))))

(defn stacktop
  "Get value at the top of the stack"
  [{:keys [stack]}]
  (if (empty? stack)
    (error "Can't pop empty stack")
    (first stack)))

;; Update VM

(defn load-mem
  "Load new values into memory"
  [vm data]
  (assoc vm :mem (into (vector-of :int) (map (partial bit-and 0xffff) data))))

(defn set-mem
  "Set a value in memory"
  [vm addr val]
  (assoc-in vm [:mem addr] val))

(defn set-reg
  "Set a register"
  [vm reg val]
  (if (> reg 32767)
    (recur vm (- reg 32768) val)
    (if (<= 0 reg 7)
      (assoc-in vm [:reg reg] (mod val 32768))
      (error "Can't set invalid register: %s" reg))))

(defn add-input
  [vm text]
  (update vm :input str text))

(defn ensure-input
  "Ensure there is input available to consume (with `in` instruction)"
  [vm]
  (if (empty? (:input vm))
    (-> vm
        (add-input (read-line))
        (add-input "\n"))
    vm))

;; Defining the workings of the VM

(defn oparg
  "Format an argument in a disassembly listing"
  [x]
  (cond
    (< x 32)    (format "0x%02x /* %s */" x x)
    (< x 255)   (format "'%s' /* 0x%02x / %3s */" (char x) x x)
    (< x 32768) (format "0x%04x /* %s */" x x)
    (< x 32776) (format "r%s " (- x 32768))
    :else       (format "0x%04x /* %s (BAD) */" x x)))

(defn disasm-op
  "Format the disassembly representation of an operation"
  [op vm & args]
  (println (format "0x%04x: %-4s %s" (:ip vm) op (str/join " " (map oparg args)))))

;; Map of available VM operations
(def ops (atom {}))

(defmacro defop
  "Define a VM operation"
  [opcode opname args & body]
  (let [opfn# (symbol (str "op-" opname))
        disfn# (symbol (str "dis-" opname))]
    `(do
       (defn ~opfn# ~args
         (let [~'vm (update ~'vm :ip + ~(count args))]
           ~@body))
       (defn ~disfn# [~'vm ~'& ~'args]
         (apply disasm-op ~(str opname) ~'vm ~'args)
         (update ~'vm :ip + ~(count args)))
       (swap! ops assoc ~opcode {:nargs ~(dec (count args))
                                 :exec ~opfn#
                                 :disasm ~disfn#}))))

;; Define all the opcodes and their operations on the VM

(defop 0  halt [vm] (assoc vm :running false))
(defop 1  set  [vm a b] (set-reg vm a (get-num vm b)))
(defop 2  push [vm a] (update vm :stack conj (get-num vm a)))
(defop 3  pop  [vm a] (-> vm
                          (set-reg a (stacktop vm))
                          (update :stack rest)))
(defop 4  eq   [vm a b c] (set-reg vm a (if (= (get-num vm b) (get-num vm c)) 1 0)))
(defop 5  gt   [vm a b c] (set-reg vm a (if (> (get-num vm b) (get-num vm c)) 1 0)))
(defop 6  jmp  [vm a]     (assoc vm :ip (get-num vm a)))
(defop 7  jt   [vm a b]   (if (zero? (get-num vm a)) vm (assoc vm :ip (get-num vm b))))
(defop 8  jf   [vm a b]   (if (zero? (get-num vm a)) (assoc vm :ip (get-num vm b)) vm))
(defop 9  add  [vm a b c] (set-reg vm a (+ (get-num vm b) (get-num vm c))))
(defop 10 mult [vm a b c] (set-reg vm a (* (get-num vm b) (get-num vm c))))
(defop 11 mod  [vm a b c] (set-reg vm a (mod (get-num vm b) (get-num vm c))))
(defop 12 and  [vm a b c] (set-reg vm a (bit-and (get-num vm b) (get-num vm c))))
(defop 13 or   [vm a b c] (set-reg vm a (bit-or (get-num vm b) (get-num vm c))))
(defop 14 not  [vm a b]   (set-reg vm a (bit-not (get-num vm b))))
(defop 15 rmem [vm a b]   (set-reg vm a (get-in vm [:mem (get-num vm b)])))
(defop 16 wmem [vm a b]   (assoc-in vm [:mem (get-num vm a)] (get-num vm b)))
(defop 17 call [vm a]     (-> vm
                              (update :stack conj (:ip vm))
                              (assoc :ip (get-num vm a))))
(defop 18 ret  [vm]       (-> vm
                              (assoc :ip (stacktop vm))
                              (update :stack rest)))
(defop 19 out  [vm a]     (let [c (char (get-num vm a))]
                            (print c)
                            (when (= c \newline) (flush))
                            vm))
(defop 20 in   [vm a]     (let [vm (ensure-input vm)]
                            (-> vm
                                (set-reg a (int (first (:input vm))))
                                (update :input #(subs % 1)))))
(defop 21 noop [vm]       vm)

(defn step
  "Advance the VM by one operation"
  [{:keys [ip mem] :as vm} key]
  (let [opcode (get mem ip)]
    (if-let [{:keys [nargs]} (@ops opcode)]
      (let [func (get (@ops opcode) key)]
        (apply func
               vm
               (subvec mem (inc ip) (+ (inc ip) nargs))))
      (do
        (when (= key :disasm)
          (println (format "0x%04x: 0x%04x /* %s */" (:ip vm) opcode opcode)))
        (update vm :ip inc)
        #_(error "Unrecognised opcode: %s" opcode)))))

(defn exec
  "Execute code in memory of the VM"
  [vm]
  (if (:running vm)
    (recur (step vm :exec))
    (println (format "Stopping at %04x" (:ip vm)))))

(defn debug
  "Execute code in memory of the VM one instruction at a time, displaying VM state"
  [vm]
  (println (format (str "\nVM:\n"
                        "  IP: %04x (%s)\n"
                        "  Regs: %s\n"
                        "  Stack: %s\n")
                   (:ip vm) (if (:running vm) "running" "halted")
                   (mapv #(format "%04x" %) (:reg vm))
                   (mapv #(format "%04x" %) (:stack vm))))
  (step vm :disasm)
  (when (and (:running vm) (not (str/starts-with? (read-line) "q")))
    (recur (step vm :exec))))

(defn disasm
  "Print out disassembly of code in VM memory"
  [vm]
  (when (< (:ip vm) (count (:mem vm)))
    (recur (step vm :disasm))))

(def cmds
  {"exec" exec
   "debug" debug
   "disasm" disasm})

(defn -main
  [& [cmd fname]]
  (if-let [func (cmds cmd)]
    (try
      (-> init-vm
          (load-mem (bin-file (or fname "challenge.bin")))
          (set-reg 7 25734)   ;; Special teleporter value
          (set-mem 0x0209 21) ;; Remove r7 zero check
          (set-mem 0x020a 21) ;; cont
          (set-mem 0x020b 21) ;; cont
          (set-mem 0x156d 6)  ;; Set r0 to expected return value
          (set-mem 0x1571 21) ;; Remove call to expensive func
          (set-mem 0x1572 21) ;; cont
          (add-input "take tablet\n")
          (add-input "use tablet\n")
          (add-input "doorway\n")
          (add-input "north\n")
          (add-input "north\n")
          (add-input "bridge\n")
          (add-input "continue\n")
          (add-input "down\n")
          (add-input "east\n")
          (add-input "take empty lantern\n")
          (add-input "west\n")
          (add-input "west\n")
          (add-input "passage\n")
          (add-input "ladder\n")
          (add-input "west\n")
          (add-input "south\n")
          (add-input "north\n")
          (add-input "take can\n")
          (add-input "use can\n")
          (add-input "use lantern\n")
          (add-input "west\n")
          (add-input "ladder\n")
          (add-input "darkness\n")
          (add-input "continue\n")
          (add-input "west\n")
          (add-input "west\n")
          (add-input "west\n")
          (add-input "west\n")
          (add-input "north\n")
          (add-input "take red coin\n")
          (add-input "north\n") ;; doorway
          (add-input "east\n")
          (add-input "take concave coin\n")
          (add-input "down\n")
          (add-input "take corroded coin\n")
          (add-input "up\n")
          (add-input "west\n") ;; doorway
          (add-input "west\n")
          (add-input "take blue coin\n")
          (add-input "up\n")
          (add-input "take shiny coin\n")
          (add-input "down\n")
          (add-input "east\n") ;; doorway
          (add-input "use blue coin\n")
          (add-input "use red coin\n")
          (add-input "use shiny coin\n")
          (add-input "use concave coin\n")
          (add-input "use corroded coin\n")
          (add-input "north\n")
          (add-input "take teleporter\n")
          (add-input "use teleporter\n")
          ;;(add-input "take business card\n")
          ;;(add-input "take strange book\n")
          ;;(add-input "look strange book\n")
          (add-input (apply str (repeat 7 "north\n")))
          (add-input "east\ntake journal\nwest\n")
          (add-input (apply str (repeat 2 "north\n")))
          (add-input "take orb\n")     ;; 22
          (add-input "east\nnorth\n")  ;; - 4 = 18
          (add-input "north\nsouth\n") ;; * 4 = 72
          (add-input "east\neast\n")   ;; - 18 = 54
          (add-input "west\nwest\n")   ;; - 4 = 50
          (add-input "east\nnorth\n")  ;; - 11 = 39
          (add-input "north\nwest\n")  ;; - 8 = 31
          (add-input "east\neast\n")   ;; - 1 = 30
          (func))
      (finally
        (println)
        (flush)))
    (println (format "Usage: clj -M:run <cmd> [filename]\nWhere <cmd> = %s" (str/join "/" (sort (keys cmds)))))))
