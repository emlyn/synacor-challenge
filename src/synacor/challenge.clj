(ns synacor.challenge
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def init-vm
  {:running true
   :ip 0
   :mem (into (vector-of :int) (repeat 32768 0))
   :reg (into (vector-of :int) (repeat 8 0))
   :stack '()
   :input ""})

(defn set-mem
  [vm data]
  (assoc vm :mem (into (vector-of :int) (map (partial bit-and 0xffff) data))))

(defn bin-file
  [fname]
  (with-open [in (io/input-stream (io/file fname))]
    (let [buf (byte-array (* 2 32768))
          n (.read in buf)]
      (->> buf
           (partition 2)
           (map #(+ (bit-and 0xff (first %)) (* 0x100 (second %))))))))

(defn error
  [fmt & args]
  (throw (Exception. (apply format fmt args))))

(defn set-reg [vm reg val]
  (if (> reg 32767)
    (recur vm (- reg 32768) val)
    (if (<= 0 reg 7)
      (assoc-in vm [:reg reg] (mod val 32768))
      (error "Can't set invalid register: %s" reg))))

(defn get-reg [vm reg]
  (if (<= 0 reg 7)
    (-> vm :reg (get reg))
    (error "Can't get invalid register: %s" reg)))

(defn get-num
  [vm val]
  (if (< val 32768)
    val
    (get-reg vm (- val 32768))))

(defn ensure-input
  [vm]
  (if (empty? (:input vm))
    (assoc vm :input (str (read-line) \newline))
    vm))

(defn stacktop [{:keys [stack]}]
  (if (empty? stack)
    (error "Can't pop empty stack")
    (first stack)))

(defn oparg [x]
  (cond
    (< x 32)    (format "0x%02x /* %s */" x x)
    (< x 255)   (format "'%s' /* 0x%02x / %3s */" (char x) x x)
    (< x 32768) (format "0x%04x /* %s */" x x)
    (< x 32776) (format "r%s " (- x 32768))
    :else       (format "0x%04x /* %s (BAD) */" x x)))

(defn disasm-op [op vm & args]
  (println (format "0x%04x: %-4s %s" (:ip vm) op (str/join " " (map oparg args)))))

(def ops (atom {}))

(defmacro defop
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

(do
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
  (defop 21 noop [vm]       vm))

(defn step
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

(defn exec [vm]
  (when (:running vm)
    (recur (step vm :exec))))

(defn debug [vm]
  (println (format (str "\nVM:\n"
                        "  IP: %04x\n"
                        "  Regs: %s\n"
                        "  Stack: %s\n")
                   (:ip vm)
                   (mapv #(format "%04x" %) (:reg vm))
                   (mapv #(format "%04x" %) (:stack vm))))
  (step vm :disasm)
  (when (and (:running vm) (not (str/starts-with? (read-line) "q")))
    (recur (step vm :exec))))

(defn disasm [vm]
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
          (set-mem (bin-file (or fname "challenge.bin")))
          (func))
      (finally
        (println)
        (flush)))
    (println (format "Usage: clj -M:run <cmd> [filename]\nWhere <cmd> = %s" (str/join "/" (sort (keys cmds)))))))
