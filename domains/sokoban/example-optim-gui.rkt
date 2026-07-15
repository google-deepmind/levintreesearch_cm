#lang racket
#| Copyright 2023 DeepMind Technologies Limited.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.|#

(require lts-cm/domains/sokoban/sokoban
         lts-cm/domains/sokoban/gui
         lts-cm/cdb
         lts-cm/optimize
         lts-cm/misc
         timev)

(*timev?* #false) ; reduce verbosity

;; A few simple levels
(define soko-strs
  '("\
###########
#     @.$ #
#         #
###########"
    "\
###########
#      .  #
#   @  $  #
#         #
###########"
    "\
###########
#         #
# @    $  #
#      .  #
###########"
    "\
###########
#         #
# @    $  #
# .       #
###########"
    "\
###########
# .       #
# @    $  #
#         #
###########"
    "\
##########################
# .                      #
# @                   $  #
#                        #
##########################"))

"==========================================="
"=== 1) Search for solution trajectories ==="
"==========================================="

(displayln "\nLet's solve some simple Sokoban levels using LTS with context models")
(displayln "prior to any learning (aka breadth-first search).")
(ask-enter)

(define sokos (map string->sokoban soko-strs))

(define results
  (for/list ([soko (in-list sokos)] [str (in-list soko-strs)])
    (displayln "Solving:")
    (displayln str)
    (silent (solve-sokoban soko))))

(displayln "\nAll levels solved.")
(displayln "Expansions (search steps) per level:")
(map (λ (res) (dict-ref res 'expansions)) results)

(ask-enter)

"========================================================================"
"=== 2) Optimize the parameters of the policy for the found solutions ==="
"========================================================================"

(displayln "\nNow, let's optimize on all found solutions *except* the last one (kept for testing)")
(define optim (results->optimization (drop-right results 1)
                                     #:old-cdb (make-cdb n-actions)
                                     #:ε-low (*ε-low*)))

(display "\nOptimizing... ") (flush-output)
(silent (update-loop optim #:reg 10. #:Tmax 200))
(displayln "Done.")

(ask-enter)

"====================================================="
"=== 3) Search again with the new optimized policy ==="
"====================================================="

(displayln "Let's solve the same levels with the new optimized parameters.")

(ask-enter)

(define results2
  (for/list ([soko (in-list sokos)] [str (in-list soko-strs)] [i (in-naturals 1)])
    (displayln (if (= i (length sokos)) "Solving test level:" "Solving:"))
    (displayln str)
    (silent (solve-sokoban soko #:cdb (optimization-cdb optim)))))

(displayln "Expansions (search steps) per level:")
(map (λ (res) (dict-ref res 'expansions)) results2)

(ask-enter)

"============================================"
"=== 4) Visualize solution for test level ==="
"============================================"

(void
 (play-sokoban (last sokos)
               #:act-seq   (dict-ref (last results2) 'act-seq)
               #:pcond-seq (dict-ref (last results2) 'pcond-seq)))
