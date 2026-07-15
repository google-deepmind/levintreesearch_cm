#lang racket/base
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

(require global)

(provide *verbose?*
         when-verb)

(define-global:boolean *verbose?* #t "Display additional information")
;; #true by default in general, but the server changes it to #false by default

(define-syntax-rule (when-verb body ...)
  (when (*verbose?*)
    body ...))


