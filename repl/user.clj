(ns user
  "REPL convenience namespace: all of commensura, ready to poke at. `commensura.core` is
  `:refer :all`-ed (so `by`/`per`/`to`/`ratio`/`span`/`ticks`/the comparison verbs/… are bare);
  every other namespace gets a short alias.

  Auto-loaded when a REPL starts with the `:repl` alias on the classpath:

    clojure -M:repl            # or point your editor's jack-in aliases at :repl

  Dev-only: `repl/` is on the `:repl` alias alone — never on `:test`/the build, never shipped."
  (:require [commensura.core :refer :all]
            [commensura.units :as u]
            [commensura.quantity :as q]
            [commensura.interval :as iv]
            [commensura.uncertain :as un]
            [commensura.math :as m]
            [commensura.registry :as reg]
            [commensura.dimensions :as dim]
            [commensura.discover :as disc]
            [commensura.infix :as fx :refer [fj $=]]     ; not infix/`to` — core/`to` is referred
            [commensura.temperature :as temp]
            [commensura.richter :as richter]
            [commensura.cpi :as cpi]
            [commensura.currency :as cur]
            [commensura.provenance :as p]
            [commensura.reader]))                        ; load so #commensura/unit + /quantity round-trip
