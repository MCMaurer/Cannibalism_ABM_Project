breed [infecteds infected]  
breed [uninfecteds uninfected]
breed [juveniles juvenile]
breed [inf-juveniles inf-juvenile]

juveniles-own [development-level]
inf-juveniles-own [development-level]

to setup
  clear-all
  set-default-shape turtles "square"
  create-uninfecteds initial-number-uninfecteds
    [ set color yellow   
      setxy random-pxcor random-pycor 
      ]
  create-infecteds initial-number-infecteds 
    [ set color red 
      setxy random-pxcor random-pycor ]
  set-default-shape juveniles "dot"
  set-default-shape inf-juveniles "dot"
  reset-ticks
  
  
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ask uninfecteds [move]
  if uninfected-cannibalize? [ask uninfecteds [cannibalize]]
  ask uninfecteds [reproduce]
  ask uninfecteds [death]
  
  ask infecteds [can-move]
  ask infecteds [infected-cannibalize]
  ifelse vertical-transmission [ask infecteds [inf-reproduce]]
     [ask infecteds [reproduce]]
  ask infecteds [infect]
  ask infecteds [inf-death]
  
  ask juveniles [juv-move]
  ask juveniles [set development-level development-level + 1]
  ask juveniles [mature]
  ask juveniles [juv-death]
  
  ask inf-juveniles [juv-move]
  ask inf-juveniles [set development-level development-level + 1]
  ask inf-juveniles [inf-mature]
  ask inf-juveniles [inf-juv-death]

  
  tick
  
  if infect-at-tick [run [timed-start-infection]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to timed-start-infection
  if ticks = infection-arrival-time [create-infecteds 1 [set color red setxy random-pxcor random-pycor]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to death
  ;;if random death-odds = 0 [die]
  if random-float 100 < death-odds [die]
end

to inf-death
  if random-float 100 < death-odds + inf-death-modifier [die]
end

to juv-death
  if random-float 100 < juv-death-odds [die]
end

to inf-juv-death
  if random-float 100 < death-odds + inf-juv-death-modifier [die]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; all these movement things could be set based on nearest neighbor as well, which is rad.

;; Different movement levels for uninfecteds, infecteds, and juveniles

to move
  if random-float 100 < MovementOdds [setxy xcor + random 2 - random 2 ycor + random 2 - random 2]
end

to can-move
  if random-float 100 < (MovementOdds + can-move-modifier) [setxy xcor + random 2 - random 2 ycor + random 2 - random 2]
end

to juv-move
  if random-float 100 < (MovementOdds + juv-move-modifier) [setxy xcor + random 2 - random 2 ycor + random 2 - random 2]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When encountering a juvenile, what are the odds you eat it?
;; Different levels for uninfecteds and infecteds

to cannibalize
  
let prey one-of turtles-here with [shape = "dot"]
  if prey != nobody [
  if random-float 100 < (cannibalism-level * ((count uninfecteds-on neighbors) + (count infecteds-on neighbors)))
    [ ask prey [ die ]]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to infected-cannibalize
  
  let prey one-of turtles-here with [shape = "dot"]
  if prey != nobody [
  if random-float 100 < inf-cannibalism-level [
     ask prey [ die ]]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you have an uninfected within one square of you, you infect them

;; is the one-of a problem here?

to infect 
  
  let infectee one-of uninfecteds-on neighbors
  if infectee != nobody
  [if random-float 1000 < infectious-level * (count uninfecteds-on neighbors) ;; this scales UP with how many uninfecteds-on neighbors there are.
    ;; it's sort of a workaround for the random selection of one-of the neighbors to be called infectee. Basically, if you have 8 neighbors,
    ;; it's 8x more likely that the one that gets selected gets infected, which mathematically works out to the same thing as all 8 of them having
    ;; some small chance of becoming infected
    [hatch-infecteds 1 [set color red] ask infectee [die]]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reproduce
  ifelse (((count uninfecteds-on neighbors) + (count infecteds-on neighbors)) != 0)
  [if random-float 100 < fecundity * (2 / ((count uninfecteds-on neighbors) + (count infecteds-on neighbors)))
  [hatch-juveniles 1 [set color pink]]]
  [if random-float 100 < fecundity
  [hatch-juveniles 1 [set color pink]]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to inf-reproduce
  ifelse (((count uninfecteds-on neighbors) + (count infecteds-on neighbors)) != 0)
    [if random-float 100 < (fecundity + inf-fecund-modifier) * (1 / ((count uninfecteds-on neighbors) + (count infecteds-on neighbors)))
      [ifelse random-float 100 < vert-trans-odds
        [hatch-inf-juveniles 1 [set color magenta]]
        [hatch-juveniles 1 [set color pink]]]]
    [if random-float 100 < (fecundity + inf-fecund-modifier)
      [ifelse random-float 100 < vert-trans-odds
        [hatch-inf-juveniles 1 [set color magenta]]
        [hatch-juveniles 1 [set color pink]]]]
  
  ;if random-float 100 < 100 * (2 / ((count uninfecteds-on neighbors) + (count infecteds-on neighbors)))[
  ;ifelse random-float 100 < vert-trans-odds
  ;[hatch-inf-juveniles 1 [set color magenta]]
  ;[hatch-juveniles 1 [set color pink]]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to mature
  if development-level >= maturation-time [
  hatch-uninfecteds 1 [set color yellow] die ]
  ;; nymphs reach reproductive age
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to inf-mature
  if development-level >= maturation-time + inf-mature-modifier [
  hatch-infecteds 1 [set color red] die ]
  ;; nymphs reach reproductive age
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; When encountering a juvenile, what are the odds you eat it?
;; Different levels for uninfecteds and infecteds

to density-cannibalize
  
  
  let prey one-of turtles-here with [shape = "dot"]
  if prey != nobody [
  if random-float 100 < (cannibalism-level * ((count uninfecteds-on neighbors) + (count infecteds-on neighbors)))
  ;;[if random (100 - (0 * ((count uninfecteds-on neighbors) + (count infecteds-on neighbors)))) = 0
    [ ask prey [ die ]]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
610
92
1050
553
16
16
13.030303030303031
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
5
49
226
82
initial-number-uninfecteds
initial-number-uninfecteds
0
300
220
1
1
NIL
HORIZONTAL

BUTTON
18
14
84
47
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
97
13
160
46
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
6
87
226
120
initial-number-infecteds
initial-number-infecteds
0
100
25
1
1
NIL
HORIZONTAL

SLIDER
231
48
446
81
cannibalism-level
cannibalism-level
0
3
0.75
0.05
1
NIL
HORIZONTAL

SLIDER
231
88
446
121
inf-cannibalism-level
inf-cannibalism-level
0
100
80
1
1
NIL
HORIZONTAL

SLIDER
230
126
448
159
infectious-level
infectious-level
0
100
60
1
1
NIL
HORIZONTAL

SWITCH
231
10
446
43
uninfected-cannibalize?
uninfected-cannibalize?
1
1
-1000

SLIDER
6
126
226
159
maturation-time
maturation-time
0
200
25
1
1
NIL
HORIZONTAL

SLIDER
6
165
225
198
death-odds
death-odds
0
10
1
.1
1
NIL
HORIZONTAL

SLIDER
7
245
226
278
MovementOdds
MovementOdds
0
100
3
1
1
NIL
HORIZONTAL

SLIDER
8
283
226
316
can-move-modifier
can-move-modifier
-50
50
0
1
1
NIL
HORIZONTAL

SLIDER
7
324
225
357
juv-move-modifier
juv-move-modifier
-50
50
0
1
1
NIL
HORIZONTAL

SLIDER
6
204
224
237
juv-death-odds
juv-death-odds
0
5
1.2
0.1
1
NIL
HORIZONTAL

SWITCH
457
11
645
44
infect-at-tick
infect-at-tick
1
1
-1000

SLIDER
458
49
645
82
infection-arrival-time
infection-arrival-time
0
10000
1700
100
1
NIL
HORIZONTAL

SWITCH
658
10
847
43
vertical-transmission
vertical-transmission
1
1
-1000

SLIDER
658
49
847
82
vert-trans-odds
vert-trans-odds
0
100
66
1
1
NIL
HORIZONTAL

BUTTON
461
91
605
124
INFECT!
create-infecteds 1[ set color red setxy random-pxcor random-pycor]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
3
362
433
569
plot 2
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [shape = \"square\"]"
"pen-1" 1.0 0 -2674135 true "" "plot count infecteds"
"pen-2" 1.0 0 -1184463 true "" "plot count uninfecteds"

SLIDER
232
165
450
198
inf-mature-modifier
inf-mature-modifier
-100
100
0
1
1
NIL
HORIZONTAL

SLIDER
233
203
405
236
inf-death-modifier
inf-death-modifier
0
10
0.3
.1
1
NIL
HORIZONTAL

SLIDER
233
241
428
274
inf-juv-death-modifier
inf-juv-death-modifier
0
100
0
1
1
NIL
HORIZONTAL

SLIDER
235
282
407
315
fecundity
fecundity
0
20
8
.1
1
NIL
HORIZONTAL

SLIDER
235
320
413
353
inf-fecund-modifier
inf-fecund-modifier
-20
20
-1
.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

A model for a virus that induces cannibalistic behavior in its host, where infected hosts cannibalize juveniles.

## HOW IT WORKS

Adults move around and create offspring, infected adults infect nearby uninfected adults, and infected adults cannibalize juveniles they encounter.

## HOW TO USE IT
Initial values that seem to be somewhat stable:

Initial uninfecteds: 50
Initial uninfecteds: 20
Maturation time: 159
Death-odds: 100
Uninfected-cannibalize?: on
Cannibalism-level: 20
Inf-cannibalism-level: 90
Infectious-level: 70
Movement-odds: 10
Can-move-modifier: 5
Juv-move-modifier: .5

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

Currently no transmission from infected juvenile to uninfected adult: could add

Currently no mating, but probably a pain to add. Maybe better to just assume that there is a threshold population level below which reproduction is highly unlikely. Although if females mate once and then lay eggs a bunch, it may not matter as much?





## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="test" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <exitCondition>count juveniles &gt; 10000</exitCondition>
    <metric>count juveniles</metric>
    <metric>count uninfecteds</metric>
    <metric>count infecteds</metric>
    <enumeratedValueSet variable="can-move-modifier">
      <value value="7.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectious-level">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-move-modifier">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-cannibalism-level">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-uninfecteds">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-odds">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-odds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cannibalism-level">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturation-time">
      <value value="159"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-infecteds">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uninfected-cannibalize?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ChangeMovementOdds" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="9000"/>
    <exitCondition>ticks &gt; 1800 and count infecteds = 0</exitCondition>
    <metric>count uninfecteds</metric>
    <metric>count infecteds</metric>
    <enumeratedValueSet variable="initial-number-infecteds">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturation-time">
      <value value="168"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-move-modifier">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="can-move-modifier">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-death-odds">
      <value value="84"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-uninfecteds">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uninfected-cannibalize?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infect-at-tick">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vertical-transmission">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="MovementOdds" first="20" step="1" last="40"/>
    <enumeratedValueSet variable="cannibalism-level">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectious-level">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-odds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-cannibalism-level">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infection-arrival-time">
      <value value="1600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vert-trans-odds">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ChangeCannModifier" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="9000"/>
    <exitCondition>ticks &gt; 1800 and count infecteds = 0</exitCondition>
    <metric>count uninfecteds</metric>
    <metric>count infecteds</metric>
    <enumeratedValueSet variable="initial-number-infecteds">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturation-time">
      <value value="168"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-move-modifier">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="can-move-modifier" first="1" step="0.1" last="5"/>
    <enumeratedValueSet variable="juv-death-odds">
      <value value="84"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-uninfecteds">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uninfected-cannibalize?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infect-at-tick">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vertical-transmission">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MovementOdds">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cannibalism-level">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectious-level">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-odds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-cannibalism-level">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infection-arrival-time">
      <value value="1600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vert-trans-odds">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sensitivity_infected-death-modifier" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>count uninfecteds</metric>
    <metric>count infecteds</metric>
    <enumeratedValueSet variable="infection-arrival-time">
      <value value="1700"/>
    </enumeratedValueSet>
    <steppedValueSet variable="inf-death-modifier" first="0" step="0.025" last="0.5"/>
    <enumeratedValueSet variable="juv-death-odds">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-move-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MovementOdds">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vert-trans-odds">
      <value value="66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturation-time">
      <value value="124"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-cannibalism-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectious-level">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-juv-death-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-uninfecteds">
      <value value="220"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-odds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-mature-modifier">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-fecund-modifier">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vertical-transmission">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cannibalism-level">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infect-at-tick">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-infecteds">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="can-move-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uninfected-cannibalize?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sensitivity_inf-fecund-modifier" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>count uninfecteds</metric>
    <metric>count infecteds</metric>
    <enumeratedValueSet variable="infection-arrival-time">
      <value value="1700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-death-modifier">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-death-odds">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-move-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MovementOdds">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vert-trans-odds">
      <value value="66"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maturation-time">
      <value value="124"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-cannibalism-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectious-level">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-juv-death-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-uninfecteds">
      <value value="220"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-odds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-mature-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="inf-fecund-modifier" first="-4" step="0.25" last="0"/>
    <enumeratedValueSet variable="vertical-transmission">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cannibalism-level">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infect-at-tick">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-infecteds">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="can-move-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uninfected-cannibalize?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sensitivity_maturation-time" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>count uninfecteds</metric>
    <metric>count infecteds</metric>
    <enumeratedValueSet variable="infection-arrival-time">
      <value value="1700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-death-modifier">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-death-odds">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-move-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MovementOdds">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vert-trans-odds">
      <value value="66"/>
    </enumeratedValueSet>
    <steppedValueSet variable="maturation-time" first="25" step="25" last="500"/>
    <enumeratedValueSet variable="inf-cannibalism-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectious-level">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-juv-death-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-uninfecteds">
      <value value="220"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-odds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-mature-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-fecund-modifier">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vertical-transmission">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cannibalism-level">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infect-at-tick">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-infecteds">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="can-move-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uninfected-cannibalize?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sensitivity_maturation-time_FAST" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>repeat 10 [ go ]</go>
    <timeLimit steps="10000"/>
    <metric>count uninfecteds</metric>
    <metric>count infecteds</metric>
    <enumeratedValueSet variable="infection-arrival-time">
      <value value="1700"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-death-modifier">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-death-odds">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juv-move-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MovementOdds">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vert-trans-odds">
      <value value="66"/>
    </enumeratedValueSet>
    <steppedValueSet variable="maturation-time" first="25" step="25" last="500"/>
    <enumeratedValueSet variable="inf-cannibalism-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectious-level">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-juv-death-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-uninfecteds">
      <value value="220"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-odds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-mature-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-fecund-modifier">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vertical-transmission">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cannibalism-level">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infect-at-tick">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-infecteds">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fecundity">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="can-move-modifier">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uninfected-cannibalize?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
