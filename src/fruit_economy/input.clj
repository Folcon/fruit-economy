(ns fruit-economy.input
  (:import [io.github.humbleui.jwm EventMouseButton MouseButton EventKey Key KeyModifier]))


(defn mouse-button->kw [mouse-button]
  (condp = mouse-button
    MouseButton/PRIMARY :mouse-button/left-mouse-button
    MouseButton/SECONDARY :mouse-button/right-mouse-button
    MouseButton/MIDDLE :mouse-button/middle-mouse-button
    MouseButton/FORWARD :mouse-button/forward-mouse-button
    MouseButton/BACK :mouse-button/back-mouse-button))

(defn mouse-button-modifiers->kw [raw-button]
  {:key/alt (.isModifierDown ^EventMouseButton raw-button KeyModifier/ALT)
   :key/control (.isModifierDown ^EventMouseButton raw-button KeyModifier/CONTROL)
   :key/caps-lock (.isModifierDown ^EventMouseButton raw-button KeyModifier/CAPS_LOCK)
   :key/shift (.isModifierDown ^EventMouseButton raw-button KeyModifier/SHIFT)
   :key/win-logo (.isModifierDown ^EventMouseButton raw-button KeyModifier/WIN_LOGO)
   :key/linux-meta (.isModifierDown ^EventMouseButton raw-button KeyModifier/LINUX_META)
   :key/linux-super (.isModifierDown ^EventMouseButton raw-button KeyModifier/LINUX_SUPER)
   :key/mac-command (.isModifierDown ^EventMouseButton raw-button KeyModifier/MAC_COMMAND)
   :key/mac-option (.isModifierDown ^EventMouseButton raw-button KeyModifier/MAC_OPTION)
   :key/mac-fn (.isModifierDown ^EventMouseButton raw-button KeyModifier/MAC_FN)})

(defn key->kw [key]
  (condp = key
    Key/UNDEFINED :key/undefined
    Key/CAPS_LOCK :key/caps-lock
    Key/SHIFT :key/shift
    Key/CONTROL :key/control
    Key/ALT :key/alt
    Key/WIN_LOGO :key/win-logo
    Key/LINUX_META :key/linux-meta
    Key/LINUX_SUPER :key/linux-super
    Key/MAC_COMMAND :key/mac-command
    Key/MAC_OPTION :key/mac-option
    Key/MAC_FN :key/mac-fn
    Key/ENTER :key/enter
    Key/BACKSPACE :key/backspace
    Key/TAB :key/tab
    Key/CANCEL :key/cancel
    Key/CLEAR :key/clear
    Key/PAUSE :key/pause
    Key/ESCAPE :key/escape
    Key/SPACE :key/space
    Key/PAGE_UP :key/page-up
    Key/PAGE_DOWN :key/page-down
    Key/END :key/end
    Key/HOME :key/home
    Key/LEFT :key/left
    Key/UP :key/up
    Key/RIGHT :key/right
    Key/DOWN :key/down
    Key/COMMA :key/comma
    Key/MINUS :key/minus
    Key/PERIOD :key/period
    Key/SLASH :key/slash
    Key/DIGIT0 :key/digit0
    Key/DIGIT1 :key/digit1
    Key/DIGIT2 :key/digit2
    Key/DIGIT3 :key/digit3
    Key/DIGIT4 :key/digit4
    Key/DIGIT5 :key/digit5
    Key/DIGIT6 :key/digit6
    Key/DIGIT7 :key/digit7
    Key/DIGIT8 :key/digit8
    Key/DIGIT9 :key/digit9
    Key/SEMICOLON :key/semicolon
    Key/EQUALS :key/equals
    Key/A :key/a
    Key/B :key/b
    Key/C :key/c
    Key/D :key/d
    Key/E :key/e
    Key/F :key/f
    Key/G :key/g
    Key/H :key/h
    Key/I :key/i
    Key/J :key/j
    Key/K :key/k
    Key/L :key/l
    Key/M :key/m
    Key/N :key/n
    Key/O :key/o
    Key/P :key/p
    Key/Q :key/q
    Key/R :key/r
    Key/S :key/s
    Key/T :key/t
    Key/U :key/u
    Key/V :key/v
    Key/W :key/w
    Key/X :key/x
    Key/Y :key/y
    Key/Z :key/z
    Key/OPEN_BRACKET :key/open-bracket
    Key/BACK_SLASH :key/back-slash
    Key/CLOSE_BRACKET :key/close-bracket
    Key/MULTIPLY :key/multiply
    Key/ADD :key/add
    Key/SEPARATOR :key/separator
    Key/DELETE :key/delete
    Key/NUM_LOCK :key/num-lock
    Key/SCROLL_LOCK :key/scroll-lock
    Key/F1 :key/f1
    Key/F2 :key/f2
    Key/F3 :key/f3
    Key/F4 :key/f4
    Key/F5 :key/f5
    Key/F6 :key/f6
    Key/F7 :key/f7
    Key/F8 :key/f8
    Key/F9 :key/f9
    Key/F10 :key/f10
    Key/F11 :key/f11
    Key/F12 :key/f12
    Key/F13 :key/f13
    Key/F14 :key/f14
    Key/F15 :key/f15
    Key/F16 :key/f16
    Key/F17 :key/f17
    Key/F18 :key/f18
    Key/F19 :key/f19
    Key/F20 :key/f20
    Key/F21 :key/f21
    Key/F22 :key/f22
    Key/F23 :key/f23
    Key/F24 :key/f24
    Key/PRINTSCREEN :key/printscreen
    Key/INSERT :key/insert
    Key/HELP :key/help
    Key/BACK_QUOTE :key/back-quote
    Key/QUOTE :key/quote
    Key/MENU :key/menu
    Key/KANA :key/kana
    Key/VOLUME_UP :key/volume-up
    Key/VOLUME_DOWN :key/volume-down
    Key/MUTE :key/mute))

(defn key-modifiers->kw [raw-key]
  {:key/alt  (.isModifierDown ^EventKey raw-key KeyModifier/ALT)
   :key/control (.isModifierDown ^EventKey raw-key KeyModifier/CONTROL)
   :key/caps-lock (.isModifierDown ^EventKey raw-key KeyModifier/CAPS_LOCK)
   :key/shift (.isModifierDown ^EventKey raw-key KeyModifier/SHIFT)
   :key/win-logo (.isModifierDown ^EventKey raw-key KeyModifier/WIN_LOGO)
   :key/linux-meta (.isModifierDown ^EventKey raw-key KeyModifier/LINUX_META)
   :key/linux-super (.isModifierDown ^EventKey raw-key KeyModifier/LINUX_SUPER)
   :key/mac-command (.isModifierDown ^EventKey raw-key KeyModifier/MAC_COMMAND)
   :key/mac-option (.isModifierDown ^EventKey raw-key KeyModifier/MAC_OPTION)
   :key/mac-fn (.isModifierDown ^EventKey raw-key KeyModifier/MAC_FN)})

(defn key-mask->key-type [key-mask]
  {:function-key? (.isFunctionKey ^Key key-mask)
   :navigation-key? (.isNavigationKey ^Key key-mask)
   :arrow-key? (.isArrowKey ^Key key-mask)
   :modifier-key? (.isModifierKey ^Key key-mask)
   :digit-key? (.isDigitKey ^Key key-mask)
   :whitespace-key? (.isWhitespaceKey ^Key key-mask)
   :media-key? (.isMediaKey ^Key key-mask)})
