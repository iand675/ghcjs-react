{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FunctionalDependencies #-}
module React.DOM where
import qualified Data.Foldable as F
import Data.Proxy
import qualified Data.JSString as JSString
import JavaScript.Object (Object)
import GHCJS.Foreign
import GHCJS.Marshal.Pure
import GHCJS.Types
import React

foreign import javascript "React['DOM'][$1].apply(this, [$2].concat($3))" js_elem :: JSString -> Props ps -> JSVal -> ReactNode
mkElem :: (Applicative t, Foldable t, F.Foldable elems) => JSString -> t Prop -> elems ReactNode -> ReactNode
mkElem str ps c = js_elem str (buildProps ps) (if Prelude.null c then jsNull else pToJSVal $ array $ F.toList c)

mkEmptyElem :: (Applicative t, Foldable t) => JSString -> t Prop -> ReactNode
mkEmptyElem str ps = js_elem str (buildProps ps) jsUndefined

class ElementOrProp f t where
  symbolName :: JSString -> (f, Proxy t)

instance (Applicative t, Foldable t, Foldable elems) => ElementOrProp (t Prop -> elems ReactNode -> ReactNode) a where
  symbolName n = (mkElem n, Proxy)

instance (Applicative t, Foldable t) => ElementOrProp (t Prop -> ReactNode) a where
  symbolName n = (mkEmptyElem n, Proxy)

instance ElementOrProp (PropName t) t where
  symbolName n = (PropName n, Proxy)

instance PToJSVal t => ElementOrProp (t -> Prop) t where
  symbolName n = ((.:) (PropName n :: PropName t), Proxy)

class IsProp f t | f -> t where
  mkProp :: JSString -> f

instance IsProp (PropName t) t where
  mkProp = PropName
instance PToJSVal t => IsProp (t -> Prop) t where
  mkProp str = (.:) (PropName str :: PropName t)

a_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
a_ = mkElem "a"

abbr_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
abbr_ = mkElem "abbr"

address_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
address_ = mkElem "address"

area_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
area_ = mkEmptyElem "area"

article_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
article_ = mkElem "article"

aside_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
aside_ = mkElem "aside"

audio_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
audio_ = mkElem "audio"

b_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
b_ = mkElem "b"

base_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
base_ = mkEmptyElem "base"

bdi_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
bdi_ = mkElem "bdi"

bdo_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
bdo_ = mkElem "bdo"

big_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
big_ = mkElem "big"

blockquote_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
blockquote_ = mkElem "blockquote"

body_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
body_ = mkElem "body"

br_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
br_ = mkEmptyElem "br"

button_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
button_ = mkElem "button"

canvas_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
canvas_ = mkElem "canvas"

caption_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
caption_ = mkElem "caption"

circle_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
circle_ = mkElem "circle"

clipPath_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
clipPath_ = mkElem "clipPath"

code_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
code_ = mkElem "code"

col_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
col_ = mkEmptyElem "col"

colgroup_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
colgroup_ = mkElem "colgroup"

datalist_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
datalist_ = mkElem "datalist"

dd_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
dd_ = mkElem "dd"

defs_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
defs_ = mkElem "defs"

del_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
del_ = mkElem "del"

details_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
details_ = mkElem "details"

dfn_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
dfn_ = mkElem "dfn"

dialog_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
dialog_ = mkElem "dialog"

div_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
div_ = mkElem "div"

dl_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
dl_ = mkElem "dl"

dt_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
dt_ = mkElem "dt"

ellipse_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
ellipse_ = mkElem "ellipse"

em_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
em_ = mkElem "em"

embed_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
embed_ = mkEmptyElem "embed"

fieldset_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
fieldset_ = mkElem "fieldset"

figcaption_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
figcaption_ = mkElem "figcaption"

figure_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
figure_ = mkElem "figure"

footer_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
footer_ = mkElem "footer"

g_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
g_ = mkElem "g"

h1_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
h1_ = mkElem "h1"

h2_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
h2_ = mkElem "h2"

h3_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
h3_ = mkElem "h3"

h4_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
h4_ = mkElem "h4"

h5_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
h5_ = mkElem "h5"

h6_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
h6_ = mkElem "h6"

head_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
head_ = mkElem "head"

header_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
header_ = mkElem "header"

hgroup_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
hgroup_ = mkElem "hgroup"

hr_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
hr_ = mkEmptyElem "hr"

html_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
html_ = mkElem "html"

i_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
i_ = mkElem "i"

iframe_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
iframe_ = mkElem "iframe"

image_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
image_ = mkElem "image"

img_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
img_ = mkEmptyElem "img"

input_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
input_ = mkEmptyElem "input"

ins_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
ins_ = mkElem "ins"

kbd_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
kbd_ = mkElem "kbd"

keygen_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
keygen_ = mkEmptyElem "keygen"

legend_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
legend_ = mkElem "legend"

li_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
li_ = mkElem "li"

line_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
line_ = mkElem "line"

linearGradient_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
linearGradient_ = mkElem "linearGradient"

link_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
link_ = mkEmptyElem "link"

main_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
main_ = mkElem "main"

map_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
map_ = mkElem "map"

mark_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
mark_ = mkElem "mark"

menu_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
menu_ = mkElem "menu"

menuitem_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
menuitem_ = mkEmptyElem "menuitem"

meta_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
meta_ = mkEmptyElem "meta"

meter_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
meter_ = mkElem "meter"

nav_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
nav_ = mkElem "nav"

noscript_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
noscript_ = mkElem "noscript"

object_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
object_ = mkElem "object"

ol_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
ol_ = mkElem "ol"

optgroup_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
optgroup_ = mkElem "optgroup"

option_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
option_ = mkElem "option"

output_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
output_ = mkElem "output"

p_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
p_ = mkElem "p"

param_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
param_ = mkEmptyElem "param"

path_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
path_ = mkElem "path"

picture_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
picture_ = mkElem "picture"

polygon_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
polygon_ = mkElem "polygon"

polyline_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
polyline_ = mkElem "polyline"

pre_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
pre_ = mkElem "pre"

progress_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
progress_ = mkElem "progress"

q_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
q_ = mkElem "q"

radialGradient_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
radialGradient_ = mkElem "radialGradient"

rect_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
rect_ = mkElem "rect"

rp_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
rp_ = mkElem "rp"

rt_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
rt_ = mkElem "rt"

ruby_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
ruby_ = mkElem "ruby"

s_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
s_ = mkElem "s"

samp_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
samp_ = mkElem "samp"

script_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
script_ = mkElem "script"

section_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
section_ = mkElem "section"

select_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
select_ = mkElem "select"

small_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
small_ = mkElem "small"

source_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
source_ = mkEmptyElem "source"

stop_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
stop_ = mkElem "stop"

strong_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
strong_ = mkElem "strong"

sub_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
sub_ = mkElem "sub"

svg_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
svg_ = mkElem "svg"

table_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
table_ = mkElem "table"

tbody_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
tbody_ = mkElem "tbody"

td_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
td_ = mkElem "td"

text_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
text_ = mkElem "text"

textarea_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
textarea_ = mkElem "textarea"

tfoot_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
tfoot_ = mkElem "tfoot"

th_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
th_ = mkElem "th"

thead_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
thead_ = mkElem "thead"

time_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
time_ = mkElem "time"

tr_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
tr_ = mkElem "tr"

track_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
track_ = mkEmptyElem "track"

tspan_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
tspan_ = mkElem "tspan"

u_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
u_ = mkElem "u"

ul_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
ul_ = mkElem "ul"

var_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
var_ = mkElem "var"

video_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactNode -> ReactNode
video_ = mkElem "video"

wbr_ :: (Applicative t, Foldable t) => t Prop -> ReactNode
wbr_ = mkEmptyElem "wbr"


accept_ :: IsProp p JSString => p
accept_ = mkProp "accept"

acceptCharset_ :: IsProp p JSString => p
acceptCharset_ = mkProp "acceptCharset"

accessKey_ :: IsProp p JSString => p
accessKey_ = mkProp "accessKey"

action_ :: IsProp p JSString => p
action_ = mkProp "action"

allowFullScreen_ :: IsProp p JSString => p
allowFullScreen_ = mkProp "allowFullScreen"

allowTransparency_ :: IsProp p JSString => p
allowTransparency_ = mkProp "allowTransparency"

alt_ :: IsProp p JSString => p
alt_ = mkProp "alt"

async_ :: IsProp p JSString => p
async_ = mkProp "async"

autoComplete_ :: IsProp p JSString => p
autoComplete_ = mkProp "autoComplete"

autoFocus_ :: IsProp p JSString => p
autoFocus_ = mkProp "autoFocus"

autoPlay_ :: IsProp p JSString => p
autoPlay_ = mkProp "autoPlay"

capture_ :: IsProp p JSString => p
capture_ = mkProp "capture"

cellPadding_ :: IsProp p JSString => p
cellPadding_ = mkProp "cellPadding"

cellSpacing_ :: IsProp p JSString => p
cellSpacing_ = mkProp "cellSpacing"

challenge_ :: IsProp p JSString => p
challenge_ = mkProp "challenge"

charSet_ :: IsProp p JSString => p
charSet_ = mkProp "charSet"

checked_ :: IsProp p Bool => p
checked_ = mkProp "checked"

cite_ :: ElementOrProp p JSString => p
cite_ = fst (symbolName "cite" :: ElementOrProp p JSString => (p, Proxy JSString))

classID_ :: IsProp p JSString => p
classID_ = mkProp "classID"

className_ :: IsProp p JSString => p
className_ = mkProp "className"

colSpan_ :: IsProp p JSString => p
colSpan_ = mkProp "colSpan"

cols_ :: IsProp p JSString => p
cols_ = mkProp "cols"

content_ :: IsProp p JSString => p
content_ = mkProp "content"

contentEditable_ :: IsProp p JSString => p
contentEditable_ = mkProp "contentEditable"

contextMenu_ :: IsProp p JSString => p
contextMenu_ = mkProp "contextMenu"

controls_ :: IsProp p JSString => p
controls_ = mkProp "controls"

coords_ :: IsProp p JSString => p
coords_ = mkProp "coords"

crossOrigin_ :: IsProp p JSString => p
crossOrigin_ = mkProp "crossOrigin"

data_ :: ElementOrProp p JSString => p
data_ = fst (symbolName "data" :: ElementOrProp p JSString => (p, Proxy JSString))

dateTime_ :: IsProp p JSString => p
dateTime_ = mkProp "dateTime"

default_ :: IsProp p JSString => p
default_ = mkProp "default"

defer_ :: IsProp p JSString => p
defer_ = mkProp "defer"

dir_ :: IsProp p JSString => p
dir_ = mkProp "dir"

disabled_ :: IsProp p Bool => p
disabled_ = mkProp "disabled"

download_ :: IsProp p JSString => p
download_ = mkProp "download"

draggable_ :: IsProp p JSString => p
draggable_ = mkProp "draggable"

encType_ :: IsProp p JSString => p
encType_ = mkProp "encType"

form_ :: ElementOrProp p JSString => p
form_ = fst (symbolName "form" :: ElementOrProp p JSString => (p, Proxy JSString))

formAction_ :: IsProp p JSString => p
formAction_ = mkProp "formAction"

formEncType_ :: IsProp p JSString => p
formEncType_ = mkProp "formEncType"

formMethod_ :: IsProp p JSString => p
formMethod_ = mkProp "formMethod"

formNoValidate_ :: IsProp p JSString => p
formNoValidate_ = mkProp "formNoValidate"

formTarget_ :: IsProp p JSString => p
formTarget_ = mkProp "formTarget"

frameBorder_ :: IsProp p JSString => p
frameBorder_ = mkProp "frameBorder"

headers_ :: IsProp p JSString => p
headers_ = mkProp "headers"

height_ :: IsProp p Int => p
height_ = mkProp "height"

hidden_ :: IsProp p Bool => p
hidden_ = mkProp "hidden"

high_ :: IsProp p JSString => p
high_ = mkProp "high"

href_ :: IsProp p JSString => p
href_ = mkProp "href"

hrefLang_ :: IsProp p JSString => p
hrefLang_ = mkProp "hrefLang"

htmlFor_ :: IsProp p JSString => p
htmlFor_ = mkProp "htmlFor"

httpEquiv_ :: IsProp p JSString => p
httpEquiv_ = mkProp "httpEquiv"

icon_ :: IsProp p JSString => p
icon_ = mkProp "icon"

id_ :: IsProp p JSString => p
id_ = mkProp "id"

inputMode_ :: IsProp p JSString => p
inputMode_ = mkProp "inputMode"

integrity_ :: IsProp p JSString => p
integrity_ = mkProp "integrity"

is_ :: IsProp p JSString => p
is_ = mkProp "is"

keyParams_ :: IsProp p JSString => p
keyParams_ = mkProp "keyParams"

keyType_ :: IsProp p JSString => p
keyType_ = mkProp "keyType"

kind_ :: IsProp p JSString => p
kind_ = mkProp "kind"

label_ :: ElementOrProp p JSString => p
label_ = fst (symbolName "label" :: ElementOrProp p JSString => (p, Proxy JSString))

lang_ :: IsProp p JSString => p
lang_ = mkProp "lang"

list_ :: IsProp p JSString => p
list_ = mkProp "list"

loop_ :: IsProp p JSString => p
loop_ = mkProp "loop"

low_ :: IsProp p JSString => p
low_ = mkProp "low"

manifest_ :: IsProp p JSString => p
manifest_ = mkProp "manifest"

marginHeight_ :: IsProp p JSString => p
marginHeight_ = mkProp "marginHeight"

marginWidth_ :: IsProp p JSString => p
marginWidth_ = mkProp "marginWidth"

max_ :: IsProp p JSString => p
max_ = mkProp "max"

maxLength_ :: IsProp p JSString => p
maxLength_ = mkProp "maxLength"

media_ :: IsProp p JSString => p
media_ = mkProp "media"

mediaGroup_ :: IsProp p JSString => p
mediaGroup_ = mkProp "mediaGroup"

method_ :: IsProp p JSString => p
method_ = mkProp "method"

min_ :: IsProp p JSString => p
min_ = mkProp "min"

minLength_ :: IsProp p JSString => p
minLength_ = mkProp "minLength"

multiple_ :: IsProp p JSString => p
multiple_ = mkProp "multiple"

muted_ :: IsProp p JSString => p
muted_ = mkProp "muted"

name_ :: IsProp p JSString => p
name_ = mkProp "name"

noValidate_ :: IsProp p Bool => p
noValidate_ = mkProp "noValidate"

nonce_ :: IsProp p JSString => p
nonce_ = mkProp "nonce"

open_ :: IsProp p JSString => p
open_ = mkProp "open"

optimum_ :: IsProp p JSString => p
optimum_ = mkProp "optimum"

pattern_ :: ElementOrProp p JSString => p
pattern_ = fst (symbolName "pattern" :: ElementOrProp p JSString => (p, Proxy JSString))

placeholder_ :: IsProp p JSString => p
placeholder_ = mkProp "placeholder"

poster_ :: IsProp p JSString => p
poster_ = mkProp "poster"

preload_ :: IsProp p JSString => p
preload_ = mkProp "preload"

profile_ :: IsProp p JSString => p
profile_ = mkProp "profile"

radioGroup_ :: IsProp p JSString => p
radioGroup_ = mkProp "radioGroup"

readOnly_ :: IsProp p JSString => p
readOnly_ = mkProp "readOnly"

rel_ :: IsProp p JSString => p
rel_ = mkProp "rel"

required_ :: IsProp p Bool => p
required_ = mkProp "required"

reversed_ :: IsProp p JSString => p
reversed_ = mkProp "reversed"

role_ :: IsProp p JSString => p
role_ = mkProp "role"

rowSpan_ :: IsProp p JSString => p
rowSpan_ = mkProp "rowSpan"

rows_ :: IsProp p Int => p
rows_ = mkProp "rows"

sandbox_ :: IsProp p JSString => p
sandbox_ = mkProp "sandbox"

scope_ :: IsProp p JSString => p
scope_ = mkProp "scope"

scoped_ :: IsProp p JSString => p
scoped_ = mkProp "scoped"

scrolling_ :: IsProp p JSString => p
scrolling_ = mkProp "scrolling"

seamless_ :: IsProp p JSString => p
seamless_ = mkProp "seamless"

selected_ :: IsProp p JSString => p
selected_ = mkProp "selected"

shape_ :: IsProp p JSString => p
shape_ = mkProp "shape"

size_ :: IsProp p JSString => p
size_ = mkProp "size"

sizes_ :: IsProp p JSString => p
sizes_ = mkProp "sizes"

span_ :: ElementOrProp p JSString => p
span_ = fst (symbolName "span" :: ElementOrProp p JSString => (p, Proxy JSString))

spellCheck_ :: IsProp p JSString => p
spellCheck_ = mkProp "spellCheck"

src_ :: IsProp p JSString => p
src_ = mkProp "src"

srcDoc_ :: IsProp p JSString => p
srcDoc_ = mkProp "srcDoc"

srcLang_ :: IsProp p JSString => p
srcLang_ = mkProp "srcLang"

srcSet_ :: IsProp p JSString => p
srcSet_ = mkProp "srcSet"

start_ :: IsProp p JSString => p
start_ = mkProp "start"

step_ :: IsProp p JSString => p
step_ = mkProp "step"

style_ :: ElementOrProp p Object => p
style_ = fst (symbolName "style" :: ElementOrProp p Object => (p, Proxy Object))

summary_ :: IsProp p JSString => p
summary_ = mkProp "summary"

tabIndex_ :: IsProp p Int => p
tabIndex_ = mkProp "tabIndex"

target_ :: IsProp p JSString => p
target_ = mkProp "target"

title_ :: ElementOrProp p JSString => p
title_ = fst (symbolName "title" :: ElementOrProp p JSString => (p, Proxy JSString))

type_ :: IsProp p JSString => p
type_ = mkProp "type"

useMap_ :: IsProp p JSString => p
useMap_ = mkProp "useMap"

value_ :: IsProp p (Maybe JSString) => p
value_ = mkProp "value"

width_ :: IsProp p Int => p
width_ = mkProp "width"

wmode_ :: IsProp p JSString => p
wmode_ = mkProp "wmode"
wrap_ :: IsProp p JSString => p
wrap_ = mkProp "wrap"
about_ :: IsProp p JSString => p
about_ = mkProp "about"
datatype_ :: IsProp p JSString => p
datatype_ = mkProp "datatype"
inlist_ :: IsProp p JSString => p
inlist_ = mkProp "inlist"
prefix_ :: IsProp p JSString => p
prefix_ = mkProp "prefix"
property_ :: IsProp p JSString => p
property_ = mkProp "property"
resource_ :: IsProp p JSString => p
resource_ = mkProp "resource"
typeof_ :: IsProp p JSString => p
typeof_ = mkProp "typeof"
vocab_ :: IsProp p JSString => p
vocab_ = mkProp "vocab"
autoCapitalize_ :: IsProp p JSString => p
autoCapitalize_ = mkProp "autoCapitalize"
autoCorrect_ :: IsProp p JSString => p
autoCorrect_ = mkProp "autoCorrect"
color_ :: IsProp p JSString => p
color_ = mkProp "color"
itemProp_ :: IsProp p JSString => p
itemProp_ = mkProp "itemProp"
itemScope_ :: IsProp p JSString => p
itemScope_ = mkProp "itemScope"
itemType_ :: IsProp p JSString => p
itemType_ = mkProp "itemType"
itemRef_ :: IsProp p JSString => p
itemRef_ = mkProp "itemRef"
itemID_ :: IsProp p JSString => p
itemID_ = mkProp "itemID"
security_ :: IsProp p JSString => p
security_ = mkProp "security"
unselectable_ :: IsProp p JSString => p
unselectable_ = mkProp "unselectable"
results_ :: IsProp p JSString => p
results_ = mkProp "results"
autoSave_ :: IsProp p JSString => p
autoSave_ = mkProp "autoSave"
accentHeight_ :: IsProp p JSString => p
accentHeight_ = mkProp "accentHeight"
accumulate_ :: IsProp p JSString => p
accumulate_ = mkProp "accumulate"
additive_ :: IsProp p JSString => p
additive_ = mkProp "additive"
alignmentBaseline_ :: IsProp p JSString => p
alignmentBaseline_ = mkProp "alignmentBaseline"
allowReorder_ :: IsProp p JSString => p
allowReorder_ = mkProp "allowReorder"
alphabetic_ :: IsProp p JSString => p
alphabetic_ = mkProp "alphabetic"
amplitude_ :: IsProp p JSString => p
amplitude_ = mkProp "amplitude"
arabicForm_ :: IsProp p JSString => p
arabicForm_ = mkProp "arabicForm"
ascent_ :: IsProp p JSString => p
ascent_ = mkProp "ascent"
attributeName_ :: IsProp p JSString => p
attributeName_ = mkProp "attributeName"
attributeType_ :: IsProp p JSString => p
attributeType_ = mkProp "attributeType"
autoReverse_ :: IsProp p JSString => p
autoReverse_ = mkProp "autoReverse"
azimuth_ :: IsProp p JSString => p
azimuth_ = mkProp "azimuth"
baseFrequency_ :: IsProp p JSString => p
baseFrequency_ = mkProp "baseFrequency"
baseProfile_ :: IsProp p JSString => p
baseProfile_ = mkProp "baseProfile"
baselineShift_ :: IsProp p JSString => p
baselineShift_ = mkProp "baselineShift"
bbox_ :: IsProp p JSString => p
bbox_ = mkProp "bbox"
begin_ :: IsProp p JSString => p
begin_ = mkProp "begin"
bias_ :: IsProp p JSString => p
bias_ = mkProp "bias"
by_ :: IsProp p JSString => p
by_ = mkProp "by"
calcMode_ :: IsProp p JSString => p
calcMode_ = mkProp "calcMode"
capHeight_ :: IsProp p JSString => p
capHeight_ = mkProp "capHeight"
clip_ :: IsProp p JSString => p
clip_ = mkProp "clip"
clipPathUnits_ :: IsProp p JSString => p
clipPathUnits_ = mkProp "clipPathUnits"
clipRule_ :: IsProp p JSString => p
clipRule_ = mkProp "clipRule"
colorInterpolation_ :: IsProp p JSString => p
colorInterpolation_ = mkProp "colorInterpolation"
colorInterpolationFilters_ :: IsProp p JSString => p
colorInterpolationFilters_ = mkProp "colorInterpolationFilters"
colorProfile_ :: IsProp p JSString => p
colorProfile_ = mkProp "colorProfile"
colorRendering_ :: IsProp p JSString => p
colorRendering_ = mkProp "colorRendering"
contentScriptType_ :: IsProp p JSString => p
contentScriptType_ = mkProp "contentScriptType"
contentStyleType_ :: IsProp p JSString => p
contentStyleType_ = mkProp "contentStyleType"
cursor_ :: IsProp p JSString => p
cursor_ = mkProp "cursor"
cx_ :: IsProp p JSString => p
cx_ = mkProp "cx"
cy_ :: IsProp p JSString => p
cy_ = mkProp "cy"
d_ :: IsProp p JSString => p
d_ = mkProp "d"
decelerate_ :: IsProp p JSString => p
decelerate_ = mkProp "decelerate"
descent_ :: IsProp p JSString => p
descent_ = mkProp "descent"
diffuseConstant_ :: IsProp p JSString => p
diffuseConstant_ = mkProp "diffuseConstant"
direction_ :: IsProp p JSString => p
direction_ = mkProp "direction"
display_ :: IsProp p JSString => p
display_ = mkProp "display"
divisor_ :: IsProp p JSString => p
divisor_ = mkProp "divisor"
dominantBaseline_ :: IsProp p JSString => p
dominantBaseline_ = mkProp "dominantBaseline"
dur_ :: IsProp p JSString => p
dur_ = mkProp "dur"
dx_ :: IsProp p JSString => p
dx_ = mkProp "dx"
dy_ :: IsProp p JSString => p
dy_ = mkProp "dy"
edgeMode_ :: IsProp p JSString => p
edgeMode_ = mkProp "edgeMode"
elevation_ :: IsProp p JSString => p
elevation_ = mkProp "elevation"
enableBackground_ :: IsProp p JSString => p
enableBackground_ = mkProp "enableBackground"
end_ :: IsProp p JSString => p
end_ = mkProp "end"
exponent_ :: IsProp p JSString => p
exponent_ = mkProp "exponent"
externalResourcesRequired_ :: IsProp p JSString => p
externalResourcesRequired_ = mkProp "externalResourcesRequired"
fill_ :: IsProp p JSString => p
fill_ = mkProp "fill"
fillOpacity_ :: IsProp p JSString => p
fillOpacity_ = mkProp "fillOpacity"
fillRule_ :: IsProp p JSString => p
fillRule_ = mkProp "fillRule"
filter_ :: IsProp p JSString => p
filter_ = mkProp "filter"
filterRes_ :: IsProp p JSString => p
filterRes_ = mkProp "filterRes"
filterUnits_ :: IsProp p JSString => p
filterUnits_ = mkProp "filterUnits"
floodColor_ :: IsProp p JSString => p
floodColor_ = mkProp "floodColor"
floodOpacity_ :: IsProp p JSString => p
floodOpacity_ = mkProp "floodOpacity"
focusable_ :: IsProp p JSString => p
focusable_ = mkProp "focusable"
fontFamily_ :: IsProp p JSString => p
fontFamily_ = mkProp "fontFamily"
fontSize_ :: IsProp p JSString => p
fontSize_ = mkProp "fontSize"
fontSizeAdjust_ :: IsProp p JSString => p
fontSizeAdjust_ = mkProp "fontSizeAdjust"
fontStretch_ :: IsProp p JSString => p
fontStretch_ = mkProp "fontStretch"
fontStyle_ :: IsProp p JSString => p
fontStyle_ = mkProp "fontStyle"
fontVariant_ :: IsProp p JSString => p
fontVariant_ = mkProp "fontVariant"
fontWeight_ :: IsProp p JSString => p
fontWeight_ = mkProp "fontWeight"
format_ :: IsProp p JSString => p
format_ = mkProp "format"
from_ :: IsProp p JSString => p
from_ = mkProp "from"
fx_ :: IsProp p JSString => p
fx_ = mkProp "fx"
fy_ :: IsProp p JSString => p
fy_ = mkProp "fy"
g1_ :: IsProp p JSString => p
g1_ = mkProp "g1"
g2_ :: IsProp p JSString => p
g2_ = mkProp "g2"
glyphName_ :: IsProp p JSString => p
glyphName_ = mkProp "glyphName"
glyphOrientationHorizontal_ :: IsProp p JSString => p
glyphOrientationHorizontal_ = mkProp "glyphOrientationHorizontal"
glyphOrientationVertical_ :: IsProp p JSString => p
glyphOrientationVertical_ = mkProp "glyphOrientationVertical"
glyphRef_ :: IsProp p JSString => p
glyphRef_ = mkProp "glyphRef"
gradientTransform_ :: IsProp p JSString => p
gradientTransform_ = mkProp "gradientTransform"
gradientUnits_ :: IsProp p JSString => p
gradientUnits_ = mkProp "gradientUnits"
hanging_ :: IsProp p JSString => p
hanging_ = mkProp "hanging"
horizAdvX_ :: IsProp p JSString => p
horizAdvX_ = mkProp "horizAdvX"
horizOriginX_ :: IsProp p JSString => p
horizOriginX_ = mkProp "horizOriginX"
ideographic_ :: IsProp p JSString => p
ideographic_ = mkProp "ideographic"
imageRendering_ :: IsProp p JSString => p
imageRendering_ = mkProp "imageRendering"
in_ :: IsProp p JSString => p
in_ = mkProp "in"
in2_ :: IsProp p JSString => p
in2_ = mkProp "in2"
intercept_ :: IsProp p JSString => p
intercept_ = mkProp "intercept"
k_ :: IsProp p JSString => p
k_ = mkProp "k"
k1_ :: IsProp p JSString => p
k1_ = mkProp "k1"
k2_ :: IsProp p JSString => p
k2_ = mkProp "k2"
k3_ :: IsProp p JSString => p
k3_ = mkProp "k3"
k4_ :: IsProp p JSString => p
k4_ = mkProp "k4"
kernelMatrix_ :: IsProp p JSString => p
kernelMatrix_ = mkProp "kernelMatrix"
kernelUnitLength_ :: IsProp p JSString => p
kernelUnitLength_ = mkProp "kernelUnitLength"
kerning_ :: IsProp p JSString => p
kerning_ = mkProp "kerning"
keyPoints_ :: IsProp p JSString => p
keyPoints_ = mkProp "keyPoints"
keySplines_ :: IsProp p JSString => p
keySplines_ = mkProp "keySplines"
keyTimes_ :: IsProp p JSString => p
keyTimes_ = mkProp "keyTimes"
lengthAdjust_ :: IsProp p JSString => p
lengthAdjust_ = mkProp "lengthAdjust"
letterSpacing_ :: IsProp p JSString => p
letterSpacing_ = mkProp "letterSpacing"
lightingColor_ :: IsProp p JSString => p
lightingColor_ = mkProp "lightingColor"
limitingConeAngle_ :: IsProp p JSString => p
limitingConeAngle_ = mkProp "limitingConeAngle"
local_ :: IsProp p JSString => p
local_ = mkProp "local"
markerEnd_ :: IsProp p JSString => p
markerEnd_ = mkProp "markerEnd"
markerHeight_ :: IsProp p JSString => p
markerHeight_ = mkProp "markerHeight"
markerMid_ :: IsProp p JSString => p
markerMid_ = mkProp "markerMid"
markerStart_ :: IsProp p JSString => p
markerStart_ = mkProp "markerStart"
markerUnits_ :: IsProp p JSString => p
markerUnits_ = mkProp "markerUnits"
markerWidth_ :: IsProp p JSString => p
markerWidth_ = mkProp "markerWidth"
mask_ :: ElementOrProp p JSString => p
mask_ = fst (symbolName "mask" :: ElementOrProp p JSString => (p, Proxy JSString))
maskContentUnits_ :: IsProp p JSString => p
maskContentUnits_ = mkProp "maskContentUnits"
maskUnits_ :: IsProp p JSString => p
maskUnits_ = mkProp "maskUnits"
mathematical_ :: IsProp p JSString => p
mathematical_ = mkProp "mathematical"
mode_ :: IsProp p JSString => p
mode_ = mkProp "mode"
numOctaves_ :: IsProp p JSString => p
numOctaves_ = mkProp "numOctaves"
offset_ :: IsProp p JSString => p
offset_ = mkProp "offset"
opacity_ :: IsProp p JSString => p
opacity_ = mkProp "opacity"
operator_ :: IsProp p JSString => p
operator_ = mkProp "operator"
order_ :: IsProp p JSString => p
order_ = mkProp "order"
orient_ :: IsProp p JSString => p
orient_ = mkProp "orient"
orientation_ :: IsProp p JSString => p
orientation_ = mkProp "orientation"
origin_ :: IsProp p JSString => p
origin_ = mkProp "origin"
overflow_ :: IsProp p JSString => p
overflow_ = mkProp "overflow"
overlinePosition_ :: IsProp p JSString => p
overlinePosition_ = mkProp "overlinePosition"
overlineThickness_ :: IsProp p JSString => p
overlineThickness_ = mkProp "overlineThickness"
paintOrder_ :: IsProp p JSString => p
paintOrder_ = mkProp "paintOrder"
panose1_ :: IsProp p JSString => p
panose1_ = mkProp "panose1"
pathLength_ :: IsProp p JSString => p
pathLength_ = mkProp "pathLength"
patternContentUnits_ :: IsProp p JSString => p
patternContentUnits_ = mkProp "patternContentUnits"
patternTransform_ :: IsProp p JSString => p
patternTransform_ = mkProp "patternTransform"
patternUnits_ :: IsProp p JSString => p
patternUnits_ = mkProp "patternUnits"
pointerEvents_ :: IsProp p JSString => p
pointerEvents_ = mkProp "pointerEvents"
points_ :: IsProp p JSString => p
points_ = mkProp "points"
pointsAtX_ :: IsProp p JSString => p
pointsAtX_ = mkProp "pointsAtX"
pointsAtY_ :: IsProp p JSString => p
pointsAtY_ = mkProp "pointsAtY"
pointsAtZ_ :: IsProp p JSString => p
pointsAtZ_ = mkProp "pointsAtZ"
preserveAlpha_ :: IsProp p JSString => p
preserveAlpha_ = mkProp "preserveAlpha"
preserveAspectRatio_ :: IsProp p JSString => p
preserveAspectRatio_ = mkProp "preserveAspectRatio"
primitiveUnits_ :: IsProp p JSString => p
primitiveUnits_ = mkProp "primitiveUnits"
r_ :: IsProp p JSString => p
r_ = mkProp "r"
radius_ :: IsProp p JSString => p
radius_ = mkProp "radius"
refX_ :: IsProp p JSString => p
refX_ = mkProp "refX"
refY_ :: IsProp p JSString => p
refY_ = mkProp "refY"
renderingIntent_ :: IsProp p JSString => p
renderingIntent_ = mkProp "renderingIntent"
repeatCount_ :: IsProp p JSString => p
repeatCount_ = mkProp "repeatCount"
repeatDur_ :: IsProp p JSString => p
repeatDur_ = mkProp "repeatDur"
requiredExtensions_ :: IsProp p JSString => p
requiredExtensions_ = mkProp "requiredExtensions"
requiredFeatures_ :: IsProp p JSString => p
requiredFeatures_ = mkProp "requiredFeatures"
restart_ :: IsProp p JSString => p
restart_ = mkProp "restart"
result_ :: IsProp p JSString => p
result_ = mkProp "result"
rotate_ :: IsProp p JSString => p
rotate_ = mkProp "rotate"
rx_ :: IsProp p JSString => p
rx_ = mkProp "rx"
ry_ :: IsProp p JSString => p
ry_ = mkProp "ry"
scale_ :: IsProp p JSString => p
scale_ = mkProp "scale"
seed_ :: IsProp p JSString => p
seed_ = mkProp "seed"
shapeRendering_ :: IsProp p JSString => p
shapeRendering_ = mkProp "shapeRendering"
slope_ :: IsProp p JSString => p
slope_ = mkProp "slope"
spacing_ :: IsProp p JSString => p
spacing_ = mkProp "spacing"
specularConstant_ :: IsProp p JSString => p
specularConstant_ = mkProp "specularConstant"
specularExponent_ :: IsProp p JSString => p
specularExponent_ = mkProp "specularExponent"
speed_ :: IsProp p JSString => p
speed_ = mkProp "speed"
spreadMethod_ :: IsProp p JSString => p
spreadMethod_ = mkProp "spreadMethod"
startOffset_ :: IsProp p JSString => p
startOffset_ = mkProp "startOffset"
stdDeviation_ :: IsProp p JSString => p
stdDeviation_ = mkProp "stdDeviation"
stemh_ :: IsProp p JSString => p
stemh_ = mkProp "stemh"
stemv_ :: IsProp p JSString => p
stemv_ = mkProp "stemv"
stitchTiles_ :: IsProp p JSString => p
stitchTiles_ = mkProp "stitchTiles"
stopColor_ :: IsProp p JSString => p
stopColor_ = mkProp "stopColor"
stopOpacity_ :: IsProp p JSString => p
stopOpacity_ = mkProp "stopOpacity"
strikethroughPosition_ :: IsProp p JSString => p
strikethroughPosition_ = mkProp "strikethroughPosition"
strikethroughThickness_ :: IsProp p JSString => p
strikethroughThickness_ = mkProp "strikethroughThickness"
string_ :: IsProp p JSString => p
string_ = mkProp "string"
stroke_ :: IsProp p JSString => p
stroke_ = mkProp "stroke"
strokeDasharray_ :: IsProp p JSString => p
strokeDasharray_ = mkProp "strokeDasharray"
strokeDashoffset_ :: IsProp p JSString => p
strokeDashoffset_ = mkProp "strokeDashoffset"
strokeLinecap_ :: IsProp p JSString => p
strokeLinecap_ = mkProp "strokeLinecap"
strokeLinejoin_ :: IsProp p JSString => p
strokeLinejoin_ = mkProp "strokeLinejoin"
strokeMiterlimit_ :: IsProp p JSString => p
strokeMiterlimit_ = mkProp "strokeMiterlimit"
strokeOpacity_ :: IsProp p JSString => p
strokeOpacity_ = mkProp "strokeOpacity"
strokeWidth_ :: IsProp p Double => p
strokeWidth_ = mkProp "strokeWidth"
surfaceScale_ :: IsProp p JSString => p
surfaceScale_ = mkProp "surfaceScale"
systemLanguage_ :: IsProp p JSString => p
systemLanguage_ = mkProp "systemLanguage"
tableValues_ :: IsProp p JSString => p
tableValues_ = mkProp "tableValues"
targetX_ :: IsProp p JSString => p
targetX_ = mkProp "targetX"
targetY_ :: IsProp p JSString => p
targetY_ = mkProp "targetY"
textAnchor_ :: IsProp p JSString => p
textAnchor_ = mkProp "textAnchor"
textDecoration_ :: IsProp p JSString => p
textDecoration_ = mkProp "textDecoration"
textLength_ :: IsProp p JSString => p
textLength_ = mkProp "textLength"
textRendering_ :: IsProp p JSString => p
textRendering_ = mkProp "textRendering"
to_ :: IsProp p JSString => p
to_ = mkProp "to"
transform_ :: IsProp p JSString => p
transform_ = mkProp "transform"
u1_ :: IsProp p JSString => p
u1_ = mkProp "u1"
u2_ :: IsProp p JSString => p
u2_ = mkProp "u2"
underlinePosition_ :: IsProp p JSString => p
underlinePosition_ = mkProp "underlinePosition"
underlineThickness_ :: IsProp p JSString => p
underlineThickness_ = mkProp "underlineThickness"
unicode_ :: IsProp p JSString => p
unicode_ = mkProp "unicode"
unicodeBidi_ :: IsProp p JSString => p
unicodeBidi_ = mkProp "unicodeBidi"
unicodeRange_ :: IsProp p JSString => p
unicodeRange_ = mkProp "unicodeRange"
unitsPerEm_ :: IsProp p JSString => p
unitsPerEm_ = mkProp "unitsPerEm"
vAlphabetic_ :: IsProp p JSString => p
vAlphabetic_ = mkProp "vAlphabetic"
vHanging_ :: IsProp p JSString => p
vHanging_ = mkProp "vHanging"
vIdeographic_ :: IsProp p JSString => p
vIdeographic_ = mkProp "vIdeographic"
vMathematical_ :: IsProp p JSString => p
vMathematical_ = mkProp "vMathematical"
values_ :: IsProp p JSString => p
values_ = mkProp "values"
vectorEffect_ :: IsProp p JSString => p
vectorEffect_ = mkProp "vectorEffect"
version_ :: IsProp p JSString => p
version_ = mkProp "version"
vertAdvY_ :: IsProp p JSString => p
vertAdvY_ = mkProp "vertAdvY"
vertOriginX_ :: IsProp p JSString => p
vertOriginX_ = mkProp "vertOriginX"
vertOriginY_ :: IsProp p JSString => p
vertOriginY_ = mkProp "vertOriginY"
viewBox_ :: IsProp p JSString => p
viewBox_ = mkProp "viewBox"
viewTarget_ :: IsProp p JSString => p
viewTarget_ = mkProp "viewTarget"
visibility_ :: IsProp p JSString => p
visibility_ = mkProp "visibility"
widths_ :: IsProp p JSString => p
widths_ = mkProp "widths"
wordSpacing_ :: IsProp p JSString => p
wordSpacing_ = mkProp "wordSpacing"
writingMode_ :: IsProp p JSString => p
writingMode_ = mkProp "writingMode"
x_ :: IsProp p JSString => p
x_ = mkProp "x"
x1_ :: IsProp p JSString => p
x1_ = mkProp "x1"
x2_ :: IsProp p JSString => p
x2_ = mkProp "x2"
xChannelSelector_ :: IsProp p JSString => p
xChannelSelector_ = mkProp "xChannelSelector"
xHeight_ :: IsProp p JSString => p
xHeight_ = mkProp "xHeight"
xlinkActuate_ :: IsProp p JSString => p
xlinkActuate_ = mkProp "xlinkActuate"
xlinkArcrole_ :: IsProp p JSString => p
xlinkArcrole_ = mkProp "xlinkArcrole"
xlinkHref_ :: IsProp p JSString => p
xlinkHref_ = mkProp "xlinkHref"
xlinkRole_ :: IsProp p JSString => p
xlinkRole_ = mkProp "xlinkRole"
xlinkShow_ :: IsProp p JSString => p
xlinkShow_ = mkProp "xlinkShow"
xlinkTitle_ :: IsProp p JSString => p
xlinkTitle_ = mkProp "xlinkTitle"
xlinkType_ :: IsProp p JSString => p
xlinkType_ = mkProp "xlinkType"
xmlBase_ :: IsProp p JSString => p
xmlBase_ = mkProp "xmlBase"
xmlLang_ :: IsProp p JSString => p
xmlLang_ = mkProp "xmlLang"
xmlSpace_ :: IsProp p JSString => p
xmlSpace_ = mkProp "xmlSpace"
y_ :: IsProp p JSString => p
y_ = mkProp "y"
y1_ :: IsProp p JSString => p
y1_ = mkProp "y1"
y2_ :: IsProp p JSString => p
y2_ = mkProp "y2"
yChannelSelector_ :: IsProp p JSString => p
yChannelSelector_ = mkProp "yChannelSelector"
z_ :: IsProp p JSString => p
z_ = mkProp "z"
zoomAndPan_ :: IsProp p JSString => p
zoomAndPan_ = mkProp "zoomAndPan"

dataAttr :: JSString -> IsProp p JSString => p
dataAttr = mkProp . JSString.append "data-"

ariaAttr :: JSString -> IsProp p JSString => p
ariaAttr = mkProp . JSString.append "aria-"
