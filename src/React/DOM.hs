{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module React.DOM where
import qualified Data.Foldable as F
import Data.Proxy
import qualified Data.JSString as JSString
import JavaScript.Object (Object)
import GHCJS.Foreign
import GHCJS.Marshal.Pure
import GHCJS.Types
import React

foreign import javascript "React['DOM'][$1]($2, $3)" js_elem :: JSString -> Props ps -> JSVal -> ReactElement
mkElem :: (Applicative t, Foldable t, F.Foldable elems) => JSString -> t Prop -> elems ReactElement -> ReactElement
mkElem str ps c = js_elem str (buildProps ps) (if Prelude.null c then jsNull else pToJSVal $ array $ F.toList c)

mkEmptyElem :: (Applicative t, Foldable t) => JSString -> t Prop -> ReactElement
mkEmptyElem str ps = js_elem str (buildProps ps) jsUndefined

class ElementOrProp f t where
  symbolName :: JSString -> (f, Proxy t)

instance (Applicative t, Foldable t) => ElementOrProp (t Prop -> [ReactElement] -> ReactElement) a where
  symbolName n = (mkElem n, Proxy)

instance (Applicative t, Foldable t) => ElementOrProp (t Prop -> ReactElement) a where
  symbolName n = (mkEmptyElem n, Proxy)

instance ElementOrProp (PropName p) p where
  symbolName n = (PropName n, Proxy)

a_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
a_ = mkElem "a"

abbr_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
abbr_ = mkElem "abbr"

address_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
address_ = mkElem "address"

area_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
area_ = mkEmptyElem "area"

article_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
article_ = mkElem "article"

aside_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
aside_ = mkElem "aside"

audio_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
audio_ = mkElem "audio"

b_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
b_ = mkElem "b"

base_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
base_ = mkEmptyElem "base"

bdi_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
bdi_ = mkElem "bdi"

bdo_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
bdo_ = mkElem "bdo"

big_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
big_ = mkElem "big"

blockquote_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
blockquote_ = mkElem "blockquote"

body_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
body_ = mkElem "body"

br_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
br_ = mkEmptyElem "br"

button_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
button_ = mkElem "button"

canvas_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
canvas_ = mkElem "canvas"

caption_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
caption_ = mkElem "caption"

circle_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
circle_ = mkElem "circle"

clipPath_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
clipPath_ = mkElem "clipPath"

code_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
code_ = mkElem "code"

col_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
col_ = mkEmptyElem "col"

colgroup_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
colgroup_ = mkElem "colgroup"

datalist_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
datalist_ = mkElem "datalist"

dd_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
dd_ = mkElem "dd"

defs_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
defs_ = mkElem "defs"

del_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
del_ = mkElem "del"

details_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
details_ = mkElem "details"

dfn_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
dfn_ = mkElem "dfn"

dialog_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
dialog_ = mkElem "dialog"

div_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
div_ = mkElem "div"

dl_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
dl_ = mkElem "dl"

dt_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
dt_ = mkElem "dt"

ellipse_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
ellipse_ = mkElem "ellipse"

em_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
em_ = mkElem "em"

embed_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
embed_ = mkEmptyElem "embed"

fieldset_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
fieldset_ = mkElem "fieldset"

figcaption_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
figcaption_ = mkElem "figcaption"

figure_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
figure_ = mkElem "figure"

footer_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
footer_ = mkElem "footer"

g_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
g_ = mkElem "g"

h1_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
h1_ = mkElem "h1"

h2_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
h2_ = mkElem "h2"

h3_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
h3_ = mkElem "h3"

h4_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
h4_ = mkElem "h4"

h5_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
h5_ = mkElem "h5"

h6_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
h6_ = mkElem "h6"

head_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
head_ = mkElem "head"

header_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
header_ = mkElem "header"

hgroup_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
hgroup_ = mkElem "hgroup"

hr_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
hr_ = mkEmptyElem "hr"

html_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
html_ = mkElem "html"

i_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
i_ = mkElem "i"

iframe_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
iframe_ = mkElem "iframe"

image_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
image_ = mkElem "image"

img_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
img_ = mkEmptyElem "img"

input_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
input_ = mkEmptyElem "input"

ins_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
ins_ = mkElem "ins"

kbd_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
kbd_ = mkElem "kbd"

keygen_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
keygen_ = mkEmptyElem "keygen"

legend_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
legend_ = mkElem "legend"

li_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
li_ = mkElem "li"

line_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
line_ = mkElem "line"

linearGradient_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
linearGradient_ = mkElem "linearGradient"

link_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
link_ = mkEmptyElem "link"

main_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
main_ = mkElem "main"

map_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
map_ = mkElem "map"

mark_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
mark_ = mkElem "mark"

menu_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
menu_ = mkElem "menu"

menuitem_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
menuitem_ = mkEmptyElem "menuitem"

meta_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
meta_ = mkEmptyElem "meta"

meter_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
meter_ = mkElem "meter"

nav_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
nav_ = mkElem "nav"

noscript_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
noscript_ = mkElem "noscript"

object_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
object_ = mkElem "object"

ol_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
ol_ = mkElem "ol"

optgroup_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
optgroup_ = mkElem "optgroup"

option_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
option_ = mkElem "option"

output_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
output_ = mkElem "output"

p_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
p_ = mkElem "p"

param_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
param_ = mkEmptyElem "param"

path_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
path_ = mkElem "path"

picture_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
picture_ = mkElem "picture"

polygon_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
polygon_ = mkElem "polygon"

polyline_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
polyline_ = mkElem "polyline"

pre_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
pre_ = mkElem "pre"

progress_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
progress_ = mkElem "progress"

q_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
q_ = mkElem "q"

radialGradient_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
radialGradient_ = mkElem "radialGradient"

rect_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
rect_ = mkElem "rect"

rp_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
rp_ = mkElem "rp"

rt_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
rt_ = mkElem "rt"

ruby_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
ruby_ = mkElem "ruby"

s_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
s_ = mkElem "s"

samp_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
samp_ = mkElem "samp"

script_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
script_ = mkElem "script"

section_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
section_ = mkElem "section"

select_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
select_ = mkElem "select"

small_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
small_ = mkElem "small"

source_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
source_ = mkEmptyElem "source"

stop_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
stop_ = mkElem "stop"

strong_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
strong_ = mkElem "strong"

sub_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
sub_ = mkElem "sub"

svg_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
svg_ = mkElem "svg"

table_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
table_ = mkElem "table"

tbody_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
tbody_ = mkElem "tbody"

td_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
td_ = mkElem "td"

text_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
text_ = mkElem "text"

textarea_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
textarea_ = mkElem "textarea"

tfoot_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
tfoot_ = mkElem "tfoot"

th_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
th_ = mkElem "th"

thead_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
thead_ = mkElem "thead"

time_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
time_ = mkElem "time"

tr_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
tr_ = mkElem "tr"

track_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
track_ = mkEmptyElem "track"

tspan_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
tspan_ = mkElem "tspan"

u_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
u_ = mkElem "u"

ul_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
ul_ = mkElem "ul"

var_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
var_ = mkElem "var"

video_ :: (Applicative t, Foldable t, Foldable elems) => t Prop -> elems ReactElement -> ReactElement
video_ = mkElem "video"

wbr_ :: (Applicative t, Foldable t) => t Prop -> ReactElement
wbr_ = mkEmptyElem "wbr"


accept_ :: PropName JSString
accept_ = PropName "accept"

acceptCharset_ :: PropName JSString
acceptCharset_ = PropName "acceptCharset"

accessKey_ :: PropName JSString
accessKey_ = PropName "accessKey"

action_ :: PropName JSString
action_ = PropName "action"

allowFullScreen_ :: PropName JSString
allowFullScreen_ = PropName "allowFullScreen"

allowTransparency_ :: PropName JSString
allowTransparency_ = PropName "allowTransparency"

alt_ :: PropName JSString
alt_ = PropName "alt"

async_ :: PropName JSString
async_ = PropName "async"

autoComplete_ :: PropName JSString
autoComplete_ = PropName "autoComplete"

autoFocus_ :: PropName JSString
autoFocus_ = PropName "autoFocus"

autoPlay_ :: PropName JSString
autoPlay_ = PropName "autoPlay"

capture_ :: PropName JSString
capture_ = PropName "capture"

cellPadding_ :: PropName JSString
cellPadding_ = PropName "cellPadding"

cellSpacing_ :: PropName JSString
cellSpacing_ = PropName "cellSpacing"

challenge_ :: PropName JSString
challenge_ = PropName "challenge"

charSet_ :: PropName JSString
charSet_ = PropName "charSet"

checked_ :: PropName JSString
checked_ = PropName "checked"

cite_ :: ElementOrProp p JSString => p
cite_ = fst (symbolName "cite" :: ElementOrProp p JSString => (p, Proxy JSString))

classID_ :: PropName JSString
classID_ = PropName "classID"

className_ :: PropName JSString
className_ = PropName "className"

colSpan_ :: PropName JSString
colSpan_ = PropName "colSpan"

cols_ :: PropName JSString
cols_ = PropName "cols"

content_ :: PropName JSString
content_ = PropName "content"

contentEditable_ :: PropName JSString
contentEditable_ = PropName "contentEditable"

contextMenu_ :: PropName JSString
contextMenu_ = PropName "contextMenu"

controls_ :: PropName JSString
controls_ = PropName "controls"

coords_ :: PropName JSString
coords_ = PropName "coords"

crossOrigin_ :: PropName JSString
crossOrigin_ = PropName "crossOrigin"

data_ :: ElementOrProp p JSString => p
data_ = fst (symbolName "data" :: ElementOrProp p JSString => (p, Proxy JSString))

dateTime_ :: PropName JSString
dateTime_ = PropName "dateTime"

default_ :: PropName JSString
default_ = PropName "default"

defer_ :: PropName JSString
defer_ = PropName "defer"

dir_ :: PropName JSString
dir_ = PropName "dir"

disabled_ :: PropName JSString
disabled_ = PropName "disabled"

download_ :: PropName JSString
download_ = PropName "download"

draggable_ :: PropName JSString
draggable_ = PropName "draggable"

encType_ :: PropName JSString
encType_ = PropName "encType"

form_ :: ElementOrProp p JSString => p
form_ = fst (symbolName "form" :: ElementOrProp p JSString => (p, Proxy JSString))

formAction_ :: PropName JSString
formAction_ = PropName "formAction"

formEncType_ :: PropName JSString
formEncType_ = PropName "formEncType"

formMethod_ :: PropName JSString
formMethod_ = PropName "formMethod"

formNoValidate_ :: PropName JSString
formNoValidate_ = PropName "formNoValidate"

formTarget_ :: PropName JSString
formTarget_ = PropName "formTarget"

frameBorder_ :: PropName JSString
frameBorder_ = PropName "frameBorder"

headers_ :: PropName JSString
headers_ = PropName "headers"

height_ :: PropName Int
height_ = PropName "height"

hidden_ :: PropName Bool
hidden_ = PropName "hidden"

high_ :: PropName JSString
high_ = PropName "high"

href_ :: PropName JSString
href_ = PropName "href"

hrefLang_ :: PropName JSString
hrefLang_ = PropName "hrefLang"

htmlFor_ :: PropName JSString
htmlFor_ = PropName "htmlFor"

httpEquiv_ :: PropName JSString
httpEquiv_ = PropName "httpEquiv"

icon_ :: PropName JSString
icon_ = PropName "icon"

id_ :: PropName JSString
id_ = PropName "id"

inputMode_ :: PropName JSString
inputMode_ = PropName "inputMode"

integrity_ :: PropName JSString
integrity_ = PropName "integrity"

is_ :: PropName JSString
is_ = PropName "is"

keyParams_ :: PropName JSString
keyParams_ = PropName "keyParams"

keyType_ :: PropName JSString
keyType_ = PropName "keyType"

kind_ :: PropName JSString
kind_ = PropName "kind"

label_ :: ElementOrProp p JSString => p
label_ = fst (symbolName "label" :: ElementOrProp p JSString => (p, Proxy JSString))

lang_ :: PropName JSString
lang_ = PropName "lang"

list_ :: PropName JSString
list_ = PropName "list"

loop_ :: PropName JSString
loop_ = PropName "loop"

low_ :: PropName JSString
low_ = PropName "low"

manifest_ :: PropName JSString
manifest_ = PropName "manifest"

marginHeight_ :: PropName JSString
marginHeight_ = PropName "marginHeight"

marginWidth_ :: PropName JSString
marginWidth_ = PropName "marginWidth"

max_ :: PropName JSString
max_ = PropName "max"

maxLength_ :: PropName JSString
maxLength_ = PropName "maxLength"

media_ :: PropName JSString
media_ = PropName "media"

mediaGroup_ :: PropName JSString
mediaGroup_ = PropName "mediaGroup"

method_ :: PropName JSString
method_ = PropName "method"

min_ :: PropName JSString
min_ = PropName "min"

minLength_ :: PropName JSString
minLength_ = PropName "minLength"

multiple_ :: PropName JSString
multiple_ = PropName "multiple"

muted_ :: PropName JSString
muted_ = PropName "muted"

name_ :: PropName JSString
name_ = PropName "name"

noValidate_ :: PropName Bool
noValidate_ = PropName "noValidate"

nonce_ :: PropName JSString
nonce_ = PropName "nonce"

open_ :: PropName JSString
open_ = PropName "open"

optimum_ :: PropName JSString
optimum_ = PropName "optimum"

pattern_ :: ElementOrProp p JSString => p
pattern_ = fst (symbolName "pattern" :: ElementOrProp p JSString => (p, Proxy JSString))

placeholder_ :: PropName JSString
placeholder_ = PropName "placeholder"

poster_ :: PropName JSString
poster_ = PropName "poster"

preload_ :: PropName JSString
preload_ = PropName "preload"

profile_ :: PropName JSString
profile_ = PropName "profile"

radioGroup_ :: PropName JSString
radioGroup_ = PropName "radioGroup"

readOnly_ :: PropName JSString
readOnly_ = PropName "readOnly"

rel_ :: PropName JSString
rel_ = PropName "rel"

required_ :: PropName Bool
required_ = PropName "required"

reversed_ :: PropName JSString
reversed_ = PropName "reversed"

role_ :: PropName JSString
role_ = PropName "role"

rowSpan_ :: PropName JSString
rowSpan_ = PropName "rowSpan"

rows_ :: PropName Int
rows_ = PropName "rows"

sandbox_ :: PropName JSString
sandbox_ = PropName "sandbox"

scope_ :: PropName JSString
scope_ = PropName "scope"

scoped_ :: PropName JSString
scoped_ = PropName "scoped"

scrolling_ :: PropName JSString
scrolling_ = PropName "scrolling"

seamless_ :: PropName JSString
seamless_ = PropName "seamless"

selected_ :: PropName JSString
selected_ = PropName "selected"

shape_ :: PropName JSString
shape_ = PropName "shape"

size_ :: PropName JSString
size_ = PropName "size"

sizes_ :: PropName JSString
sizes_ = PropName "sizes"

span_ :: ElementOrProp p JSString => p
span_ = fst (symbolName "span" :: ElementOrProp p JSString => (p, Proxy JSString))

spellCheck_ :: PropName JSString
spellCheck_ = PropName "spellCheck"

src_ :: PropName JSString
src_ = PropName "src"

srcDoc_ :: PropName JSString
srcDoc_ = PropName "srcDoc"

srcLang_ :: PropName JSString
srcLang_ = PropName "srcLang"

srcSet_ :: PropName JSString
srcSet_ = PropName "srcSet"

start_ :: PropName JSString
start_ = PropName "start"

step_ :: PropName JSString
step_ = PropName "step"

style_ :: ElementOrProp p Object => p
style_ = fst (symbolName "style" :: ElementOrProp p Object => (p, Proxy Object))

summary_ :: PropName JSString
summary_ = PropName "summary"

tabIndex_ :: PropName Int
tabIndex_ = PropName "tabIndex"

target_ :: PropName JSString
target_ = PropName "target"

title_ :: ElementOrProp p JSString => p
title_ = fst (symbolName "title" :: ElementOrProp p JSString => (p, Proxy JSString))

type_ :: PropName JSString
type_ = PropName "type"

useMap_ :: PropName JSString
useMap_ = PropName "useMap"

value_ :: PropName JSString
value_ = PropName "value"

width_ :: PropName Int
width_ = PropName "width"

wmode_ :: PropName JSString
wmode_ = PropName "wmode"
wrap_ :: PropName JSString
wrap_ = PropName "wrap"
about_ :: PropName JSString
about_ = PropName "about"
datatype_ :: PropName JSString
datatype_ = PropName "datatype"
inlist_ :: PropName JSString
inlist_ = PropName "inlist"
prefix_ :: PropName JSString
prefix_ = PropName "prefix"
property_ :: PropName JSString
property_ = PropName "property"
resource_ :: PropName JSString
resource_ = PropName "resource"
typeof_ :: PropName JSString
typeof_ = PropName "typeof"
vocab_ :: PropName JSString
vocab_ = PropName "vocab"
autoCapitalize_ :: PropName JSString
autoCapitalize_ = PropName "autoCapitalize"
autoCorrect_ :: PropName JSString
autoCorrect_ = PropName "autoCorrect"
color_ :: PropName JSString
color_ = PropName "color"
itemProp_ :: PropName JSString
itemProp_ = PropName "itemProp"
itemScope_ :: PropName JSString
itemScope_ = PropName "itemScope"
itemType_ :: PropName JSString
itemType_ = PropName "itemType"
itemRef_ :: PropName JSString
itemRef_ = PropName "itemRef"
itemID_ :: PropName JSString
itemID_ = PropName "itemID"
security_ :: PropName JSString
security_ = PropName "security"
unselectable_ :: PropName JSString
unselectable_ = PropName "unselectable"
results_ :: PropName JSString
results_ = PropName "results"
autoSave_ :: PropName JSString
autoSave_ = PropName "autoSave"
accentHeight_ :: PropName JSString
accentHeight_ = PropName "accentHeight"
accumulate_ :: PropName JSString
accumulate_ = PropName "accumulate"
additive_ :: PropName JSString
additive_ = PropName "additive"
alignmentBaseline_ :: PropName JSString
alignmentBaseline_ = PropName "alignmentBaseline"
allowReorder_ :: PropName JSString
allowReorder_ = PropName "allowReorder"
alphabetic_ :: PropName JSString
alphabetic_ = PropName "alphabetic"
amplitude_ :: PropName JSString
amplitude_ = PropName "amplitude"
arabicForm_ :: PropName JSString
arabicForm_ = PropName "arabicForm"
ascent_ :: PropName JSString
ascent_ = PropName "ascent"
attributeName_ :: PropName JSString
attributeName_ = PropName "attributeName"
attributeType_ :: PropName JSString
attributeType_ = PropName "attributeType"
autoReverse_ :: PropName JSString
autoReverse_ = PropName "autoReverse"
azimuth_ :: PropName JSString
azimuth_ = PropName "azimuth"
baseFrequency_ :: PropName JSString
baseFrequency_ = PropName "baseFrequency"
baseProfile_ :: PropName JSString
baseProfile_ = PropName "baseProfile"
baselineShift_ :: PropName JSString
baselineShift_ = PropName "baselineShift"
bbox_ :: PropName JSString
bbox_ = PropName "bbox"
begin_ :: PropName JSString
begin_ = PropName "begin"
bias_ :: PropName JSString
bias_ = PropName "bias"
by_ :: PropName JSString
by_ = PropName "by"
calcMode_ :: PropName JSString
calcMode_ = PropName "calcMode"
capHeight_ :: PropName JSString
capHeight_ = PropName "capHeight"
clip_ :: PropName JSString
clip_ = PropName "clip"
clipPathUnits_ :: PropName JSString
clipPathUnits_ = PropName "clipPathUnits"
clipRule_ :: PropName JSString
clipRule_ = PropName "clipRule"
colorInterpolation_ :: PropName JSString
colorInterpolation_ = PropName "colorInterpolation"
colorInterpolationFilters_ :: PropName JSString
colorInterpolationFilters_ = PropName "colorInterpolationFilters"
colorProfile_ :: PropName JSString
colorProfile_ = PropName "colorProfile"
colorRendering_ :: PropName JSString
colorRendering_ = PropName "colorRendering"
contentScriptType_ :: PropName JSString
contentScriptType_ = PropName "contentScriptType"
contentStyleType_ :: PropName JSString
contentStyleType_ = PropName "contentStyleType"
cursor_ :: PropName JSString
cursor_ = PropName "cursor"
cx_ :: PropName JSString
cx_ = PropName "cx"
cy_ :: PropName JSString
cy_ = PropName "cy"
d_ :: PropName JSString
d_ = PropName "d"
decelerate_ :: PropName JSString
decelerate_ = PropName "decelerate"
descent_ :: PropName JSString
descent_ = PropName "descent"
diffuseConstant_ :: PropName JSString
diffuseConstant_ = PropName "diffuseConstant"
direction_ :: PropName JSString
direction_ = PropName "direction"
display_ :: PropName JSString
display_ = PropName "display"
divisor_ :: PropName JSString
divisor_ = PropName "divisor"
dominantBaseline_ :: PropName JSString
dominantBaseline_ = PropName "dominantBaseline"
dur_ :: PropName JSString
dur_ = PropName "dur"
dx_ :: PropName JSString
dx_ = PropName "dx"
dy_ :: PropName JSString
dy_ = PropName "dy"
edgeMode_ :: PropName JSString
edgeMode_ = PropName "edgeMode"
elevation_ :: PropName JSString
elevation_ = PropName "elevation"
enableBackground_ :: PropName JSString
enableBackground_ = PropName "enableBackground"
end_ :: PropName JSString
end_ = PropName "end"
exponent_ :: PropName JSString
exponent_ = PropName "exponent"
externalResourcesRequired_ :: PropName JSString
externalResourcesRequired_ = PropName "externalResourcesRequired"
fill_ :: PropName JSString
fill_ = PropName "fill"
fillOpacity_ :: PropName JSString
fillOpacity_ = PropName "fillOpacity"
fillRule_ :: PropName JSString
fillRule_ = PropName "fillRule"
filter_ :: PropName JSString
filter_ = PropName "filter"
filterRes_ :: PropName JSString
filterRes_ = PropName "filterRes"
filterUnits_ :: PropName JSString
filterUnits_ = PropName "filterUnits"
floodColor_ :: PropName JSString
floodColor_ = PropName "floodColor"
floodOpacity_ :: PropName JSString
floodOpacity_ = PropName "floodOpacity"
focusable_ :: PropName JSString
focusable_ = PropName "focusable"
fontFamily_ :: PropName JSString
fontFamily_ = PropName "fontFamily"
fontSize_ :: PropName JSString
fontSize_ = PropName "fontSize"
fontSizeAdjust_ :: PropName JSString
fontSizeAdjust_ = PropName "fontSizeAdjust"
fontStretch_ :: PropName JSString
fontStretch_ = PropName "fontStretch"
fontStyle_ :: PropName JSString
fontStyle_ = PropName "fontStyle"
fontVariant_ :: PropName JSString
fontVariant_ = PropName "fontVariant"
fontWeight_ :: PropName JSString
fontWeight_ = PropName "fontWeight"
format_ :: PropName JSString
format_ = PropName "format"
from_ :: PropName JSString
from_ = PropName "from"
fx_ :: PropName JSString
fx_ = PropName "fx"
fy_ :: PropName JSString
fy_ = PropName "fy"
g1_ :: PropName JSString
g1_ = PropName "g1"
g2_ :: PropName JSString
g2_ = PropName "g2"
glyphName_ :: PropName JSString
glyphName_ = PropName "glyphName"
glyphOrientationHorizontal_ :: PropName JSString
glyphOrientationHorizontal_ = PropName "glyphOrientationHorizontal"
glyphOrientationVertical_ :: PropName JSString
glyphOrientationVertical_ = PropName "glyphOrientationVertical"
glyphRef_ :: PropName JSString
glyphRef_ = PropName "glyphRef"
gradientTransform_ :: PropName JSString
gradientTransform_ = PropName "gradientTransform"
gradientUnits_ :: PropName JSString
gradientUnits_ = PropName "gradientUnits"
hanging_ :: PropName JSString
hanging_ = PropName "hanging"
horizAdvX_ :: PropName JSString
horizAdvX_ = PropName "horizAdvX"
horizOriginX_ :: PropName JSString
horizOriginX_ = PropName "horizOriginX"
ideographic_ :: PropName JSString
ideographic_ = PropName "ideographic"
imageRendering_ :: PropName JSString
imageRendering_ = PropName "imageRendering"
in_ :: PropName JSString
in_ = PropName "in"
in2_ :: PropName JSString
in2_ = PropName "in2"
intercept_ :: PropName JSString
intercept_ = PropName "intercept"
k_ :: PropName JSString
k_ = PropName "k"
k1_ :: PropName JSString
k1_ = PropName "k1"
k2_ :: PropName JSString
k2_ = PropName "k2"
k3_ :: PropName JSString
k3_ = PropName "k3"
k4_ :: PropName JSString
k4_ = PropName "k4"
kernelMatrix_ :: PropName JSString
kernelMatrix_ = PropName "kernelMatrix"
kernelUnitLength_ :: PropName JSString
kernelUnitLength_ = PropName "kernelUnitLength"
kerning_ :: PropName JSString
kerning_ = PropName "kerning"
keyPoints_ :: PropName JSString
keyPoints_ = PropName "keyPoints"
keySplines_ :: PropName JSString
keySplines_ = PropName "keySplines"
keyTimes_ :: PropName JSString
keyTimes_ = PropName "keyTimes"
lengthAdjust_ :: PropName JSString
lengthAdjust_ = PropName "lengthAdjust"
letterSpacing_ :: PropName JSString
letterSpacing_ = PropName "letterSpacing"
lightingColor_ :: PropName JSString
lightingColor_ = PropName "lightingColor"
limitingConeAngle_ :: PropName JSString
limitingConeAngle_ = PropName "limitingConeAngle"
local_ :: PropName JSString
local_ = PropName "local"
markerEnd_ :: PropName JSString
markerEnd_ = PropName "markerEnd"
markerHeight_ :: PropName JSString
markerHeight_ = PropName "markerHeight"
markerMid_ :: PropName JSString
markerMid_ = PropName "markerMid"
markerStart_ :: PropName JSString
markerStart_ = PropName "markerStart"
markerUnits_ :: PropName JSString
markerUnits_ = PropName "markerUnits"
markerWidth_ :: PropName JSString
markerWidth_ = PropName "markerWidth"
mask_ :: ElementOrProp p JSString => p
mask_ = fst (symbolName "mask" :: ElementOrProp p JSString => (p, Proxy JSString))
maskContentUnits_ :: PropName JSString
maskContentUnits_ = PropName "maskContentUnits"
maskUnits_ :: PropName JSString
maskUnits_ = PropName "maskUnits"
mathematical_ :: PropName JSString
mathematical_ = PropName "mathematical"
mode_ :: PropName JSString
mode_ = PropName "mode"
numOctaves_ :: PropName JSString
numOctaves_ = PropName "numOctaves"
offset_ :: PropName JSString
offset_ = PropName "offset"
opacity_ :: PropName JSString
opacity_ = PropName "opacity"
operator_ :: PropName JSString
operator_ = PropName "operator"
order_ :: PropName JSString
order_ = PropName "order"
orient_ :: PropName JSString
orient_ = PropName "orient"
orientation_ :: PropName JSString
orientation_ = PropName "orientation"
origin_ :: PropName JSString
origin_ = PropName "origin"
overflow_ :: PropName JSString
overflow_ = PropName "overflow"
overlinePosition_ :: PropName JSString
overlinePosition_ = PropName "overlinePosition"
overlineThickness_ :: PropName JSString
overlineThickness_ = PropName "overlineThickness"
paintOrder_ :: PropName JSString
paintOrder_ = PropName "paintOrder"
panose1_ :: PropName JSString
panose1_ = PropName "panose1"
pathLength_ :: PropName JSString
pathLength_ = PropName "pathLength"
patternContentUnits_ :: PropName JSString
patternContentUnits_ = PropName "patternContentUnits"
patternTransform_ :: PropName JSString
patternTransform_ = PropName "patternTransform"
patternUnits_ :: PropName JSString
patternUnits_ = PropName "patternUnits"
pointerEvents_ :: PropName JSString
pointerEvents_ = PropName "pointerEvents"
points_ :: PropName JSString
points_ = PropName "points"
pointsAtX_ :: PropName JSString
pointsAtX_ = PropName "pointsAtX"
pointsAtY_ :: PropName JSString
pointsAtY_ = PropName "pointsAtY"
pointsAtZ_ :: PropName JSString
pointsAtZ_ = PropName "pointsAtZ"
preserveAlpha_ :: PropName JSString
preserveAlpha_ = PropName "preserveAlpha"
preserveAspectRatio_ :: PropName JSString
preserveAspectRatio_ = PropName "preserveAspectRatio"
primitiveUnits_ :: PropName JSString
primitiveUnits_ = PropName "primitiveUnits"
r_ :: PropName JSString
r_ = PropName "r"
radius_ :: PropName JSString
radius_ = PropName "radius"
refX_ :: PropName JSString
refX_ = PropName "refX"
refY_ :: PropName JSString
refY_ = PropName "refY"
renderingIntent_ :: PropName JSString
renderingIntent_ = PropName "renderingIntent"
repeatCount_ :: PropName JSString
repeatCount_ = PropName "repeatCount"
repeatDur_ :: PropName JSString
repeatDur_ = PropName "repeatDur"
requiredExtensions_ :: PropName JSString
requiredExtensions_ = PropName "requiredExtensions"
requiredFeatures_ :: PropName JSString
requiredFeatures_ = PropName "requiredFeatures"
restart_ :: PropName JSString
restart_ = PropName "restart"
result_ :: PropName JSString
result_ = PropName "result"
rotate_ :: PropName JSString
rotate_ = PropName "rotate"
rx_ :: PropName JSString
rx_ = PropName "rx"
ry_ :: PropName JSString
ry_ = PropName "ry"
scale_ :: PropName JSString
scale_ = PropName "scale"
seed_ :: PropName JSString
seed_ = PropName "seed"
shapeRendering_ :: PropName JSString
shapeRendering_ = PropName "shapeRendering"
slope_ :: PropName JSString
slope_ = PropName "slope"
spacing_ :: PropName JSString
spacing_ = PropName "spacing"
specularConstant_ :: PropName JSString
specularConstant_ = PropName "specularConstant"
specularExponent_ :: PropName JSString
specularExponent_ = PropName "specularExponent"
speed_ :: PropName JSString
speed_ = PropName "speed"
spreadMethod_ :: PropName JSString
spreadMethod_ = PropName "spreadMethod"
startOffset_ :: PropName JSString
startOffset_ = PropName "startOffset"
stdDeviation_ :: PropName JSString
stdDeviation_ = PropName "stdDeviation"
stemh_ :: PropName JSString
stemh_ = PropName "stemh"
stemv_ :: PropName JSString
stemv_ = PropName "stemv"
stitchTiles_ :: PropName JSString
stitchTiles_ = PropName "stitchTiles"
stopColor_ :: PropName JSString
stopColor_ = PropName "stopColor"
stopOpacity_ :: PropName JSString
stopOpacity_ = PropName "stopOpacity"
strikethroughPosition_ :: PropName JSString
strikethroughPosition_ = PropName "strikethroughPosition"
strikethroughThickness_ :: PropName JSString
strikethroughThickness_ = PropName "strikethroughThickness"
string_ :: PropName JSString
string_ = PropName "string"
stroke_ :: PropName JSString
stroke_ = PropName "stroke"
strokeDasharray_ :: PropName JSString
strokeDasharray_ = PropName "strokeDasharray"
strokeDashoffset_ :: PropName JSString
strokeDashoffset_ = PropName "strokeDashoffset"
strokeLinecap_ :: PropName JSString
strokeLinecap_ = PropName "strokeLinecap"
strokeLinejoin_ :: PropName JSString
strokeLinejoin_ = PropName "strokeLinejoin"
strokeMiterlimit_ :: PropName JSString
strokeMiterlimit_ = PropName "strokeMiterlimit"
strokeOpacity_ :: PropName JSString
strokeOpacity_ = PropName "strokeOpacity"
strokeWidth_ :: PropName JSString
strokeWidth_ = PropName "strokeWidth"
surfaceScale_ :: PropName JSString
surfaceScale_ = PropName "surfaceScale"
systemLanguage_ :: PropName JSString
systemLanguage_ = PropName "systemLanguage"
tableValues_ :: PropName JSString
tableValues_ = PropName "tableValues"
targetX_ :: PropName JSString
targetX_ = PropName "targetX"
targetY_ :: PropName JSString
targetY_ = PropName "targetY"
textAnchor_ :: PropName JSString
textAnchor_ = PropName "textAnchor"
textDecoration_ :: PropName JSString
textDecoration_ = PropName "textDecoration"
textLength_ :: PropName JSString
textLength_ = PropName "textLength"
textRendering_ :: PropName JSString
textRendering_ = PropName "textRendering"
to_ :: PropName JSString
to_ = PropName "to"
transform_ :: PropName JSString
transform_ = PropName "transform"
u1_ :: PropName JSString
u1_ = PropName "u1"
u2_ :: PropName JSString
u2_ = PropName "u2"
underlinePosition_ :: PropName JSString
underlinePosition_ = PropName "underlinePosition"
underlineThickness_ :: PropName JSString
underlineThickness_ = PropName "underlineThickness"
unicode_ :: PropName JSString
unicode_ = PropName "unicode"
unicodeBidi_ :: PropName JSString
unicodeBidi_ = PropName "unicodeBidi"
unicodeRange_ :: PropName JSString
unicodeRange_ = PropName "unicodeRange"
unitsPerEm_ :: PropName JSString
unitsPerEm_ = PropName "unitsPerEm"
vAlphabetic_ :: PropName JSString
vAlphabetic_ = PropName "vAlphabetic"
vHanging_ :: PropName JSString
vHanging_ = PropName "vHanging"
vIdeographic_ :: PropName JSString
vIdeographic_ = PropName "vIdeographic"
vMathematical_ :: PropName JSString
vMathematical_ = PropName "vMathematical"
values_ :: PropName JSString
values_ = PropName "values"
vectorEffect_ :: PropName JSString
vectorEffect_ = PropName "vectorEffect"
version_ :: PropName JSString
version_ = PropName "version"
vertAdvY_ :: PropName JSString
vertAdvY_ = PropName "vertAdvY"
vertOriginX_ :: PropName JSString
vertOriginX_ = PropName "vertOriginX"
vertOriginY_ :: PropName JSString
vertOriginY_ = PropName "vertOriginY"
viewBox_ :: PropName JSString
viewBox_ = PropName "viewBox"
viewTarget_ :: PropName JSString
viewTarget_ = PropName "viewTarget"
visibility_ :: PropName JSString
visibility_ = PropName "visibility"
widths_ :: PropName JSString
widths_ = PropName "widths"
wordSpacing_ :: PropName JSString
wordSpacing_ = PropName "wordSpacing"
writingMode_ :: PropName JSString
writingMode_ = PropName "writingMode"
x_ :: PropName JSString
x_ = PropName "x"
x1_ :: PropName JSString
x1_ = PropName "x1"
x2_ :: PropName JSString
x2_ = PropName "x2"
xChannelSelector_ :: PropName JSString
xChannelSelector_ = PropName "xChannelSelector"
xHeight_ :: PropName JSString
xHeight_ = PropName "xHeight"
xlinkActuate_ :: PropName JSString
xlinkActuate_ = PropName "xlinkActuate"
xlinkArcrole_ :: PropName JSString
xlinkArcrole_ = PropName "xlinkArcrole"
xlinkHref_ :: PropName JSString
xlinkHref_ = PropName "xlinkHref"
xlinkRole_ :: PropName JSString
xlinkRole_ = PropName "xlinkRole"
xlinkShow_ :: PropName JSString
xlinkShow_ = PropName "xlinkShow"
xlinkTitle_ :: PropName JSString
xlinkTitle_ = PropName "xlinkTitle"
xlinkType_ :: PropName JSString
xlinkType_ = PropName "xlinkType"
xmlBase_ :: PropName JSString
xmlBase_ = PropName "xmlBase"
xmlLang_ :: PropName JSString
xmlLang_ = PropName "xmlLang"
xmlSpace_ :: PropName JSString
xmlSpace_ = PropName "xmlSpace"
y_ :: PropName JSString
y_ = PropName "y"
y1_ :: PropName JSString
y1_ = PropName "y1"
y2_ :: PropName JSString
y2_ = PropName "y2"
yChannelSelector_ :: PropName JSString
yChannelSelector_ = PropName "yChannelSelector"
z_ :: PropName JSString
z_ = PropName "z"
zoomAndPan_ :: PropName JSString
zoomAndPan_ = PropName "zoomAndPan"

dataAttr :: JSString -> PropName JSString
dataAttr = PropName . JSString.append "data-"

ariaAttr :: JSString -> PropName JSString
ariaAttr = PropName . JSString.append "aria-"
