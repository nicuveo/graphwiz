module Text.Dot.Attributes where

import "this" Prelude

import Control.Lens

import Text.Dot.Types


--------------------------------------------------------------------------------
-- Attributes access

-- | Retrieves the 'Attributes' of the given 'Entity'.
--
-- Given an entity attribute, return a lens to the corresponding attributes map
-- in a given 'DotGraph', which is an internal opaque type. This is meant to be
-- used inside the 'DotT' monad, relying on the fact that it is a State monad
-- under the hood.
--
-- Using @OverloadedLists@ makes working with the full attributes map a bit
-- easier.
--
-- @
--     graph do
--       x <- node "x"
--
--       -- replaces the entire mapping (erases the label!)
--       attributes e .= [("fontcolor", "red")]
--
--       -- combines the existing mapping with the new one, favoring old values
--       attributes e <>= [("fontcolor", "blue"), ("fontsize", "12")]
--
--       -- combines the existing mapping with the new one, favoring new values
--       attributes e <>:= [("fontcolor", "blue"), ("fontsize", "12")]
-- @
--
-- This function is best used with the provided field accessors, such as
-- 'fontcolor', to be more explicit about the way to deal with prevous values.
attributes :: Entity -> Lens' DotGraph Attributes
attributes e = entityAttributes . at e . non mempty

-- | Retrieves the default 'Attributes' of the given 'EntityType'.
--
-- Given an entity type, return a lens to the corresponding default attributes
-- map in a given 'DotGraph', which is an internal opaque type. This is meant to
-- be used inside the 'DotT' monad, relying on the fact that it is a State monad
-- under the hood.
--
-- After modifying the defaults for a given entity type, any new such entity
-- will have its attributes set to the new default values.
--
-- @
--     graph do
--       x <- node "x"
--       use (its color) -- Nothing
--
--       defaults Node color ?= "red"
--
--       y <- node "y"
--       use (its color) -- Just "red"
-- @
--
-- This function is best used with the provided field accessors, such as
-- 'fontcolor', to be more explicit about the way to deal with prevous values.
defaults :: EntityType -> Lens' Attributes (Maybe Text) -> Lens' DotGraph (Maybe Text)
defaults t l f d = (defaultAttributes . at t . non mempty . l) f d

-- | Retrieves the 'Attributes' of the latest created 'Entity'.
--
-- This lens focuses on a 'DotGraph', an opaque internal type, and is meant to
-- be used from within the 'DotT' monad (see 'attributes'). It always focuses on
-- the latest created entity: the top-level graph, a node, an edge, a subgraph,
-- or a cluster.
--
-- @
--     graph do
--       its title .= "my graph"
--
--       node "bar"
--       its fontsize .= "34"
--
--       edge "bar" "bar"
--       its style .= "dotted"
--
--       cluster do
--         its label .= "cluster"
-- @
its :: Lens' Attributes (Maybe Text) -> Lens' DotGraph (Maybe Text)
its l f d = (attributes (_latest d) . l) f d

-- | Simple alias for 'at'.
--
-- This makes the code a tiny bit more natural when accessing fields by name:
--
-- @
--     graph do
--       x <- node "x"
--
--       -- replace the old value, if any
--       its (attribute "fontcolor") ?= "red"
--       its (attribute "fontcolor") .= Just "red"
--
--       -- erase the attribute
--       its (attribute "fontcolor") .= Nothing
--
--       -- set the value if it wasn't previously set
--       its (attribute "fontcolor") %= ifAbsent "blue"
-- @
attribute :: Text -> Lens' Attributes (Maybe Text)
attribute = at

-- | Replaces a maybe value only if it wasn't set.
--
-- >>> ifAbsent "foo" Nothing
-- Just "foo"
--
-- >>> ifAbsent "foo" (Just "bar")
-- Just "bar"
--
-- This is best used in conjuction with 'attribute', or one of the explicit
-- attribute accessors.
ifAbsent :: a -> Maybe a -> Maybe a
ifAbsent x m = m <|> Just x


--------------------------------------------------------------------------------
-- All attributes

-- | Maps to the "_background" attribute.
background         :: Lens' Attributes (Maybe Text)
background         = attribute "_background"

-- | Maps to the "Dackground" attribute.
damping            :: Lens' Attributes (Maybe Text)
damping            = attribute "Damping"

-- | Maps to the "cluster" attribute.
isCcluster         :: Lens' Attributes (Maybe Text)
isCcluster         = attribute "cluster"

-- | Maps to the "K" attribute.
k                  :: Lens' Attributes (Maybe Text)
k                  = attribute "K"

-- | Maps to the "class" attribute.
svgClass           :: Lens' Attributes (Maybe Text)
svgClass           = attribute "class"

-- | Maps to the "id" attribute.
svgID              :: Lens' Attributes (Maybe Text)
svgID              = attribute "id"

-- | Maps to the "TBbalance" attribute.
tbbalance          :: Lens' Attributes (Maybe Text)
tbbalance          = attribute "TBbalance"

-- | Maps to the "URL" attribute.
url                :: Lens' Attributes (Maybe Text)
url                = attribute "URL"

area               :: Lens' Attributes (Maybe Text)
area               = attribute "area"
arrowhead          :: Lens' Attributes (Maybe Text)
arrowhead          = attribute "arrowhead"
arrowsize          :: Lens' Attributes (Maybe Text)
arrowsize          = attribute "arrowsize"
arrowtail          :: Lens' Attributes (Maybe Text)
arrowtail          = attribute "arrowtail"
bb                 :: Lens' Attributes (Maybe Text)
bb                 = attribute "bb"
beautify           :: Lens' Attributes (Maybe Text)
beautify           = attribute "beautify"
bgcolor            :: Lens' Attributes (Maybe Text)
bgcolor            = attribute "bgcolor"
black              :: Lens' Attributes (Maybe Text)
black              = attribute "black"
center             :: Lens' Attributes (Maybe Text)
center             = attribute "center"
charset            :: Lens' Attributes (Maybe Text)
charset            = attribute "charset"
clusterrank        :: Lens' Attributes (Maybe Text)
clusterrank        = attribute "clusterrank"
color              :: Lens' Attributes (Maybe Text)
color              = attribute "color"
colorscheme        :: Lens' Attributes (Maybe Text)
colorscheme        = attribute "colorscheme"
comment            :: Lens' Attributes (Maybe Text)
comment            = attribute "comment"
compound           :: Lens' Attributes (Maybe Text)
compound           = attribute "compound"
concentrate        :: Lens' Attributes (Maybe Text)
concentrate        = attribute "concentrate"
constraint         :: Lens' Attributes (Maybe Text)
constraint         = attribute "constraint"
decorate           :: Lens' Attributes (Maybe Text)
decorate           = attribute "decorate"
defaultdist        :: Lens' Attributes (Maybe Text)
defaultdist        = attribute "defaultdist"
dim                :: Lens' Attributes (Maybe Text)
dim                = attribute "dim"
dimen              :: Lens' Attributes (Maybe Text)
dimen              = attribute "dimen"
dir                :: Lens' Attributes (Maybe Text)
dir                = attribute "dir"
diredgeconstraints :: Lens' Attributes (Maybe Text)
diredgeconstraints = attribute "diredgeconstraints"
distortion         :: Lens' Attributes (Maybe Text)
distortion         = attribute "distortion"
dpi                :: Lens' Attributes (Maybe Text)
dpi                = attribute "dpi"
edgeURL            :: Lens' Attributes (Maybe Text)
edgeURL            = attribute "edgeURL"
edgehref           :: Lens' Attributes (Maybe Text)
edgehref           = attribute "edgehref"
edgetarget         :: Lens' Attributes (Maybe Text)
edgetarget         = attribute "edgetarget"
edgetooltip        :: Lens' Attributes (Maybe Text)
edgetooltip        = attribute "edgetooltip"
epsilon            :: Lens' Attributes (Maybe Text)
epsilon            = attribute "epsilon"
esep               :: Lens' Attributes (Maybe Text)
esep               = attribute "esep"
fillcolor          :: Lens' Attributes (Maybe Text)
fillcolor          = attribute "fillcolor"
fixedsize          :: Lens' Attributes (Maybe Text)
fixedsize          = attribute "fixedsize"
fontcolor          :: Lens' Attributes (Maybe Text)
fontcolor          = attribute "fontcolor"
fontname           :: Lens' Attributes (Maybe Text)
fontname           = attribute "fontname"
fontnames          :: Lens' Attributes (Maybe Text)
fontnames          = attribute "fontnames"
fontpath           :: Lens' Attributes (Maybe Text)
fontpath           = attribute "fontpath"
fontsize           :: Lens' Attributes (Maybe Text)
fontsize           = attribute "fontsize"
forcelabels        :: Lens' Attributes (Maybe Text)
forcelabels        = attribute "forcelabels"
gradientangle      :: Lens' Attributes (Maybe Text)
gradientangle      = attribute "gradientangle"
group              :: Lens' Attributes (Maybe Text)
group              = attribute "group"
headURL            :: Lens' Attributes (Maybe Text)
headURL            = attribute "headURL"
head_lp            :: Lens' Attributes (Maybe Text)
head_lp            = attribute "head_lp"
headclip           :: Lens' Attributes (Maybe Text)
headclip           = attribute "headclip"
headhref           :: Lens' Attributes (Maybe Text)
headhref           = attribute "headhref"
headlabel          :: Lens' Attributes (Maybe Text)
headlabel          = attribute "headlabel"
headport           :: Lens' Attributes (Maybe Text)
headport           = attribute "headport"
headtarget         :: Lens' Attributes (Maybe Text)
headtarget         = attribute "headtarget"
headtooltip        :: Lens' Attributes (Maybe Text)
headtooltip        = attribute "headtooltip"
height             :: Lens' Attributes (Maybe Text)
height             = attribute "height"
href               :: Lens' Attributes (Maybe Text)
href               = attribute "href"
image              :: Lens' Attributes (Maybe Text)
image              = attribute "image"
imagepath          :: Lens' Attributes (Maybe Text)
imagepath          = attribute "imagepath"
imagepos           :: Lens' Attributes (Maybe Text)
imagepos           = attribute "imagepos"
imagescale         :: Lens' Attributes (Maybe Text)
imagescale         = attribute "imagescale"
inputscale         :: Lens' Attributes (Maybe Text)
inputscale         = attribute "inputscale"
label              :: Lens' Attributes (Maybe Text)
label              = attribute "label"
labelURL           :: Lens' Attributes (Maybe Text)
labelURL           = attribute "labelURL"
label_scheme       :: Lens' Attributes (Maybe Text)
label_scheme       = attribute "label_scheme"
labelangle         :: Lens' Attributes (Maybe Text)
labelangle         = attribute "labelangle"
labeldistance      :: Lens' Attributes (Maybe Text)
labeldistance      = attribute "labeldistance"
labelfloat         :: Lens' Attributes (Maybe Text)
labelfloat         = attribute "labelfloat"
labelfontcolor     :: Lens' Attributes (Maybe Text)
labelfontcolor     = attribute "labelfontcolor"
labelfontname      :: Lens' Attributes (Maybe Text)
labelfontname      = attribute "labelfontname"
labelfontsize      :: Lens' Attributes (Maybe Text)
labelfontsize      = attribute "labelfontsize"
labelhref          :: Lens' Attributes (Maybe Text)
labelhref          = attribute "labelhref"
labeljust          :: Lens' Attributes (Maybe Text)
labeljust          = attribute "labeljust"
labelloc           :: Lens' Attributes (Maybe Text)
labelloc           = attribute "labelloc"
labeltarget        :: Lens' Attributes (Maybe Text)
labeltarget        = attribute "labeltarget"
labeltooltip       :: Lens' Attributes (Maybe Text)
labeltooltip       = attribute "labeltooltip"
landscape          :: Lens' Attributes (Maybe Text)
landscape          = attribute "landscape"
layer              :: Lens' Attributes (Maybe Text)
layer              = attribute "layer"
layerlistsep       :: Lens' Attributes (Maybe Text)
layerlistsep       = attribute "layerlistsep"
layers             :: Lens' Attributes (Maybe Text)
layers             = attribute "layers"
layerselect        :: Lens' Attributes (Maybe Text)
layerselect        = attribute "layerselect"
layersep           :: Lens' Attributes (Maybe Text)
layersep           = attribute "layersep"
layout             :: Lens' Attributes (Maybe Text)
layout             = attribute "layout"
len                :: Lens' Attributes (Maybe Text)
len                = attribute "len"
levels             :: Lens' Attributes (Maybe Text)
levels             = attribute "levels"
levelsgap          :: Lens' Attributes (Maybe Text)
levelsgap          = attribute "levelsgap"
lhead              :: Lens' Attributes (Maybe Text)
lhead              = attribute "lhead"
lheight            :: Lens' Attributes (Maybe Text)
lheight            = attribute "lheight"
linelength         :: Lens' Attributes (Maybe Text)
linelength         = attribute "linelength"
lp                 :: Lens' Attributes (Maybe Text)
lp                 = attribute "lp"
ltail              :: Lens' Attributes (Maybe Text)
ltail              = attribute "ltail"
lwidth             :: Lens' Attributes (Maybe Text)
lwidth             = attribute "lwidth"
margin             :: Lens' Attributes (Maybe Text)
margin             = attribute "margin"
maxiter            :: Lens' Attributes (Maybe Text)
maxiter            = attribute "maxiter"
mclimit            :: Lens' Attributes (Maybe Text)
mclimit            = attribute "mclimit"
mindist            :: Lens' Attributes (Maybe Text)
mindist            = attribute "mindist"
minlen             :: Lens' Attributes (Maybe Text)
minlen             = attribute "minlen"
mode               :: Lens' Attributes (Maybe Text)
mode               = attribute "mode"
model              :: Lens' Attributes (Maybe Text)
model              = attribute "model"
newrank            :: Lens' Attributes (Maybe Text)
newrank            = attribute "newrank"
nodesep            :: Lens' Attributes (Maybe Text)
nodesep            = attribute "nodesep"
nojustify          :: Lens' Attributes (Maybe Text)
nojustify          = attribute "nojustify"
normalize          :: Lens' Attributes (Maybe Text)
normalize          = attribute "normalize"
notranslate        :: Lens' Attributes (Maybe Text)
notranslate        = attribute "notranslate"
nslimit            :: Lens' Attributes (Maybe Text)
nslimit            = attribute "nslimit"
nslimit1           :: Lens' Attributes (Maybe Text)
nslimit1           = attribute "nslimit1"
oneblock           :: Lens' Attributes (Maybe Text)
oneblock           = attribute "oneblock"
ordering           :: Lens' Attributes (Maybe Text)
ordering           = attribute "ordering"
orientation        :: Lens' Attributes (Maybe Text)
orientation        = attribute "orientation"
outputorder        :: Lens' Attributes (Maybe Text)
outputorder        = attribute "outputorder"
overlap            :: Lens' Attributes (Maybe Text)
overlap            = attribute "overlap"
overlap_scaling    :: Lens' Attributes (Maybe Text)
overlap_scaling    = attribute "overlap_scaling"
overlap_shrink     :: Lens' Attributes (Maybe Text)
overlap_shrink     = attribute "overlap_shrink"
pack               :: Lens' Attributes (Maybe Text)
pack               = attribute "pack"
packmode           :: Lens' Attributes (Maybe Text)
packmode           = attribute "packmode"
pad                :: Lens' Attributes (Maybe Text)
pad                = attribute "pad"
page               :: Lens' Attributes (Maybe Text)
page               = attribute "page"
pagedir            :: Lens' Attributes (Maybe Text)
pagedir            = attribute "pagedir"
pencolor           :: Lens' Attributes (Maybe Text)
pencolor           = attribute "pencolor"
penwidth           :: Lens' Attributes (Maybe Text)
penwidth           = attribute "penwidth"
peripheries        :: Lens' Attributes (Maybe Text)
peripheries        = attribute "peripheries"
pin                :: Lens' Attributes (Maybe Text)
pin                = attribute "pin"
pos                :: Lens' Attributes (Maybe Text)
pos                = attribute "pos"
quadtree           :: Lens' Attributes (Maybe Text)
quadtree           = attribute "quadtree"
quantum            :: Lens' Attributes (Maybe Text)
quantum            = attribute "quantum"
rank               :: Lens' Attributes (Maybe Text)
rank               = attribute "rank"
rankdir            :: Lens' Attributes (Maybe Text)
rankdir            = attribute "rankdir"
ranksep            :: Lens' Attributes (Maybe Text)
ranksep            = attribute "ranksep"
ratio              :: Lens' Attributes (Maybe Text)
ratio              = attribute "ratio"
rects              :: Lens' Attributes (Maybe Text)
rects              = attribute "rects"
regular            :: Lens' Attributes (Maybe Text)
regular            = attribute "regular"
remincross         :: Lens' Attributes (Maybe Text)
remincross         = attribute "remincross"
repulsiveforce     :: Lens' Attributes (Maybe Text)
repulsiveforce     = attribute "repulsiveforce"
resolution         :: Lens' Attributes (Maybe Text)
resolution         = attribute "resolution"
root               :: Lens' Attributes (Maybe Text)
root               = attribute "root"
rotate             :: Lens' Attributes (Maybe Text)
rotate             = attribute "rotate"
rotation           :: Lens' Attributes (Maybe Text)
rotation           = attribute "rotation"
samehead           :: Lens' Attributes (Maybe Text)
samehead           = attribute "samehead"
sametail           :: Lens' Attributes (Maybe Text)
sametail           = attribute "sametail"
samplepoints       :: Lens' Attributes (Maybe Text)
samplepoints       = attribute "samplepoints"
scale              :: Lens' Attributes (Maybe Text)
scale              = attribute "scale"
searchsize         :: Lens' Attributes (Maybe Text)
searchsize         = attribute "searchsize"
sep                :: Lens' Attributes (Maybe Text)
sep                = attribute "sep"
shape              :: Lens' Attributes (Maybe Text)
shape              = attribute "shape"
shapefile          :: Lens' Attributes (Maybe Text)
shapefile          = attribute "shapefile"
showboxes          :: Lens' Attributes (Maybe Text)
showboxes          = attribute "showboxes"
sides              :: Lens' Attributes (Maybe Text)
sides              = attribute "sides"
size               :: Lens' Attributes (Maybe Text)
size               = attribute "size"
skew               :: Lens' Attributes (Maybe Text)
skew               = attribute "skew"
smoothing          :: Lens' Attributes (Maybe Text)
smoothing          = attribute "smoothing"
sortv              :: Lens' Attributes (Maybe Text)
sortv              = attribute "sortv"
splines            :: Lens' Attributes (Maybe Text)
splines            = attribute "splines"
start              :: Lens' Attributes (Maybe Text)
start              = attribute "start"
style              :: Lens' Attributes (Maybe Text)
style              = attribute "style"
stylesheet         :: Lens' Attributes (Maybe Text)
stylesheet         = attribute "stylesheet"
tailURL            :: Lens' Attributes (Maybe Text)
tailURL            = attribute "tailURL"
tail_lp            :: Lens' Attributes (Maybe Text)
tail_lp            = attribute "tail_lp"
tailclip           :: Lens' Attributes (Maybe Text)
tailclip           = attribute "tailclip"
tailhref           :: Lens' Attributes (Maybe Text)
tailhref           = attribute "tailhref"
taillabel          :: Lens' Attributes (Maybe Text)
taillabel          = attribute "taillabel"
tailport           :: Lens' Attributes (Maybe Text)
tailport           = attribute "tailport"
tailtarget         :: Lens' Attributes (Maybe Text)
tailtarget         = attribute "tailtarget"
tailtooltip        :: Lens' Attributes (Maybe Text)
tailtooltip        = attribute "tailtooltip"
target             :: Lens' Attributes (Maybe Text)
target             = attribute "target"
tooltip            :: Lens' Attributes (Maybe Text)
tooltip            = attribute "tooltip"
truecolor          :: Lens' Attributes (Maybe Text)
truecolor          = attribute "truecolor"
vertices           :: Lens' Attributes (Maybe Text)
vertices           = attribute "vertices"
viewport           :: Lens' Attributes (Maybe Text)
viewport           = attribute "viewport"
voro_margin        :: Lens' Attributes (Maybe Text)
voro_margin        = attribute "voro_margin"
weight             :: Lens' Attributes (Maybe Text)
weight             = attribute "weight"
width              :: Lens' Attributes (Maybe Text)
width              = attribute "width"
xdotversion        :: Lens' Attributes (Maybe Text)
xdotversion        = attribute "xdotversion"
xlabel             :: Lens' Attributes (Maybe Text)
xlabel             = attribute "xlabel"
xlp                :: Lens' Attributes (Maybe Text)
xlp                = attribute "xlp"
z                  :: Lens' Attributes (Maybe Text)
z                  = attribute "z"
