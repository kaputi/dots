local awful = require("awful")
awful.util = require("awful.util")

theme_path = awful.util.getdir("config") .. "/themes/myTheme/"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- default variables

theme = {}

dofile(theme_path .. "elements.lua")
-- dofile(theme_path .. "titlebar.lua")
dofile(theme_path .. "layouts.lua")

theme.awesome_icon       = theme_path .. "launcher/logo20_arch.png"
theme.awesome_subicon    = theme_path .. "launcher/logo20_arch.png"

return theme


