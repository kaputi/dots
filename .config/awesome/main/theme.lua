-- Standard awesome library
-- local gears = require("gears")
local awful = require("awful")

-- Theme handling library
local beautiful = require("beautiful")

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
local my_theme_path = awful.util.getdir("config") .. "themes/myTheme/theme.lua"

-- Themes define colours, icons, font and wallpapers.
-- beautiful.init("/usr/share/awesome/themes/default/theme.lua")
-- beautiful.init(gears.filesystem.get_themes_dir() .. "zenburn/theme.lua")
beautiful.init(my_theme_path)

-- if (RC.vars.wallpaper) then
--   local wallpaper = RC.vars.wallpaper
--   if awful.util.file_readable(wallpaper) then theme.wallpaper = wallpaper end
-- end

-- -- Wallpaper
-- if beautiful.wallpaper then
--   for s = 1, screen.count() do
--     gears.wallpaper.maximized(beautiful.wallpaper, s, true)
--   end
-- end
