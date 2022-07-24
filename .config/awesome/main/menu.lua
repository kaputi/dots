-- Standard awesome library
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- Theme handling library
local beautiful = require("beautiful") -- for awesome.icon

local M = {}  -- menu
local _M = {} -- module

-- reading
-- https://awesomewm.org/apidoc/popups%20and%20bars/awful.menu.html

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- This is used later as the default terminal and editor to run.
-- local terminal = "xfce4-terminal"
local terminal = RC.vars.terminal

-- Variable definitions
-- This is used later as the default terminal and editor to run.
local editor = os.getenv("EDITOR") or "nano"
local editor_cmd = terminal .. " -e " .. editor

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

M.awesome = {
  { "hotkeys", function() 
      hotkeys_popup.show_help(nil, awful.screen.focused()) 
    end },
  { "manual", terminal .. " -e man awesome" },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "Terminal", terminal },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end }
}

M.favorite = {
  { "chromium", "google-chrome-stable" },
  { "libreoffice", "libreoffice" },
  { "transmission", "transmission-gtk" },
  { "gimp", "gimp" },
  { "inkscape", "inkscape" },
}

M.power= {
    {"Shut Down", function() awful.spawn.with_shell("shutdown now") end},
    {"Restart", function() awful.spawn.with_shell("reboot") end},
    {"Logout", function() awesome.quit() end},
    {"Suspend", function() awful.spawn.with_shell("systemctl suspend") end},
    {"Hibernate", function() awful.spawn.with_shell("blurlock && systemctl hibernate") end},
}
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()

  -- Main Menu
  local menu_items = {
    { "awesome", M.awesome, beautiful.awesome_subicon },
    { "open terminal", terminal },
    { "favorite", M.favorite },
    {"power", M.power}
  }

  return menu_items
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
