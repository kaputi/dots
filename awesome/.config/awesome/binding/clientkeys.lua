-- Standard Awesome library
local gears = require("gears")
local awful = require("awful")
-- Custom Local Library
-- local titlebar = require("anybox.titlebar")

local _M = {}
local modkey = RC.vars.modkey

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local clientkeys = gears.table.join(awful.key({modkey}, "f", function(c)
    c.fullscreen = not c.fullscreen
    c:raise()
  end, {description = "Fullscreen", group = "client"}),
      awful.key({modkey}, "q", function(c)
        c:kill()
      end, {description = "Quit", group = "client"}),
      awful.key({modkey, "Control"}, "space", awful.client.floating.toggle,
          {description = "Toggle Floating", group = "client"}),
      awful.key({modkey, "Control"}, "Return", function(c)
        c:swap(awful.client.getmaster())
      end, {description = "Set as Master", group = "client"}),
      -- awful.key({modkey}, "o", function(c)
      --   c:move_to_screen()
      -- end, {description = "move to screen", group = "client"}),
      awful.key({modkey}, "t", function(c)
        c.ontop = not c.ontop
      end, {description = "Keep on Top", group = "client"}),
      awful.key({modkey}, "n", function(c)
        -- The client currently has the input focus, so it cannot be
        -- minimized, since minimized clients can't have the focus.
        c.minimized = true
      end, {description = "Minimize", group = "client"}),
      awful.key({modkey}, "m", function(c)
        c.maximized = not c.maximized
        c:raise()
      end, {description = "(un)maximize", group = "client"}),
      awful.key({modkey, "Control"}, "m", function(c)
        c.maximized_vertical = not c.maximized_vertical
        c:raise()
      end, {description = "(un)maximize vertically", group = "client"}),
      awful.key({modkey, "Shift"}, "m", function(c)
        c.maximized_horizontal = not c.maximized_horizontal
        c:raise()
      end, {description = "(un)maximize horizontally", group = "client"}))

  return clientkeys
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, {
  __call = function(_, ...)
    return _M.get(...)
  end
})
