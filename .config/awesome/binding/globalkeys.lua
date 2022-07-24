-- Standard awesome library
local gears = require('gears')
local awful = require('awful')
-- local hotkeys_popup = require("awful.hotkeys_popup").widget
local hotkeys_popup = require('awful.hotkeys_popup')
-- Menubar library
local menubar = require('menubar')
local xrandr = require('xrandr')

-- Resource Configuration
local modkey = RC.vars.modkey
local terminal = RC.vars.terminal

local _M = {}

-- reading
-- https://awesomewm.org/wiki/Global_Keybindings

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get()
  local globalkeys = gears.table.join(
    awful.key(
      { modkey },
      '/',
      hotkeys_popup.show_help,
      { description = 'Show Help', group = 'awesome' }
    ),
    -- awful.key({modkey}, 'p', function()
    --   xrandr.xrandr()
    -- end, {description = 'xrandr', group = 'tag'}),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Tag browsing
    -- awful.key({modkey}, "Left", awful.tag.viewprev,
    --     {description = "view previous", group = "tag"}),
    -- awful.key({modkey}, "Right", awful.tag.viewnext,
    --     {description = "view next", group = "tag"}),
    awful.key(
      { modkey },
      'Escape',
      awful.tag.history.restore,
      { description = 'Last Tag', group = 'tag' }
    ),

    awful.key({ modkey }, 'j', function()
      awful.client.focus.byidx(1)
    end, { description = 'Focus Next', group = 'client' }),
    awful.key({ modkey }, 'k', function()
      awful.client.focus.byidx(-1)
    end, { description = 'Focus Previous', group = 'client' }),
    -- awful.key({modkey}, "w", function()
    --   RC.mainmenu:show()
    -- end, {description = "show main menu", group = "awesome"}),
    awful.key({ modkey }, 'w', function()
      awful.layout.inc(1)
    end, { description = 'Cycle Trhough Layouts', group = 'client' }),
    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- brightness keys
    awful.key({}, 'XF86MonBrightnessUp', function()
      awful.spawn.with_shell('myBacklight -inc')
    end, { description = 'Icrease brightness', group = 'screen' }),

    awful.key({}, 'XF86MonBrightnessDown', function()
      awful.spawn.with_shell('myBacklight -dec')
    end, { description = 'Decrease Brightness', group = 'screen' }),
    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Layout manipulation
    awful.key({ modkey, 'Shift' }, 'j', function()
      awful.client.swap.byidx(1)
    end, { description = 'Swap With Next Client', group = 'client' }),
    awful.key({ modkey, 'Shift' }, 'k', function()
      awful.client.swap.byidx(-1)
    end, { description = 'Swap With Previous Client', group = 'client' }),
    -- awful.key({modkey, "Control"}, "j", function()
    --   awful.screen.focus_relative(1)
    -- end, {description = "focus the next screen", group = "screen"}),
    -- awful.key({modkey, "Control"}, "k", function()
    --   awful.screen.focus_relative(-1)
    -- end, {description = "focus the previous screen", group = "screen"}),
    awful.key(
      { modkey },
      'u',
      awful.client.urgent.jumpto,
      { description = 'Go To Urgent', group = 'client' }
    ),
    awful.key({ modkey }, 'Tab', function()
      awful.client.focus.history.previous()
      if client.focus then
        client.focus:raise()
      end
    end, { description = 'Focus Last', group = 'client' }),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Standard program
    awful.key({ modkey }, 'Return', function()
      awful.spawn(terminal)
    end, { description = 'Terminal', group = 'launcher' }),
    awful.key({ modkey }, 'c', function()
      awful.spawn('quickconfig')
    end, { description = 'Configs', group = 'launcher' }),
    awful.key({ modkey }, 'F1', function()
      awful.spawn('pcmanfm')
    end, { description = 'File Manager', group = 'launcher' }),
    awful.key({ modkey }, 'F2', function()
      awful.spawn('brave')
    end, { description = 'Brave Browser', group = 'launcher' }),
    awful.key(
      { modkey, 'Control' },
      'r',
      awesome.restart,
      { description = 'Reload Awesome', group = 'awesome' }
    ),
    awful.key(
      { modkey, 'Shift' },
      'q',
      awesome.quit,
      { description = 'Quit Awesome', group = 'awesome' }
    ),
    awful.key({ modkey }, '0', function()
      awful.spawn('powerdmenu')
    end, { description = 'Power Menu', group = 'awesome' }),
    awful.key({ modkey }, '=', function()
      awful.screen.focused().systray.visible =
        not awful.screen.focused().systray.visible
    end, { description = 'Toggle systray visibility', group = 'layout' }),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Layout manipulation
    awful.key({ modkey }, 'l', function()
      awful.tag.incmwfact(0.05)
    end, { description = 'Increase Master Size', group = 'layout' }),
    awful.key({ modkey }, 'h', function()
      awful.tag.incmwfact(-0.05)
    end, { description = 'Decrease Master Size', group = 'layout' }),
    awful.key({ modkey, 'Shift' }, 'h', function()
      awful.tag.incnmaster(1, nil, true)
    end, { description = 'Increase Number Of Masters', group = 'layout' }),
    awful.key({ modkey, 'Shift' }, 'l', function()
      awful.tag.incnmaster(-1, nil, true)
    end, { description = 'Decrease Number Of Masters', group = 'layout' }),
    awful.key({ modkey, 'Control' }, 'h', function()
      awful.tag.incncol(1, nil, true)
    end, { description = 'Increas Number Of Columns', group = 'layout' }),
    awful.key({ modkey, 'Control' }, 'l', function()
      awful.tag.incncol(-1, nil, true)
    end, { description = 'Decrase Number Of Columns', group = 'layout' }),

    awful.key({ modkey, 'Control' }, 'n', function()
      local c = awful.client.restore()
      -- Focus restored client
      if c then
        c:emit_signal('request::activate', 'key.unminimize', { raise = true })
      end
    end, { description = 'Restore Minimized', group = 'client' }),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Prompt
    awful.key({ modkey }, 'd', function()
      awful.spawn('rofi -show run')
    end, { description = 'dmenu', group = 'launcher' }), -- App launcher
    awful.key({ modkey }, 'space', function()
      awful.spawn('rofi -modi "drun" -show drun')
    end, { description = 'App Launcher', group = 'launcher' }),

    --   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    -- Resize (Floating)
    -- awful.key({ modkey, "Control" }, "Left",  function () awful.client.moveresize( 20,  20, -40, -40) end),
    -- awful.key({ modkey, "Control" }, "Right", function () awful.client.moveresize(-20, -20,  40,  40) end),
    awful.key({ modkey, 'Control' }, 'Down', function()
      awful.client.moveresize(0, 0, 0, -50)
    end),
    awful.key({ modkey, 'Control' }, 'Up', function()
      awful.client.moveresize(0, 0, 0, 50)
    end),
    awful.key({ modkey, 'Control' }, 'Left', function()
      awful.client.moveresize(0, 0, -50, 0)
    end),
    awful.key({ modkey, 'Control' }, 'Right', function()
      awful.client.moveresize(0, 0, 50, 0)
    end)
  )

  return globalkeys
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, {
  __call = function(_, ...)
    return _M.get(...)
  end,
})
