#!/bin/sh

# The focused-window title label is set by the enabled WM aspect's `wm` Lua
# module (it subscribes front_app + the secondary title items to its own
# title-changed event and sets their labels from a WM query). This plugin is
# intentionally a no-op so the front_app item's script path stays valid on
# hosts with no WM provider.
exit 0
