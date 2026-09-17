local tym = require('tym')

tym.set_config {
  font = 'Misc Fixed 10',
  cursor_blink_mode = 'off',
  scrollback_length = 100000,
  scrollback_on_output = false,
}

tym.set_hook('clicked', function(button, uri)
  -- print('you pressed button:', button) -- 1:left, 2:middle, 3:right

  -- open URI only by middle click
  if button == 2 then
    if uri then
      -- print('you clicked URI: ', uri)
      tym.open(uri)
      -- disable the default action 'put clipboard' when open URI
      return true
    end
  end

  -- copy URI on right click
  if button == 3 then
    tym.copy(uri)
  end
end)
