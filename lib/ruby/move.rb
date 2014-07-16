require 'chunky_png'
require_relative 'dimensions'
require_relative 'average_color'
require_relative 'ascii_board'
require_relative 'image'
require_relative 'screen'

def runner(repeat)
  if repeat
    while true do
      yield
    end
  else
    yield
  end
end

def parse_move(output)
  col_s, row_s = output.split("\n").last[1..-2].split
  [col_s, row_s].map do |ratio_s|
    a = ratio_s.split("/").compact.map(&:to_f)
    a[1] ? a[0]/a[1] : a[0]
  end
end

runner(ENV['LOOP']) do
  screen = Screen.from_android
  screen.to_scheme_rgb

  output = `scheme --silent < lib/scheme/hoplite.scm`
  puts output

  screen.tap_ratio(*parse_move(output))
  screen.to_scheme_special_skills

  sleep 0.7
end