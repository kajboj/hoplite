require 'chunky_png'
require_relative 'dimensions'
require_relative 'average_color'
require_relative 'ascii_board'
require_relative 'image'
require_relative 'screen'
require_relative 'hoplite_move'
require_relative 'by_one'
require_relative 'leap'
require_relative 'parser'

DEBUG = ENV['DEBUG']

def runner(repeat)
  if repeat
    while true do
      begin
        yield
      rescue HopliteParseError
        yield
      end
    end
  else
    yield
  end
end

HopliteParseError = Class.new(Exception)

runner(ENV['LOOP']) do
  screen = if (ENV['DEVICE'] == 'false')
    screen = Screen.from_file('screen.png')
  else
    Screen.from_android
  end

  screen.to_scheme_rgb
  screen.to_scheme_special_skills

  output = `scheme --silent < lib/scheme/hoplite.scm`
  puts output

  move = Parser.parse_move(output)
  move.execute(screen)

  sleep 0.7
end