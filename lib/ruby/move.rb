require 'chunky_png'
require_relative 'dimensions'
require_relative 'average_color'
require_relative 'ascii_board'
require_relative 'image'
require_relative 'png_reader'

ADB = "/home/kajboj/code/adt/adt-bundle-linux-x86-20131030/sdk/platform-tools/adb"
REGEX = 's/\x0D\x0A/\x0A/g'
CROP = {top: 18, bottom: -101}
TAP_CORRECTION = {row: -0.05}

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

def uncrop(col, row)
  [col, row+CROP[:top]]
end

def unshrink(col, row)
  [col*4, row*4]
end

def correct_tap(col, row)
  [col, row+TAP_CORRECTION[:row]]
end

runner(ENV['LOOP']) do
  puts 'getting image from android'
  # `#{ADB} shell screencap -p | perl -pe '#{REGEX}' > png/screen.png`
  image = Image.from_android('screen.png')

  puts 'processing image'
  image = Image.new('screen.png').resize(25)
  ascii_board = AsciiBoard.new('lib/scheme/boards.scm')

  cropped = image.crop(CROP[:top], image.height+CROP[:bottom])
  ascii_dimensions = ascii_board.dimensions
  pixel_dimensions = cropped.char_dimensions_in_pixels(ascii_dimensions)
  cropped.pixelize(ascii_dimensions, pixel_dimensions, 'lib/scheme/screen.scm')

  puts 'move calculation'
  output = `scheme --silent < lib/scheme/hoplite.scm`
  puts output
  col, row = unshrink(
    *uncrop(
      *cropped.ratio_to_pixels(
        *correct_tap(
          *parse_move(output)))))
  puts col, row

  puts 'tapping'
  # `#{adb} shell input tap #{col} #{row}`
  sleep 0.7
end

# image = Image.from_android
# cropped = image.crop(x, y, width, height)
# pixelized = image.pixelize
# pixelized.to_scheme_rgb
# tap_point = Ai.new.next_move
# tap_coords = cropped.to_pixels(tap_point)
# tap_coords = tap_coords.add(0, y)
# Android.tap(tap_coords)