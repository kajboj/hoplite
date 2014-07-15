adb = "/home/kajboj/code/adt/adt-bundle-linux-x86-20131030/sdk/platform-tools/adb"
regex = 's/\x0D\x0A/\x0A/g'

def runner(repeat)
  if repeat
    while true do
      yield
    end
  else
    yield
  end
end

runner(ENV['LOOP']) do
  puts 'getting image from android'
  `#{adb} shell screencap -p | perl -pe '#{regex}' > png/screen.png`
  puts 'processing image'
  load 'lib/ruby/png_reader.rb'
  puts 'move calculation'
  output = `scheme --silent < lib/scheme/hoplite.scm`
  puts output

  col, row = ratio_to_pixels(*parse_move(output))
  puts col, row

  puts 'tapping'
  `#{adb} shell input tap #{col} #{row}`
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