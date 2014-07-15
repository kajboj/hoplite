require 'chunky_png'
require_relative 'dimensions'
require_relative 'average_color'
require_relative 'ascii_board'
require_relative 'image'

CROP = {top: 18, bottom: -101}
TAP_CORRECTION = {row: -0.05}

def char_dimensions_in_pixels(image, ascii_dimensions)
  Dimensions.new(
    image.width.to_f / ascii_dimensions.width,
    image.height.to_f / ascii_dimensions.height
  )
end

def put_dot_on_image(col, row, input_filename, output_filename)
  image = ChunkyPNG::Image.from_file(png_filename(input_filename))
  [[0, 1], [1, 1], [1, 0], [0, 0]].each do |(a, b)|
    image[col+a, row+b] = ChunkyPNG::Color.rgba(255, 0, 0, 255)
  end
  image.save(png_filename(output_filename))
end

def parse_move(output)
  col_s, row_s = output.split("\n").last[1..-2].split
  [col_s, row_s].map do |ratio_s|
    a = ratio_s.split("/").compact.map(&:to_f)
    a[1] ? a[0]/a[1] : a[0]
  end
end

def png_to_rgb_list
  image = Image.new('screen.png').resize(25)
  ascii_board = AsciiBoard.new('lib/scheme/boards.scm')

  cropped = image.crop(CROP[:top], image.height+CROP[:bottom])
  ascii_dimensions = ascii_board.dimensions
  pixel_dimensions = char_dimensions_in_pixels(cropped, ascii_dimensions)
  cropped.pixelize(ascii_dimensions, pixel_dimensions, 'lib/scheme/screen.scm')
end

png_to_rgb_list