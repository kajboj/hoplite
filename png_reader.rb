require 'chunky_png'

PNG_DIR = 'png'

def load_ascii_board
  boards = File.read('boards.scm')
  empty_board = boards.scan(/define empty-board "(.*?)"/m).first.first
  empty_board.gsub("\\\\", "\\")[1..-1]
end

class Dimensions
  attr_reader :width, :height
  def initialize(width, height)
    @width, @height = width, height
  end
  def to_s
    "(#{width}x#{height})"
  end
end

class AverageColor
  def initialize
    @r, @g, @b = 0, 0, 0
    @count = 0
  end
  def sample(i)
    @r += ChunkyPNG::Color.r(i)
    @g += ChunkyPNG::Color.g(i)
    @b += ChunkyPNG::Color.b(i)
    @count += 1
  end
  def r; @r / @count; end
  def g; @g / @count; end
  def b; @b / @count; end
end

def dimensions(ascii_board)
  lines = ascii_board.split("\n")
  height = lines.size
  width = lines.max_by(&:length).size
  Dimensions.new(width, height)
end

def char_dimensions_in_pixels(image, ascii_dimensions)
  Dimensions.new(
    image.width.to_f / ascii_dimensions.width,
    image.height.to_f / ascii_dimensions.height
  )
end

def crop(image, minrow, maxrow)
  png = ChunkyPNG::Image.new(image.width, maxrow-minrow, ChunkyPNG::Color.rgba(0, 255, 0, 255))

  (0..image.width-1).each do |col|
    (minrow..maxrow-1).each do |row|
      png[col, row - minrow] = image[col, row]
    end
  end

  png.save(File.join(PNG_DIR, 'cropped.png'))
  png
end

def pixelize(image, ascii_board_dimensions, pixel_dimensions)
  png = ChunkyPNG::Image.new(image.width, image.height, ChunkyPNG::Color.rgba(0, 255, 0, 255))
  scm = File.open('screen.scm', 'w') do |f|
    f.write("(define screen '(\n")

    (0..ascii_board_dimensions.height-1).each do |ascii_row|
      f.write("(")
      (0..ascii_board_dimensions.width-1).each do |ascii_col|
        avg_color = nil
        each_pixel_of_ascii_tile(image, ascii_col, ascii_row, pixel_dimensions) do |col, row, avg|
          avg_color = avg
          png[col, row] = ChunkyPNG::Color.rgba(avg.r, avg.g, avg.b, 255)
        end
        f.write("(#{avg_color.r} #{avg_color.g} #{avg_color.b}) ")
      end
      f.write(")\n")
    end

    f.write("))")
  end

  png.save(File.join(PNG_DIR, 'pixelated.png'))
end

def each_pixel_of_ascii_tile(image, ascii_col, ascii_row, pixel_dimensions)
  mincol = (ascii_col*pixel_dimensions.width).floor
  minrow = (ascii_row*pixel_dimensions.height).floor
  maxcol = (mincol + pixel_dimensions.width).floor
  maxrow = (minrow + pixel_dimensions.height).floor

  avg = avg_rect(image, mincol, maxcol, minrow, maxrow)

  (minrow..maxrow).each do |row|
    (mincol..maxcol).each do |col|
      yield col, row, avg
    end
  end
end

def avg_rect(image, mincol, maxcol, minrow, maxrow)
  avg = AverageColor.new
  (minrow..maxrow).each do |row|
    (mincol..maxcol).each do |col|
      avg.sample(image[col, row])
    end
  end
  avg
end

input_filename = File.join(PNG_DIR, 'screen.png')

`convert -resize 25% #{input_filename} screen-small.png`

image = ChunkyPNG::Image.from_file('screen-small.png')

board = load_ascii_board

cropped = crop(image, 18, image.height-101)
ascii_dimensions = dimensions(board)
pixel_dimensions = char_dimensions_in_pixels(cropped, ascii_dimensions)
pixelize(cropped, ascii_dimensions, pixel_dimensions  )